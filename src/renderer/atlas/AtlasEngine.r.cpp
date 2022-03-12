// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

#include "pch.h"
#include "AtlasEngine.h"

#include <fstream>

#include "dwrite.h"

// #### NOTE ####
// If you see any code in here that contains "_api." you might be seeing a race condition.
// The AtlasEngine::Present() method is called on a background thread without any locks,
// while any of the API methods (like AtlasEngine::Invalidate) might be called concurrently.
// The usage of the _r field is safe as its members are in practice
// only ever written to by the caller of Present() (the "Renderer" class).
// The _api fields on the other hand are concurrently written to by others.

#pragma warning(disable : 4100) // '...': unreferenced formal parameter
// Disable a bunch of warnings which get in the way of writing performant code.
#pragma warning(disable : 26429) // Symbol 'data' is never tested for nullness, it can be marked as not_null (f.23).
#pragma warning(disable : 26446) // Prefer to use gsl::at() instead of unchecked subscript operator (bounds.4).
#pragma warning(disable : 26481) // Don't use pointer arithmetic. Use span instead (bounds.1).
#pragma warning(disable : 26482) // Only index into arrays using constant expressions (bounds.2).

using namespace Microsoft::Console::Render;

#pragma region IRenderEngine

// Present() is called without the console buffer lock being held.
// --> Put as much in here as possible.
[[nodiscard]] HRESULT AtlasEngine::Present() noexcept
try
{
    _adjustAtlasSize();
    _reserveScratchpadSize(_r.maxEncounteredCellCount);
    _processGlyphQueue();

    if (WI_IsFlagSet(_r.invalidations, RenderInvalidations::Cursor))
    {
        _drawCursor();
        WI_ClearFlag(_r.invalidations, RenderInvalidations::Cursor);
    }

    // The values the constant buffer depends on are potentially updated after BeginPaint().
    if (WI_IsFlagSet(_r.invalidations, RenderInvalidations::ConstBuffer))
    {
        _updateConstantBuffer();
        WI_ClearFlag(_r.invalidations, RenderInvalidations::ConstBuffer);
    }

    {
        D3D12_RANGE readRange{};
        void* data;
        THROW_IF_FAILED(_r.cellBuffer->Map(0, &readRange, &data));
        memcpy(data, _r.cells.data(), _r.cells.size() * sizeof(Cell));
        _r.cellBuffer->Unmap(0, nullptr);
    }

    {
        _r.deviceContext->VSSetShader(_r.vertexShader.get(), nullptr, 0);
        _r.deviceContext->PSSetShader(_r.pixelShader.get(), nullptr, 0);

        // Our vertex shader uses a trick from Bill Bilodeau published in
        // "Vertex Shader Tricks" at GDC14 to draw a fullscreen triangle
        // without vertex/index buffers. This prepares our context for this.
        _r.commandList->IASetVertexBuffers(0, 0, nullptr, nullptr, nullptr);
        _r.commandList->IASetIndexBuffer(nullptr);
        _r.commandList->IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);

        _r.deviceContext->PSSetConstantBuffers(0, 1, _r.constantBuffer.addressof());

        const std::array resources{ _r.cellView.get(), _r.atlasView.get() };
        _r.deviceContext->PSSetShaderResources(0, gsl::narrow_cast<UINT>(resources.size()), resources.data());

        // After Present calls, the back buffer needs to explicitly be
        // re-bound to the D3D11 immediate context before it can be used again.
        _r.deviceContext->OMSetRenderTargets(1, _r.renderTargetView.addressof(), nullptr);
        _r.deviceContext->Draw(3, 0);

        // Command list allocators can only be reset when the associated
        // command lists have finished execution on the GPU; apps should use
        // fences to determine GPU execution progress.
        THROW_IF_FAILED(_r.commandAllocator->Reset());

        // However, when ExecuteCommandList() is called on a particular command
        // list, that command list can then be reset at any time and must be before
        // re-recording.
        THROW_IF_FAILED(_r.commandList->Reset(_r.commandAllocator.get(), _r.pipelineState.get()));

        // Set necessary state.

        // Indicate that the back buffer will be used as a render target.
        D3D12_RESOURCE_BARRIER barrier;
        barrier.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
        barrier.Flags = D3D12_RESOURCE_BARRIER_FLAG_NONE;
        barrier.Transition.pResource = _r.renderTargetView[_r.frameIndex].get();
        barrier.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
        barrier.Transition.StateBefore = D3D12_RESOURCE_STATE_PRESENT;
        barrier.Transition.StateAfter = D3D12_RESOURCE_STATE_RENDER_TARGET;
        _r.commandList->ResourceBarrier(1, &barrier);

        // Record commands.
        _r.commandList->SetGraphicsRootSignature(_r.rootSignature.get());

        _r.commandList->IASetVertexBuffers(0, 0, nullptr);
        _r.commandList->IASetIndexBuffer(nullptr);
        _r.commandList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);

        D3D12_VIEWPORT viewport{};
        viewport.Width = static_cast<float>(_api.sizeInPixel.x); // TODO
        viewport.Height = static_cast<float>(_api.sizeInPixel.y);
        _r.commandList->RSSetViewports(1, &viewport);

        auto handle = _r.renderTargetViewHeap->GetCPUDescriptorHandleForHeapStart();
        handle.ptr += _r.frameIndex * _r.device->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_RTV);
        _r.commandList->OMSetRenderTargets(1, &handle, FALSE, nullptr);

        _r.commandList->DrawInstanced(3, 1, 0, 0);

        // Indicate that the back buffer will now be used to present.
        barrier.Transition.StateBefore = D3D12_RESOURCE_STATE_RENDER_TARGET;
        barrier.Transition.StateAfter = D3D12_RESOURCE_STATE_PRESENT;
        _r.commandList->ResourceBarrier(1, &barrier);

        THROW_IF_FAILED(_r.commandList->Close());
    }

    // See documentation for IDXGISwapChain2::GetFrameLatencyWaitableObject method:
    // > For every frame it renders, the app should wait on this handle before starting any rendering operations.
    // > Note that this requirement includes the first frame the app renders with the swap chain.
    assert(_r.frameLatencyWaitableObjectUsed);

    // > IDXGISwapChain::Present: Partial Presentation (using a dirty rects or scroll) is not supported
    // > for SwapChains created with DXGI_SWAP_EFFECT_DISCARD or DXGI_SWAP_EFFECT_FLIP_DISCARD.
    // ---> No need to call IDXGISwapChain1::Present1.
    //      TODO: Would IDXGISwapChain1::Present1 and its dirty rects have benefits for remote desktop?
    THROW_IF_FAILED(_r.swapChain->Present(1, 0));

    return S_OK;
}
catch (const wil::ResultException& exception)
{
    return _handleException(exception);
}
CATCH_RETURN()

#pragma endregion

void AtlasEngine::_setShaderResources() const
{
}

void AtlasEngine::_updateConstantBuffer() const noexcept
{
    const auto useClearType = _api.realizedAntialiasingMode == D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE;

    ConstBuffer buffer;
    buffer.viewport.x = 0;
    buffer.viewport.y = 0;
    buffer.viewport.z = static_cast<float>(_r.cellCount.x * _r.cellSize.x);
    buffer.viewport.w = static_cast<float>(_r.cellCount.y * _r.cellSize.y);
    DWrite_GetGammaRatios(_r.gamma, buffer.gammaRatios);
    buffer.enhancedContrast = useClearType ? _r.cleartypeEnhancedContrast : _r.grayscaleEnhancedContrast;
    buffer.cellCountX = _r.cellCount.x;
    buffer.cellSize.x = _r.cellSize.x;
    buffer.cellSize.y = _r.cellSize.y;
    buffer.underlinePos.x = _r.underlinePos;
    buffer.underlinePos.y = _r.underlinePos + _r.lineThickness;
    buffer.strikethroughPos.x = _r.strikethroughPos;
    buffer.strikethroughPos.y = _r.strikethroughPos + _r.lineThickness;
    buffer.backgroundColor = _r.backgroundColor;
    buffer.cursorColor = _r.cursorOptions.cursorColor;
    buffer.selectionColor = _r.selectionColor;
    buffer.useClearType = useClearType;

    D3D12_RANGE readRange{};
    void* data;
    THROW_IF_FAILED(_r.constantBuffer->Map(0, &readRange, &data));
    memcpy(data, &buffer, sizeof(buffer));
    _r.constantBuffer->Unmap(0, nullptr);
}

void AtlasEngine::_adjustAtlasSize()
{
    if (_r.atlasPosition.y < _r.atlasSizeInPixel.y && _r.atlasPosition.x < _r.atlasSizeInPixel.x)
    {
        return;
    }

    const u32 limitX = _r.atlasSizeInPixelLimit.x;
    const u32 limitY = _r.atlasSizeInPixelLimit.y;
    const u32 posX = _r.atlasPosition.x;
    const u32 posY = _r.atlasPosition.y;
    const u32 cellX = _r.cellSize.x;
    const u32 cellY = _r.cellSize.y;
    const auto perCellArea = cellX * cellY;

    // The texture atlas is filled like this:
    //   x →
    // y +--------------+
    // ↓ |XXXXXXXXXXXXXX|
    //   |XXXXXXXXXXXXXX|
    //   |XXXXX↖        |
    //   |      |       |
    //   +------|-------+
    // This is where _r.atlasPosition points at.
    //
    // Each X is a glyph texture tile that's occupied.
    // We can compute the area of pixels consumed by adding the first
    // two lines of X (rectangular) together with the last line of X.
    const auto currentArea = posY * limitX + posX * cellY;
    // minArea reserves enough room for 64 cells in all cases (mainly during startup).
    const auto minArea = 64 * perCellArea;
    auto newArea = std::max(minArea, currentArea);

    // I want the texture to grow exponentially similar to std::vector, as this
    // ensures we don't need to resize the texture again right after having done.
    // This rounds newArea up to the next power of 2.
    unsigned long int index;
    _BitScanReverse(&index, newArea); // newArea can't be 0
    newArea = u32{ 1 } << (index + 1);

    const auto pixelPerRow = limitX * cellY;
    // newArea might be just large enough that it spans N full rows of cells and one additional row
    // just barely. This algorithm rounds up newArea to the _next_ multiple of cellY.
    const auto wantedHeight = (newArea + pixelPerRow - 1) / pixelPerRow * cellY;
    // The atlas might either be a N rows of full width (xLimit) or just one
    // row (where wantedHeight == cellY) that doesn't quite fill it's maximum width yet.
    const auto wantedWidth = wantedHeight != cellY ? limitX : newArea / perCellArea * cellX;

    // We know that limitX/limitY were u16 originally, and thus it's safe to narrow_cast it back.
    const auto height = gsl::narrow_cast<u16>(std::min(limitY, wantedHeight));
    const auto width = gsl::narrow_cast<u16>(std::min(limitX, wantedWidth));

    assert(width != 0);
    assert(height != 0);

    wil::com_ptr<ID3D11Texture2D> atlasBuffer;
    wil::com_ptr<ID3D11ShaderResourceView> atlasView;
    {
        D3D11_TEXTURE2D_DESC desc{};
        desc.Width = width;
        desc.Height = height;
        desc.MipLevels = 1;
        desc.ArraySize = 1;
        desc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
        desc.SampleDesc = { 1, 0 };
        desc.Usage = D3D11_USAGE_DEFAULT;
        desc.BindFlags = D3D11_BIND_SHADER_RESOURCE;
        desc.CPUAccessFlags = D3D11_CPU_ACCESS_WRITE;
        THROW_IF_FAILED(_r.device->CreateTexture2D(&desc, nullptr, atlasBuffer.addressof()));
        THROW_IF_FAILED(_r.device->CreateShaderResourceView(atlasBuffer.get(), nullptr, atlasView.addressof()));
    }

    // If a _r.atlasBuffer already existed, we can copy its glyphs
    // over to the new texture without re-rendering everything.
    const auto copyFromExisting = _r.atlasSizeInPixel != u16x2{};
    if (copyFromExisting)
    {
        D3D11_BOX box;
        box.left = 0;
        box.top = 0;
        box.front = 0;
        box.right = _r.atlasSizeInPixel.x;
        box.bottom = _r.atlasSizeInPixel.y;
        box.back = 1;
        _r.deviceContext->CopySubresourceRegion1(atlasBuffer.get(), 0, 0, 0, 0, _r.atlasBuffer.get(), 0, &box, D3D11_COPY_NO_OVERWRITE);
    }

    _r.atlasSizeInPixel = u16x2{ width, height };
    _r.atlasBuffer = std::move(atlasBuffer);
    _r.atlasView = std::move(atlasView);
    _setShaderResources();

    WI_SetFlagIf(_r.invalidations, RenderInvalidations::Cursor, !copyFromExisting);
}

void AtlasEngine::_reserveScratchpadSize(u16 minWidth)
{
    if (minWidth <= _r.scratchpadCellWidth)
    {
        return;
    }

    // The new size is the greater of ... cells wide:
    // * 2
    // * minWidth
    // * current size * 1.5
    const auto newWidth = std::max<UINT>(std::max<UINT>(2, minWidth), _r.scratchpadCellWidth + (_r.scratchpadCellWidth >> 1));

    _r.d2dRenderTarget.reset();
    _r.atlasScratchpad.reset();

#if ATLAS_D2D_SOFTWARE_RENDERING
    {
        D2D1_RENDER_TARGET_PROPERTIES props{};
        props.type = D2D1_RENDER_TARGET_TYPE_DEFAULT;
        props.pixelFormat = { DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED };
        props.dpiX = static_cast<float>(_r.dpi);
        props.dpiY = static_cast<float>(_r.dpi);
        THROW_IF_FAILED(_sr.wicFactory->CreateBitmap(_r.cellSize.x * newWidth, _r.cellSize.y, GUID_WICPixelFormat32bppPBGRA, WICBitmapCacheOnLoad, _r.atlasScratchpad.put()));
        THROW_IF_FAILED(_sr.d2dFactory->CreateWicBitmapRenderTarget(_r.atlasScratchpad.get(), &props, _r.d2dRenderTarget.put()));
    }
#else
    {
        const auto surface = _r.atlasScratchpad.query<IDXGISurface>();

        D3D11_TEXTURE2D_DESC desc{};
        desc.Width = _r.cellSize.x * newWidth;
        desc.Height = _r.cellSize.y;
        desc.MipLevels = 1;
        desc.ArraySize = 1;
        desc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
        desc.SampleDesc = { 1, 0 };
        desc.BindFlags = D3D11_BIND_SHADER_RESOURCE | D3D11_BIND_RENDER_TARGET;
        THROW_IF_FAILED(_r.device->CreateTexture2D(&desc, nullptr, _r.atlasScratchpad.put()));

        D2D1_RENDER_TARGET_PROPERTIES props{};
        props.type = D2D1_RENDER_TARGET_TYPE_DEFAULT;
        props.pixelFormat = { DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED };
        props.dpiX = static_cast<float>(_r.dpi);
        props.dpiY = static_cast<float>(_r.dpi);
        THROW_IF_FAILED(_sr.d2dFactory->CreateDxgiSurfaceRenderTarget(surface.get(), &props, _r.d2dRenderTarget.put()));
    }
#endif
    {
        wil::com_ptr<IDWriteRenderingParams1> renderingParams;
        DWrite_GetRenderParams(_sr.dwriteFactory.get(), &_r.gamma, &_r.cleartypeEnhancedContrast, &_r.grayscaleEnhancedContrast, renderingParams.addressof());

        // We don't really use D2D for anything except DWrite, but it
        // can't hurt to ensure that everything it does is pixel aligned.
        _r.d2dRenderTarget->SetAntialiasMode(D2D1_ANTIALIAS_MODE_ALIASED);
        // In case _api.realizedAntialiasingMode is D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE we'll
        // continuously adjust it in AtlasEngine::_drawGlyph. See _drawGlyph.
        _r.d2dRenderTarget->SetTextAntialiasMode(static_cast<D2D1_TEXT_ANTIALIAS_MODE>(_api.realizedAntialiasingMode));
        // Ensure that D2D uses the exact same gamma as our shader uses.
        _r.d2dRenderTarget->SetTextRenderingParams(renderingParams.get());
    }
    {
        static constexpr D2D1_COLOR_F color{ 1, 1, 1, 1 };
        wil::com_ptr<ID2D1SolidColorBrush> brush;
        THROW_IF_FAILED(_r.d2dRenderTarget->CreateSolidColorBrush(&color, nullptr, brush.addressof()));
        _r.brush = brush.query<ID2D1Brush>();
    }

    _r.scratchpadCellWidth = _r.maxEncounteredCellCount;
    WI_SetAllFlags(_r.invalidations, RenderInvalidations::ConstBuffer);
}

void AtlasEngine::_processGlyphQueue()
{
    if (_r.glyphQueue.empty())
    {
        return;
    }

    for (const auto& pair : _r.glyphQueue)
    {
        _drawGlyph(pair);
    }

    _r.glyphQueue.clear();
}

static std::ofstream file{ "R:/out.txt" };

void AtlasEngine::_drawGlyph(const AtlasQueueItem& item) const
{
    const auto beg = std::chrono::high_resolution_clock::now();
    const auto defer = wil::scope_exit([&]() {
        const auto end = std::chrono::high_resolution_clock::now();
        const auto out = std::to_string(std::chrono::duration_cast<std::chrono::nanoseconds>(end - beg).count()) + '\n';
        file << out;
        file.flush();
    });

    const auto key = item.key->data();
    const auto value = item.value->data();
    const auto coords = &value->coords[0];
    const auto charsLength = key->charCount;
    const auto cells = static_cast<u32>(key->attributes.cellCount);
    const auto textFormat = _getTextFormat(key->attributes.bold, key->attributes.italic);
    const auto coloredGlyph = WI_IsFlagSet(value->flags, CellFlags::ColoredGlyph);

    // See D2DFactory::DrawText
    wil::com_ptr<IDWriteTextLayout> textLayout;
    THROW_IF_FAILED(_sr.dwriteFactory->CreateTextLayout(&key->chars[0], charsLength, textFormat, cells * _r.cellSizeDIP.x, _r.cellSizeDIP.y, textLayout.addressof()));
    if (_r.typography)
    {
        textLayout->SetTypography(_r.typography.get(), { 0, charsLength });
    }

    auto options = D2D1_DRAW_TEXT_OPTIONS_NONE;
    // D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT enables a bunch of internal machinery
    // which doesn't have to run if we know we can't use it anyways in the shader.
    WI_SetFlagIf(options, D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT, coloredGlyph);

    // Colored glyphs cannot be drawn in linear gamma.
    // That's why we're simply alpha-blending them in the shader.
    // In order for this to work correctly we have to prevent them from being drawn
    // with ClearType, because we would then lack the alpha channel for the glyphs.
    if (_api.realizedAntialiasingMode == D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE)
    {
        _r.d2dRenderTarget->SetTextAntialiasMode(coloredGlyph ? D2D1_TEXT_ANTIALIAS_MODE_GRAYSCALE : D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE);
    }

    _r.d2dRenderTarget->BeginDraw();
    // We could call
    //   _r.d2dRenderTarget->PushAxisAlignedClip(&rect, D2D1_ANTIALIAS_MODE_ALIASED);
    // now to reduce the surface that needs to be cleared, but this decreases
    // performance by 10% (tested using debugGlyphGenerationPerformance).
    _r.d2dRenderTarget->Clear();
    _r.d2dRenderTarget->DrawTextLayout({}, textLayout.get(), _r.brush.get(), options);
    THROW_IF_FAILED(_r.d2dRenderTarget->EndDraw());

    for (uint32_t i = 0; i < cells; ++i)
    {
        // Specifying NO_OVERWRITE means that the system can assume that existing references to the surface that
        // may be in flight on the GPU will not be affected by the update, so the copy can proceed immediately
        // (avoiding either a batch flush or the system maintaining multiple copies of the resource behind the scenes).
        //
        // Since our shader only draws whatever is in the atlas, and since we don't replace glyph tiles that are in use,
        // we can safely (?) tell the GPU that we don't overwrite parts of our atlas that are in use.
        _copyScratchpadTile(i, coords[i], 0);
    }
}

void AtlasEngine::_drawCursor()
{
    _reserveScratchpadSize(1);

    // lineWidth is in D2D's DIPs. For instance if we have a 150-200% zoom scale we want to draw a 2px wide line.
    // At 150% scale lineWidth thus needs to be 1.33333... because at a zoom scale of 1.5 this results in a 2px wide line.
    const auto lineWidth = std::max(1.0f, static_cast<float>((_r.dpi + USER_DEFAULT_SCREEN_DPI / 2) / USER_DEFAULT_SCREEN_DPI * USER_DEFAULT_SCREEN_DPI) / static_cast<float>(_r.dpi));
    const auto cursorType = static_cast<CursorType>(_r.cursorOptions.cursorType);
    D2D1_RECT_F rect;
    rect.left = 0.0f;
    rect.top = 0.0f;
    rect.right = _r.cellSizeDIP.x;
    rect.bottom = _r.cellSizeDIP.y;

    switch (cursorType)
    {
    case CursorType::Legacy:
        rect.top = _r.cellSizeDIP.y * static_cast<float>(100 - _r.cursorOptions.heightPercentage) / 100.0f;
        break;
    case CursorType::VerticalBar:
        rect.right = lineWidth;
        break;
    case CursorType::EmptyBox:
    {
        // EmptyBox is drawn as a line and unlike filled rectangles those are drawn centered on their
        // coordinates in such a way that the line border extends half the width to each side.
        // --> Our coordinates have to be 0.5 DIP off in order to draw a 2px line on a 200% scaling.
        const auto halfWidth = lineWidth / 2.0f;
        rect.left = halfWidth;
        rect.top = halfWidth;
        rect.right -= halfWidth;
        rect.bottom -= halfWidth;
        break;
    }
    case CursorType::Underscore:
    case CursorType::DoubleUnderscore:
        rect.top = _r.cellSizeDIP.y - lineWidth;
        break;
    default:
        break;
    }

    _r.d2dRenderTarget->BeginDraw();
    _r.d2dRenderTarget->Clear();

    if (cursorType == CursorType::EmptyBox)
    {
        _r.d2dRenderTarget->DrawRectangle(&rect, _r.brush.get(), lineWidth);
    }
    else
    {
        _r.d2dRenderTarget->FillRectangle(&rect, _r.brush.get());
    }

    if (cursorType == CursorType::DoubleUnderscore)
    {
        rect.top -= 2.0f;
        rect.bottom -= 2.0f;
        _r.d2dRenderTarget->FillRectangle(&rect, _r.brush.get());
    }

    THROW_IF_FAILED(_r.d2dRenderTarget->EndDraw());

    _copyScratchpadTile(0, {});
}

void AtlasEngine::_copyScratchpadTile(uint32_t scratchpadIndex, u16x2 target, uint32_t copyFlags) const
{
#if ATLAS_D2D_SOFTWARE_RENDERING
    WICRect rect;
    rect.X = scratchpadIndex * _r.cellSize.x;
    rect.Y = 0;
    rect.Width = _r.cellSize.x;
    rect.Height = _r.cellSize.y;

    D3D11_BOX box;
    box.left = target.x;
    box.top = target.y;
    box.front = 0;
    box.right = target.x + _r.cellSize.x;
    box.bottom = target.y + _r.cellSize.y;
    box.back = 1;

    wil::com_ptr<IWICBitmapLock> lock;
    THROW_IF_FAILED(_r.atlasScratchpad->Lock(&rect, WICBitmapLockRead, lock.addressof()));

    UINT stride;
    THROW_IF_FAILED(lock->GetStride(&stride));

    UINT size;
    WICInProcPointer src;
    THROW_IF_FAILED(lock->GetDataPointer(&size, &src));

    _r.deviceContext->UpdateSubresource1(_r.atlasBuffer.get(), 0, &box, src, stride, 0, copyFlags);

#else

    D3D11_BOX box;
    box.left = scratchpadIndex * _r.cellSize.x;
    box.top = 0;
    box.front = 0;
    box.right = box.left + _r.cellSize.x;
    box.bottom = _r.cellSize.y;
    box.back = 1;
#pragma warning(suppress : 26447) // The function is declared 'noexcept' but calls function '...' which may throw exceptions (f.6).
    _r.deviceContext->CopySubresourceRegion1(_r.atlasBuffer.get(), 0, target.x, target.y, 0, _r.atlasScratchpad.get(), 0, &box, copyFlags);
#endif
}
