// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

#include "pch.h"
#include "AtlasEngine.h"

#include "dwrite.h"

#include <shader_ps.h>
#include <shader_vs.h>

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
#pragma warning(disable : 26459) // You called an STL function '...' with a raw pointer parameter at position '...' that may be unsafe [...].
#pragma warning(disable : 26481) // Don't use pointer arithmetic. Use span instead (bounds.1).
#pragma warning(disable : 26482) // Only index into arrays using constant expressions (bounds.2).

// https://en.wikipedia.org/wiki/Inversion_list
template<size_t N>
constexpr bool isInInversionList(const std::array<wchar_t, N>& ranges, wchar_t needle)
{
    const auto beg = ranges.begin();
    const auto end = ranges.end();
    decltype(ranges.begin()) it;

    // Linear search is faster than binary search for short inputs.
    if constexpr (N < 16)
    {
        it = std::find_if(beg, end, [=](wchar_t v) { return needle < v; });
    }
    else
    {
        it = std::upper_bound(beg, end, needle);
    }

    const auto idx = it - beg;
    return (idx & 1) != 0;
}

template<typename T = D2D1_COLOR_F>
constexpr T colorFromU32(uint32_t rgba)
{
    const auto r = static_cast<float>((rgba >> 0) & 0xff) / 255.0f;
    const auto g = static_cast<float>((rgba >> 8) & 0xff) / 255.0f;
    const auto b = static_cast<float>((rgba >> 16) & 0xff) / 255.0f;
    const auto a = static_cast<float>((rgba >> 24) & 0xff) / 255.0f;
    return { r, g, b, a };
}

using namespace Microsoft::Console::Render::Atlas;

#pragma region IRenderEngine

// Present() is called without the console buffer lock being held.
// --> Put as much in here as possible.
[[nodiscard]] HRESULT AtlasEngine::Present() noexcept
try
{
    if (!_r.driver)
    {
        _recreateBackend();
    }

    _r.driver->Render(_p);
    _p.glyphQueue.clear();

    if (!_p.dxgiFactory->IsCurrent())
    {
        _p.dxgiFactory.reset();
        _r = {};
        return E_PENDING; // Indicate a retry to the renderer
    }

    return S_OK;
}
catch (const wil::ResultException& exception)
{
    return _handleException(exception);
}
CATCH_RETURN()

[[nodiscard]] bool AtlasEngine::RequiresContinuousRedraw() noexcept
{
    return false;
}

void AtlasEngine::WaitUntilCanRender() noexcept
{
    if (_r.driver)
    {
        _r.driver->WaitUntilCanRender();
    }
}

#pragma endregion

void SwapChainManager::UpdateFontSettings(const RenderingPayload& p) const
{
    if (!p.s->target->hwnd)
    {
        const DXGI_MATRIX_3X2_F matrix{
            ._11 = p.d.font.dipPerPixel,
            ._22 = p.d.font.dipPerPixel,
        };
        THROW_IF_FAILED(_swapChain->SetMatrixTransform(&matrix));
    }
}

wil::com_ptr<ID3D11Texture2D> SwapChainManager::GetBuffer() const
{
    wil::com_ptr<ID3D11Texture2D> buffer;
    THROW_IF_FAILED(_swapChain->GetBuffer(0, __uuidof(ID3D11Texture2D), buffer.put_void()));
    return buffer;
}

void SwapChainManager::Present(const RenderingPayload& p)
{
    const til::rect fullRect{ 0, 0, p.s->cellCount.x, p.s->cellCount.y };

    if (!p.dirtyRect)
    {
        return;
    }

    if (p.dirtyRect != fullRect)
    {
        auto dirtyRectInPx = p.dirtyRect;
        dirtyRectInPx.left *= p.s->font->cellSize.x;
        dirtyRectInPx.top *= p.s->font->cellSize.y;
        dirtyRectInPx.right *= p.s->font->cellSize.x;
        dirtyRectInPx.bottom *= p.s->font->cellSize.y;

        RECT scrollRect{};
        POINT scrollOffset{};
        DXGI_PRESENT_PARAMETERS params{
            .DirtyRectsCount = 1,
            .pDirtyRects = dirtyRectInPx.as_win32_rect(),
        };

        if (p.scrollOffset)
        {
            scrollRect = {
                0,
                std::max<til::CoordType>(0, p.scrollOffset),
                p.s->cellCount.x,
                p.s->cellCount.y + std::min<til::CoordType>(0, p.scrollOffset),
            };
            scrollOffset = {
                0,
                p.scrollOffset,
            };

            scrollRect.top *= p.s->font->cellSize.y;
            scrollRect.right *= p.s->font->cellSize.x;
            scrollRect.bottom *= p.s->font->cellSize.y;

            scrollOffset.y *= p.s->font->cellSize.y;

            params.pScrollRect = &scrollRect;
            params.pScrollOffset = &scrollOffset;
        }

        THROW_IF_FAILED(_swapChain->Present1(1, 0, &params));
    }
    else
    {
        THROW_IF_FAILED(_swapChain->Present(1, 0));
    }

    _waitForPresentation = true;
}

void SwapChainManager::WaitUntilCanRender() noexcept
{
    // IDXGISwapChain2::GetFrameLatencyWaitableObject returns an auto-reset event.
    // Once we've waited on the event, waiting on it again will block until the timeout elapses.
    // _r.waitForPresentation guards against this.
    if (_waitForPresentation)
    {
        WaitForSingleObjectEx(_frameLatencyWaitableObject.get(), 100, true);
        _waitForPresentation = false;
    }
}

DriverD3D11::DriverD3D11(wil::com_ptr<ID3D11Device2> device, wil::com_ptr<ID3D11DeviceContext2> deviceContext) :
    _device{ std::move(device) },
    _deviceContext{ std::move(deviceContext) }
{
    // Our constant buffer will never get resized
    {
        D3D11_BUFFER_DESC desc{};
        desc.ByteWidth = sizeof(ConstBuffer);
        desc.Usage = D3D11_USAGE_DEFAULT;
        desc.BindFlags = D3D11_BIND_CONSTANT_BUFFER;
        THROW_IF_FAILED(_device->CreateBuffer(&desc, nullptr, _constantBuffer.put()));
    }

    THROW_IF_FAILED(_device->CreateVertexShader(&shader_vs[0], sizeof(shader_vs), nullptr, _vertexShader.put()));
    THROW_IF_FAILED(_device->CreatePixelShader(&shader_ps[0], sizeof(shader_ps), nullptr, _pixelShader.put()));

#ifndef NDEBUG
    _d.sourceDirectory = std::filesystem::path{ __FILE__ }.parent_path();
    _d.sourceCodeWatcher = wil::make_folder_change_reader_nothrow(_d.sourceDirectory.c_str(), false, wil::FolderChangeEvents::FileName | wil::FolderChangeEvents::LastWriteTime, [this](wil::FolderChangeEvent, PCWSTR path) {
        if (til::ends_with(path, L".hlsl"))
        {
            auto expected = INT64_MAX;
            const auto invalidationTime = std::chrono::steady_clock::now() + std::chrono::milliseconds(100);
            _d.sourceCodeInvalidationTime.compare_exchange_strong(expected, invalidationTime.time_since_epoch().count(), std::memory_order_relaxed);
        }
    });
#endif
}

void DriverD3D11::Render(const RenderingPayload& p)
{
#ifndef NDEBUG
    if (const auto invalidationTime = _d.sourceCodeInvalidationTime.load(std::memory_order_relaxed); invalidationTime != INT64_MAX && invalidationTime <= std::chrono::steady_clock::now().time_since_epoch().count())
    {
        _d.sourceCodeInvalidationTime.store(INT64_MAX, std::memory_order_relaxed);

        try
        {
            static const auto compile = [](const std::filesystem::path& path, const char* target) {
                wil::com_ptr<ID3DBlob> error;
                wil::com_ptr<ID3DBlob> blob;
                const auto hr = D3DCompileFromFile(
                    /* pFileName   */ path.c_str(),
                    /* pDefines    */ nullptr,
                    /* pInclude    */ D3D_COMPILE_STANDARD_FILE_INCLUDE,
                    /* pEntrypoint */ "main",
                    /* pTarget     */ target,
                    /* Flags1      */ D3DCOMPILE_DEBUG | D3DCOMPILE_SKIP_OPTIMIZATION | D3DCOMPILE_PACK_MATRIX_COLUMN_MAJOR | D3DCOMPILE_ENABLE_STRICTNESS | D3DCOMPILE_WARNINGS_ARE_ERRORS,
                    /* Flags2      */ 0,
                    /* ppCode      */ blob.addressof(),
                    /* ppErrorMsgs */ error.addressof());

                if (error)
                {
                    std::thread t{ [error = std::move(error)]() noexcept {
                        MessageBoxA(nullptr, static_cast<const char*>(error->GetBufferPointer()), "Compilation error", MB_ICONERROR | MB_OK);
                    } };
                    t.detach();
                }

                THROW_IF_FAILED(hr);
                return blob;
            };

            const auto vs = compile(_d.sourceDirectory / L"shader_vs.hlsl", "vs_4_0");
            const auto ps = compile(_d.sourceDirectory / L"shader_ps.hlsl", "ps_4_0");

            wil::com_ptr<ID3D11VertexShader> vertexShader;
            wil::com_ptr<ID3D11PixelShader> pixelShader;
            THROW_IF_FAILED(_device->CreateVertexShader(vs->GetBufferPointer(), vs->GetBufferSize(), nullptr, vertexShader.addressof()));
            THROW_IF_FAILED(_device->CreatePixelShader(ps->GetBufferPointer(), ps->GetBufferSize(), nullptr, pixelShader.addressof()));

            _vertexShader = std::move(vertexShader);
            _pixelShader = std::move(pixelShader);
        }
        CATCH_LOG()
    }
#endif

    if (_generation != p.s.generation())
    {
        _swapChainManager.UpdateSwapChainSettings(
            p,
            _device.get(),
            [this]() {
                _renderTargetView.reset();
                _deviceContext->ClearState();
            },
            [this]() {
                _renderTargetView.reset();
                _deviceContext->ClearState();
                _deviceContext->Flush();
            });

        if (!_renderTargetView)
        {
            const auto buffer = _swapChainManager.GetBuffer();
            THROW_IF_FAILED(_device->CreateRenderTargetView(buffer.get(), nullptr, _renderTargetView.put()));
        }

        if (_fontGeneration != p.s->font.generation())
        {
            _swapChainManager.UpdateFontSettings(p);
            _d2dRenderTarget.reset();
            _atlasSizeInPixel = {};
            _fontGeneration = p.s->font.generation();
        }

        if (_miscGeneration != p.s->misc.generation())
        {
            _createCustomShaderResources(p);
            _miscGeneration = p.s->misc.generation();
        }

        if (_targetSize != p.s->targetSize)
        {
            D3D11_VIEWPORT viewport{};
            viewport.Width = static_cast<float>(p.s->targetSize.x);
            viewport.Height = static_cast<float>(p.s->targetSize.y);
            _deviceContext->RSSetViewports(1, &viewport);
            _targetSize = p.s->targetSize;
        }

        if (_cellCount != p.s->cellCount)
        {
            _cellBuffer.reset();
            _cellView.reset();

            D3D11_BUFFER_DESC desc{};
            desc.ByteWidth = gsl::narrow<u32>(static_cast<size_t>(p.s->cellCount.x) * static_cast<size_t>(p.s->cellCount.y) * sizeof(Cell));
            desc.Usage = D3D11_USAGE_DYNAMIC;
            desc.BindFlags = D3D11_BIND_SHADER_RESOURCE;
            desc.CPUAccessFlags = D3D11_CPU_ACCESS_WRITE;
            desc.MiscFlags = D3D11_RESOURCE_MISC_BUFFER_STRUCTURED;
            desc.StructureByteStride = sizeof(Cell);
            THROW_IF_FAILED(_device->CreateBuffer(&desc, nullptr, _cellBuffer.put()));
            THROW_IF_FAILED(_device->CreateShaderResourceView(_cellBuffer.get(), nullptr, _cellView.put()));

            _cellCount = p.s->cellCount;
        }

        _updateConstantBuffer(p);
        _setShaderResources(p);
        _generation = p.s.generation();
    }

    _adjustAtlasSize(p);
    _processGlyphQueue(p);

    {
#pragma warning(suppress : 26494) // Variable 'mapped' is uninitialized. Always initialize an object (type.5).
        D3D11_MAPPED_SUBRESOURCE mapped;
        THROW_IF_FAILED(_deviceContext->Map(_cellBuffer.get(), 0, D3D11_MAP_WRITE_DISCARD, 0, &mapped));
        assert(mapped.RowPitch >= p.cells.size() * sizeof(Cell));
        memcpy(mapped.pData, p.cells.data(), p.cells.size() * sizeof(Cell));
        _deviceContext->Unmap(_cellBuffer.get(), 0);
    }

    if (_customPixelShader)
    {
        _renderWithCustomShader(p);
    }
    else
    {
        // OM: Output Merger
        _deviceContext->OMSetRenderTargets(1, _renderTargetView.addressof(), nullptr);
        _deviceContext->Draw(3, 0);
    }

    _swapChainManager.Present(p);
}

void DriverD3D11::WaitUntilCanRender() noexcept
{
    _swapChainManager.WaitUntilCanRender();
}

void DriverD3D11::_renderWithCustomShader(const RenderingPayload& p) const
{
    // Render with our main shader just like Present().
    {
        // OM: Output Merger
        _deviceContext->OMSetRenderTargets(1, _customOffscreenTextureTargetView.addressof(), nullptr);
        _deviceContext->Draw(3, 0);
    }

    // Update the custom shader's constant buffer.
    {
        CustomConstBuffer data;
        data.time = std::chrono::duration<float>(std::chrono::steady_clock::now() - _customShaderStartTime).count();
        data.scale = p.d.font.pixelPerDIP;
        data.resolution.x = static_cast<float>(p.s->cellCount.x * p.s->font->cellSize.x);
        data.resolution.y = static_cast<float>(p.s->cellCount.y * p.s->font->cellSize.y);
        data.background = colorFromU32<f32x4>(p.s->misc->backgroundColor);

#pragma warning(suppress : 26494) // Variable 'mapped' is uninitialized. Always initialize an object (type.5).
        D3D11_MAPPED_SUBRESOURCE mapped;
        THROW_IF_FAILED(_deviceContext->Map(_customShaderConstantBuffer.get(), 0, D3D11_MAP_WRITE_DISCARD, 0, &mapped));
        assert(mapped.RowPitch >= sizeof(data));
        memcpy(mapped.pData, &data, sizeof(data));
        _deviceContext->Unmap(_customShaderConstantBuffer.get(), 0);
    }

    // Render with the custom shader.
    {
        // OM: Output Merger
        // customOffscreenTextureView was just rendered to via customOffscreenTextureTargetView and is
        // set as the output target. Before we can use it as an input we have to remove it as an output.
        _deviceContext->OMSetRenderTargets(1, _renderTargetView.addressof(), nullptr);

        // VS: Vertex Shader
        _deviceContext->VSSetShader(_customVertexShader.get(), nullptr, 0);

        // PS: Pixel Shader
        _deviceContext->PSSetShader(_customPixelShader.get(), nullptr, 0);
        _deviceContext->PSSetConstantBuffers(0, 1, _customShaderConstantBuffer.addressof());
        _deviceContext->PSSetShaderResources(0, 1, _customOffscreenTextureView.addressof());
        _deviceContext->PSSetSamplers(0, 1, _customShaderSamplerState.addressof());

        _deviceContext->Draw(4, 0);
    }

    // For the next frame we need to restore our context state.
    {
        // VS: Vertex Shader
        _deviceContext->VSSetShader(_vertexShader.get(), nullptr, 0);

        // PS: Pixel Shader
        _deviceContext->PSSetShader(_pixelShader.get(), nullptr, 0);
        _deviceContext->PSSetConstantBuffers(0, 1, _constantBuffer.addressof());
        const std::array resources{ _cellView.get(), _atlasView.get() };
        _deviceContext->PSSetShaderResources(0, gsl::narrow_cast<UINT>(resources.size()), resources.data());
        _deviceContext->PSSetSamplers(0, 0, nullptr);
    }
}

void DriverD3D11::_setShaderResources(const RenderingPayload& p) const
{
    // IA: Input Assembler
    // Our vertex shader uses a trick from Bill Bilodeau published in
    // "Vertex Shader Tricks" at GDC14 to draw a fullscreen triangle
    // without vertex/index buffers. This prepares our context for this.
    _deviceContext->IASetVertexBuffers(0, 0, nullptr, nullptr, nullptr);
    _deviceContext->IASetIndexBuffer(nullptr, DXGI_FORMAT_UNKNOWN, 0);
    _deviceContext->IASetInputLayout(nullptr);
    _deviceContext->IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP);

    // VS: Vertex Shader
    _deviceContext->VSSetShader(_vertexShader.get(), nullptr, 0);

    // PS: Pixel Shader
    _deviceContext->PSSetShader(_pixelShader.get(), nullptr, 0);
    _deviceContext->PSSetConstantBuffers(0, 1, _constantBuffer.addressof());
    const std::array resources{ _cellView.get(), _atlasView.get() };
    _deviceContext->PSSetShaderResources(0, gsl::narrow_cast<UINT>(resources.size()), resources.data());
}

void DriverD3D11::_updateConstantBuffer(const RenderingPayload& p) const noexcept
{
    const auto useClearType = p.s->misc->antialiasingMode == D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE;

    ConstBuffer data;
    data.viewport.x = 0;
    data.viewport.y = 0;
    data.viewport.z = static_cast<float>(p.s->cellCount.x * p.s->font->cellSize.x);
    data.viewport.w = static_cast<float>(p.s->cellCount.y * p.s->font->cellSize.y);
    DWrite_GetGammaRatios(p.gamma, data.gammaRatios);
    data.enhancedContrast = useClearType ? p.cleartypeEnhancedContrast : p.grayscaleEnhancedContrast;
    data.cellCountX = p.s->cellCount.x;
    data.cellSize.x = p.s->font->cellSize.x;
    data.cellSize.y = p.s->font->cellSize.y;
    data.underlinePos = p.s->font->underlinePos;
    data.underlineWidth = p.s->font->underlineWidth;
    data.strikethroughPos = p.s->font->strikethroughPos;
    data.strikethroughWidth = p.s->font->strikethroughWidth;
    data.doubleUnderlinePos.x = p.s->font->doubleUnderlinePos.x;
    data.doubleUnderlinePos.y = p.s->font->doubleUnderlinePos.y;
    data.thinLineWidth = p.s->font->thinLineWidth;
    data.backgroundColor = p.s->misc->backgroundColor;
    data.cursorColor = p.s->cursor->cursorColor;
    data.selectionColor = p.s->misc->selectionColor;
    data.useClearType = useClearType;
    _deviceContext->UpdateSubresource(_constantBuffer.get(), 0, nullptr, &data, 0, 0);
}

void DriverD3D11::_adjustAtlasSize(const RenderingPayload& p)
{
    // Only grow the atlas texture if our tileAllocator needs it to be larger.
    // We have no way of shrinking our tileAllocator at the moment,
    // so technically a `requiredSize != _atlasSizeInPixel`
    // comparison would be sufficient, but better safe than sorry.
    const auto requiredSize = p.tileAllocator.size();
    if (requiredSize.x <= _atlasSizeInPixel.x && requiredSize.y <= _atlasSizeInPixel.y)
    {
        return;
    }

    wil::com_ptr<ID3D11Texture2D> atlasBuffer;
    wil::com_ptr<ID3D11ShaderResourceView> atlasView;
    {
        D3D11_TEXTURE2D_DESC desc{};
        desc.Width = requiredSize.x;
        desc.Height = requiredSize.y;
        desc.MipLevels = 1;
        desc.ArraySize = 1;
        desc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
        desc.SampleDesc = { 1, 0 };
        desc.BindFlags = D3D11_BIND_SHADER_RESOURCE | D3D11_BIND_RENDER_TARGET;
        THROW_IF_FAILED(_device->CreateTexture2D(&desc, nullptr, atlasBuffer.addressof()));
        THROW_IF_FAILED(_device->CreateShaderResourceView(atlasBuffer.get(), nullptr, atlasView.addressof()));
    }

    // If a _atlasBuffer already existed, we can copy its glyphs
    // over to the new texture without re-rendering everything.
    if (_atlasSizeInPixel != u16x2{})
    {
        D3D11_BOX box;
        box.left = 0;
        box.top = 0;
        box.front = 0;
        box.right = _atlasSizeInPixel.x;
        box.bottom = _atlasSizeInPixel.y;
        box.back = 1;
        _deviceContext->CopySubresourceRegion1(atlasBuffer.get(), 0, 0, 0, 0, _atlasBuffer.get(), 0, &box, D3D11_COPY_NO_OVERWRITE);
    }

    {
        const auto surface = atlasBuffer.query<IDXGISurface>();
        wil::com_ptr<ID2D1RenderTarget> renderTarget;

        D2D1_RENDER_TARGET_PROPERTIES props{};
        props.type = D2D1_RENDER_TARGET_TYPE_DEFAULT;
        props.pixelFormat = { DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED };
        props.dpiX = static_cast<float>(p.s->font->dpi);
        props.dpiY = static_cast<float>(p.s->font->dpi);
        THROW_IF_FAILED(p.d2dFactory->CreateDxgiSurfaceRenderTarget(surface.get(), &props, renderTarget.addressof()));

        _d2dRenderTarget = renderTarget.query<ID2D1DeviceContext1>();
        // We don't really use D2D for anything except DWrite, but it
        // can't hurt to ensure that everything it does is pixel aligned.
        _d2dRenderTarget->SetAntialiasMode(D2D1_ANTIALIAS_MODE_ALIASED);
        // In case _realizedAntialiasingMode is D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE we'll
        // continuously adjust it in DriverD3D11::_drawGlyph. See _drawGlyph.
        _d2dRenderTarget->SetTextAntialiasMode(static_cast<D2D1_TEXT_ANTIALIAS_MODE>(p.s->misc->antialiasingMode));
        // Ensure that D2D uses the exact same gamma as our shader uses.
        _d2dRenderTarget->SetTextRenderingParams(p.renderingParams.get());
    }
    {
        static constexpr D2D1_COLOR_F color{ 1, 1, 1, 1 };
        THROW_IF_FAILED(_d2dRenderTarget->CreateSolidColorBrush(&color, nullptr, _brush.put()));
    }

    _atlasSizeInPixel = requiredSize;
    _atlasBuffer = std::move(atlasBuffer);
    _atlasView = std::move(atlasView);
    _setShaderResources(p);
}

void DriverD3D11::_processGlyphQueue(const RenderingPayload& p)
{
    if (p.glyphQueue.empty() && _cursorGeneration == p.s->cursor.generation())
    {
        return;
    }

    _d2dRenderTarget->BeginDraw();

    if (_cursorGeneration != p.s->cursor.generation())
    {
        _drawCursor(p, _d2dRenderTarget.get(), { 0, 0, 1, 1 }, _brush.get(), true);
        _cursorGeneration = p.s->cursor.generation();
    }

    for (const auto& it : p.glyphQueue)
    {
        _drawGlyph(p, it);
    }

    THROW_IF_FAILED(_d2dRenderTarget->EndDraw());
}

void DriverD3D11::_drawGlyph(const RenderingPayload& p, const TileHashMap::iterator& it) const
{
    const auto key = it->first.data();
    const auto value = it->second.data();
    const auto coords = &value->coords[0];
    const auto charsLength = key->charCount;
    const auto cellCount = key->attributes.cellCount;
    const auto textFormat = p.d.font.textFormats[key->attributes.italic][key->attributes.bold].get();
    const auto coloredGlyph = WI_IsFlagSet(value->flags, CellFlags::ColoredGlyph);
    const auto cachedLayout = _getCachedGlyphLayout(p, &key->chars[0], charsLength, cellCount, textFormat, coloredGlyph);

    // Colored glyphs cannot be drawn in linear gamma.
    // That's why we're simply alpha-blending them in the shader.
    // In order for this to work correctly we have to prevent them from being drawn
    // with ClearType, because we would then lack the alpha channel for the glyphs.
    if (coloredGlyph)
    {
        _d2dRenderTarget->SetTextAntialiasMode(D2D1_TEXT_ANTIALIAS_MODE_GRAYSCALE);
    }

    for (u16 i = 0; i < cellCount; ++i)
    {
        const auto coord = coords[i];

        D2D1_RECT_F rect;
        rect.left = static_cast<float>(coord.x) * p.d.font.dipPerPixel;
        rect.top = static_cast<float>(coord.y) * p.d.font.dipPerPixel;
        rect.right = rect.left + p.d.font.cellSizeDIP.x;
        rect.bottom = rect.top + p.d.font.cellSizeDIP.y;

        D2D1_POINT_2F origin;
        origin.x = rect.left - i * p.d.font.cellSizeDIP.x;
        origin.y = rect.top;

        _d2dRenderTarget->PushAxisAlignedClip(&rect, D2D1_ANTIALIAS_MODE_ALIASED);
        _d2dRenderTarget->Clear();

        cachedLayout.applyScaling(_d2dRenderTarget.get(), origin);

        // Now that we're done using origin to calculate the center point for our transformation
        // we can use it for its intended purpose to slightly shift the glyph around.
        origin.x += cachedLayout.offset.x;
        origin.y += cachedLayout.offset.y;
        _d2dRenderTarget->DrawTextLayout(origin, cachedLayout.textLayout.get(), _brush.get(), cachedLayout.options);

        cachedLayout.undoScaling(_d2dRenderTarget.get());

        _d2dRenderTarget->PopAxisAlignedClip();
    }

    if (coloredGlyph)
    {
        _d2dRenderTarget->SetTextAntialiasMode(static_cast<D2D1_TEXT_ANTIALIAS_MODE>(p.s->misc->antialiasingMode));
    }
}

CachedGlyphLayout IDriver::_getCachedGlyphLayout(const RenderingPayload& p, const wchar_t* chars, u16 charsLength, u16 cellCount, IDWriteTextFormat* textFormat, bool coloredGlyph)
{
    const f32x2 layoutBox{ cellCount * p.d.font.cellSizeDIP.x, p.d.font.cellSizeDIP.y };
    const f32x2 halfSize{ layoutBox.x * 0.5f, layoutBox.y * 0.5f };
    bool scalingRequired = false;
    f32x2 offset{ 0, 0 };
    f32x2 scale{ 1, 1 };

    // See D2DFactory::DrawText
    wil::com_ptr<IDWriteTextLayout> textLayout;
    THROW_IF_FAILED(p.dwriteFactory->CreateTextLayout(chars, charsLength, textFormat, layoutBox.x, layoutBox.y, textLayout.addressof()));
    if (p.d.font.typography)
    {
        textLayout->SetTypography(p.d.font.typography.get(), { 0, charsLength });
    }

    // Block Element and Box Drawing characters need to be handled separately,
    // because unlike regular ones they're supposed to fill the entire layout box.
    //
    // Ranges:
    // * 0x2500-0x257F: Box Drawing
    // * 0x2580-0x259F: Block Elements
    // * 0xE0A0-0xE0A3,0xE0B0-0xE0C8,0xE0CA-0xE0CA,0xE0CC-0xE0D4: PowerLine
    //   (https://github.com/ryanoasis/nerd-fonts/wiki/Glyph-Sets-and-Code-Points#powerline-symbols)
    //
    // The following `blockCharacters` forms a so called "inversion list".
    static constexpr std::array blockCharacters{
        // clang-format off
        L'\u2500', L'\u2580',
        L'\u2580', L'\u25A0',
        L'\uE0A0', L'\uE0A4',
        L'\uE0B0', L'\uE0C9',
        L'\uE0CA', L'\uE0CB',
        L'\uE0CC', L'\uE0D5',
        // clang-format on
    };

    if (charsLength == 1 && isInInversionList(blockCharacters, chars[0]))
    {
        wil::com_ptr<IDWriteFontCollection> fontCollection;
        THROW_IF_FAILED(textFormat->GetFontCollection(fontCollection.addressof()));
        const auto baseWeight = textFormat->GetFontWeight();
        const auto baseStyle = textFormat->GetFontStyle();

        TextAnalysisSource analysisSource{ chars, 1 };
        UINT32 mappedLength = 0;
        wil::com_ptr<IDWriteFont> mappedFont;
        FLOAT mappedScale = 0;
        THROW_IF_FAILED(p.systemFontFallback->MapCharacters(
            /* analysisSource     */ &analysisSource,
            /* textPosition       */ 0,
            /* textLength         */ 1,
            /* baseFontCollection */ fontCollection.get(),
            /* baseFamilyName     */ p.s->font->fontName.data(),
            /* baseWeight         */ baseWeight,
            /* baseStyle          */ baseStyle,
            /* baseStretch        */ DWRITE_FONT_STRETCH_NORMAL,
            /* mappedLength       */ &mappedLength,
            /* mappedFont         */ mappedFont.addressof(),
            /* scale              */ &mappedScale));

        if (mappedFont)
        {
            wil::com_ptr<IDWriteFontFace> fontFace;
            THROW_IF_FAILED(mappedFont->CreateFontFace(fontFace.addressof()));

            DWRITE_FONT_METRICS metrics;
            fontFace->GetMetrics(&metrics);

            const u32 codePoint = chars[0];
            u16 glyphIndex;
            THROW_IF_FAILED(fontFace->GetGlyphIndicesW(&codePoint, 1, &glyphIndex));

            DWRITE_GLYPH_METRICS glyphMetrics;
            THROW_IF_FAILED(fontFace->GetDesignGlyphMetrics(&glyphIndex, 1, &glyphMetrics));

            const f32x2 boxSize{
                static_cast<f32>(glyphMetrics.advanceWidth) / static_cast<f32>(metrics.designUnitsPerEm) * p.s->font->fontSizeInDIP,
                static_cast<f32>(glyphMetrics.advanceHeight) / static_cast<f32>(metrics.designUnitsPerEm) * p.s->font->fontSizeInDIP,
            };

            // We always want box drawing glyphs to exactly match the size of a terminal cell.
            // So for safe measure we'll always scale them to the exact size.
            // But add 1px to the destination size, so that we don't end up with fractional pixels.
            scalingRequired = true;
            scale.x = layoutBox.x / boxSize.x;
            scale.y = layoutBox.y / boxSize.y;
        }
    }
    else
    {
        DWRITE_OVERHANG_METRICS overhang;
        THROW_IF_FAILED(textLayout->GetOverhangMetrics(&overhang));

        const DWRITE_OVERHANG_METRICS clampedOverhang{
            std::max(0.0f, overhang.left),
            std::max(0.0f, overhang.top),
            std::max(0.0f, overhang.right),
            std::max(0.0f, overhang.bottom),
        };
        f32x2 actualSize{
            layoutBox.x + overhang.left + overhang.right,
            layoutBox.y + overhang.top + overhang.bottom,
        };

        // Long glyphs should be drawn with their proper design size, even if that makes them a bit blurry,
        // because otherwise we fail to support "pseudo" block characters like the "===" ligature in Cascadia Code.
        // If we didn't force upscale that ligatures it would seemingly shrink shorter and shorter, as its
        // glyph advance is often slightly shorter by a fractional pixel or two compared to our terminal's cells.
        // It's a trade off that keeps most glyphs "crisp" while retaining support for things like "===".
        // At least I can't think of any better heuristic for this at the moment...
        if (cellCount > 2)
        {
            const auto advanceScale = p.s->font->advanceScale;
            scalingRequired = true;
            scale = { advanceScale, advanceScale };
            actualSize.x *= advanceScale;
            actualSize.y *= advanceScale;
        }

        // We need to offset glyphs that are simply outside of our layout box (layoutBox.x/.y)
        // and additionally downsize glyphs that are entirely too large to fit in.
        // The DWRITE_OVERHANG_METRICS will tell us how many DIPs the layout box is too large/small.
        // It contains a positive number if the glyph is outside and a negative one if it's inside
        // the layout box. For example, given a layoutBox.x/.y (and cell size) of 20/30:
        // * "M" is the "largest" ASCII character and might be:
        //     left:    -0.6f
        //     right:   -0.6f
        //     top:     -7.6f
        //     bottom:  -7.4f
        //   "M" doesn't fill the layout box at all!
        //   This is because we've rounded up the Terminal's cell size to whole pixels in
        //   _resolveFontMetrics. top/bottom margins are fairly large because we added the
        //   chosen font's ascender, descender and line gap metrics to get our line height.
        //   --> offsetX = 0
        //   --> offsetY = 0
        //   --> scale   = 1
        // * The bar diacritic (U+0336 combining long stroke overlay)
        //     left:    -9.0f
        //     top:    -16.3f
        //     right:    5.6f
        //     bottom: -11.7f
        //   right is positive! Our glyph is 5.6 DIPs outside of the layout box and would
        //   appear cut off during rendering. left is negative at -9, which indicates that
        //   we can simply shift the glyph by 5.6 DIPs to the left to fit it into our bounds.
        //   --> offsetX = -5.6f
        //   --> offsetY = 0
        //   --> scale   = 1
        // * Any wide emoji in a narrow cell (U+26A0 warning sign)
        //     left:     6.7f
        //     top:     -4.1f
        //     right:    6.7f
        //     bottom:  -3.0f
        //   Our emoji is outside the bounds on both the left and right side and we need to shrink it.
        //   --> offsetX = 0
        //   --> offsetY = 0
        //   --> scale   = layoutBox.y / (layoutBox.y + left + right)
        //               = 0.69f
        offset.x = clampedOverhang.left - clampedOverhang.right;
        offset.y = clampedOverhang.top - clampedOverhang.bottom;

        if ((actualSize.x - layoutBox.x) > p.d.font.dipPerPixel)
        {
            scalingRequired = true;
            offset.x = (overhang.left - overhang.right) * 0.5f;
            scale.x = layoutBox.x / actualSize.x;
            scale.y = scale.x;
        }
        if ((actualSize.y - layoutBox.y) > p.d.font.dipPerPixel)
        {
            scalingRequired = true;
            offset.y = (overhang.top - overhang.bottom) * 0.5f;
            scale.x = std::min(scale.x, layoutBox.y / actualSize.y);
            scale.y = scale.x;
        }

        // As explained below, we use D2D1_DRAW_TEXT_OPTIONS_NO_SNAP to prevent a weird issue with baseline snapping.
        // But we do want it technically, so this re-implements baseline snapping... I think?
        // It calculates the new `baseline` height after transformation by `scale.y` relative to the center point `halfSize.y`.
        //
        // This works even if `scale.y == 1`, because then `baseline == baselineInDIP + offset.y` and `baselineInDIP`
        // is always measured in full pixels. So rounding it will be equivalent to just rounding `offset.y` itself.
        const auto baseline = halfSize.y + (p.s->font->baselineInDIP + offset.y - halfSize.y) * scale.y;
        // This rounds to the nearest multiple of p.d.font.dipPerPixel.
        const auto baselineFixed = roundf(baseline * p.d.font.pixelPerDIP) * p.d.font.dipPerPixel;
        offset.y += (baselineFixed - baseline) / scale.y;
    }

    auto options = D2D1_DRAW_TEXT_OPTIONS_NONE;
    // D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT enables a bunch of internal machinery
    // which doesn't have to run if we know we can't use it anyways in the shader.
    WI_SetFlagIf(options, D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT, coloredGlyph);
    // !!! IMPORTANT !!!
    // DirectWrite/2D snaps the baseline to whole pixels, which is something we technically
    // want (it makes text look crisp), but fails in weird ways if `scalingRequired` is true.
    // As our scaling matrix's dx/dy (center point) is based on the `origin` coordinates
    // each cell we draw gets a unique, fractional baseline which gets rounded differently.
    // I'm not 100% sure why that happens, since `origin` is always in full pixels...
    // But this causes wide glyphs to draw as tiles that are potentially misaligned vertically by a pixel.
    // The resulting text rendering looks especially bad for ligatures like "====" in Cascadia Code,
    // where every single "=" might be blatantly misaligned vertically (same for any box drawings).
    WI_SetFlagIf(options, D2D1_DRAW_TEXT_OPTIONS_NO_SNAP, scalingRequired);

    return CachedGlyphLayout{
        .textLayout = textLayout,
        .halfSize = halfSize,
        .offset = offset,
        .scale = scale,
        .options = options,
        .scalingRequired = scalingRequired,
    };
}

void IDriver::_drawCursor(const RenderingPayload& p, ID2D1RenderTarget* renderTarget, u16r rect, ID2D1Brush* brush, bool clear)
{
    // lineWidth is in D2D's DIPs. For instance if we have a 150-200% zoom scale we want to draw a 2px wide line.
    // At 150% scale lineWidth thus needs to be 1.33333... because at a zoom scale of 1.5 this results in a 2px wide line.
    const auto lineWidth = std::max(1.0f, static_cast<float>((p.s->font->dpi + USER_DEFAULT_SCREEN_DPI / 2) / USER_DEFAULT_SCREEN_DPI * USER_DEFAULT_SCREEN_DPI) / static_cast<float>(p.s->font->dpi));
    const auto cursorType = static_cast<CursorType>(p.s->cursor->cursorType);

    // `clip` is the rectangle within our texture atlas that's reserved for our cursor texture, ...
    D2D1_RECT_F clip;
    clip.left = static_cast<float>(rect.left) * p.d.font.cellSizeDIP.x;
    clip.top = static_cast<float>(rect.top) * p.d.font.cellSizeDIP.y;
    clip.right = static_cast<float>(rect.right) * p.d.font.cellSizeDIP.x;
    clip.bottom = static_cast<float>(rect.bottom) * p.d.font.cellSizeDIP.y;

    // ... whereas `rect` is just the visible (= usually white) portion of our cursor.
    auto box = clip;

    switch (cursorType)
    {
    case CursorType::Legacy:
        box.top = box.bottom - p.d.font.cellSizeDIP.y * static_cast<float>(p.s->cursor->heightPercentage) / 100.0f;
        break;
    case CursorType::VerticalBar:
        box.right = box.left + lineWidth;
        break;
    case CursorType::EmptyBox:
    {
        // EmptyBox is drawn as a line and unlike filled rectangles those are drawn centered on their
        // coordinates in such a way that the line border extends half the width to each side.
        // --> Our coordinates have to be 0.5 DIP off in order to draw a 2px line on a 200% scaling.
        const auto halfWidth = lineWidth / 2.0f;
        box.left += halfWidth;
        box.top += halfWidth;
        box.right -= halfWidth;
        box.bottom -= halfWidth;
        break;
    }
    case CursorType::Underscore:
    case CursorType::DoubleUnderscore:
        box.top = box.bottom - lineWidth;
        break;
    default:
        break;
    }

    // We need to clip the area we draw in to ensure we don't
    // accidentally draw into any neighboring texture atlas tiles.
    renderTarget->PushAxisAlignedClip(&clip, D2D1_ANTIALIAS_MODE_ALIASED);

    if (clear)
    {
        renderTarget->Clear();
    }

    if (cursorType == CursorType::EmptyBox)
    {
        renderTarget->DrawRectangle(&box, brush, lineWidth);
    }
    else
    {
        renderTarget->FillRectangle(&box, brush);
    }

    if (cursorType == CursorType::DoubleUnderscore)
    {
        const auto offset = lineWidth * 2.0f;
        box.top -= offset;
        box.bottom -= offset;
        renderTarget->FillRectangle(&box, brush);
    }

    renderTarget->PopAxisAlignedClip();
}

ID2D1Brush* DriverD2D::_brushWithColor(u32 color)
{
    if (_brushColor != color)
    {
        const auto d2dColor = colorFromU32(color);
        THROW_IF_FAILED(_d2dRenderTarget->CreateSolidColorBrush(&d2dColor, nullptr, _brush.put()));
        _brushColor = color;
    }
    return _brush.get();
}

CachedGlyphLayout::operator bool() const noexcept
{
    return static_cast<bool>(textLayout);
}

void CachedGlyphLayout::reset() noexcept
{
    textLayout.reset();
}

void CachedGlyphLayout::applyScaling(ID2D1RenderTarget* d2dRenderTarget, D2D1_POINT_2F origin) const noexcept
{
    __assume(d2dRenderTarget != nullptr);

    if (scalingRequired)
    {
        const D2D1_MATRIX_3X2_F transform{
            scale.x,
            0,
            0,
            scale.y,
            (origin.x + halfSize.x) * (1.0f - scale.x),
            (origin.y + halfSize.y) * (1.0f - scale.y),
        };
        d2dRenderTarget->SetTransform(&transform);
    }
}

void CachedGlyphLayout::undoScaling(ID2D1RenderTarget* d2dRenderTarget) const noexcept
{
    __assume(d2dRenderTarget != nullptr);

    if (scalingRequired)
    {
        static constexpr D2D1_MATRIX_3X2_F identity{ 1, 0, 0, 1, 0, 0 };
        d2dRenderTarget->SetTransform(&identity);
    }
}

DriverD2D::DriverD2D(wil::com_ptr<ID3D11Device2> device, wil::com_ptr<ID3D11DeviceContext2> deviceContext) :
    _device{ std::move(device) },
    _deviceContext{ std::move(deviceContext) }
{
}

void DriverD2D::Render(const RenderingPayload& p)
{
    _swapChainManager.UpdateSwapChainSettings(
        p,
        _device.get(),
        [this]() {
            _d2dRenderTarget.reset();
            _deviceContext->ClearState();
        },
        [this]() {
            _d2dRenderTarget.reset();
            _deviceContext->ClearState();
            _deviceContext->Flush();
        });

    if (_fontGeneration != p.s->font.generation())
    {
        {
            const auto buffer = _swapChainManager.GetBuffer();
            const auto surface = buffer.query<IDXGISurface>();

            D2D1_RENDER_TARGET_PROPERTIES props{};
            props.type = D2D1_RENDER_TARGET_TYPE_DEFAULT;
            props.pixelFormat = { DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED };
            props.dpiX = static_cast<float>(p.s->font->dpi);
            props.dpiY = static_cast<float>(p.s->font->dpi);
            wil::com_ptr<ID2D1RenderTarget> renderTarget;
            THROW_IF_FAILED(p.d2dFactory->CreateDxgiSurfaceRenderTarget(surface.get(), &props, renderTarget.addressof()));
            _d2dRenderTarget = renderTarget.query<ID2D1DeviceContext1>();
        }
        {
            static constexpr D2D1_COLOR_F color{ 1, 1, 1, 1 };
            THROW_IF_FAILED(_d2dRenderTarget->CreateSolidColorBrush(&color, nullptr, _brush.put()));
            _brushColor = 0xffffffff;
        }
        _fontGeneration = p.s->font.generation();
    }

    struct CellFlagHandler
    {
        CellFlags filter;
        decltype(&DriverD2D::_d2dCellFlagRendererCursor) func;
    };

    static constexpr std::array cellFlagHandlers{
        // Ordered by lowest to highest "layer".
        // The selection for instance is drawn on top of underlines, not under them.
        CellFlagHandler{ CellFlags::Underline, &DriverD2D::_d2dCellFlagRendererUnderline },
        CellFlagHandler{ CellFlags::UnderlineDotted, &DriverD2D::_d2dCellFlagRendererUnderlineDotted },
        CellFlagHandler{ CellFlags::UnderlineDouble, &DriverD2D::_d2dCellFlagRendererUnderlineDouble },
        CellFlagHandler{ CellFlags::Strikethrough, &DriverD2D::_d2dCellFlagRendererStrikethrough },
        CellFlagHandler{ CellFlags::Cursor, &DriverD2D::_d2dCellFlagRendererCursor },
        CellFlagHandler{ CellFlags::Selected, &DriverD2D::_d2dCellFlagRendererSelected },
    };

    auto left = gsl::narrow<u16>(p.dirtyRect.left);
    auto top = gsl::narrow<u16>(p.dirtyRect.top);
    auto right = gsl::narrow<u16>(p.dirtyRect.right);
    auto bottom = gsl::narrow<u16>(p.dirtyRect.bottom);
    if constexpr (debugGlyphGenerationPerformance)
    {
        left = 0;
        top = 0;
        right = p.s->cellCount.x;
        bottom = p.s->cellCount.y;
    }

    _d2dRenderTarget->BeginDraw();

    if (_miscGeneration != p.s->misc.generation())
    {
        _d2dRenderTarget->SetTextAntialiasMode(static_cast<D2D1_TEXT_ANTIALIAS_MODE>(p.s->misc->antialiasingMode));
        _d2dRenderTarget->Clear(colorFromU32(p.s->misc->backgroundColor));
        _miscGeneration = p.s->misc.generation();
    }

    for (u16 y = top; y < bottom; ++y)
    {
        const Cell* cells = p.cells.data() + y * p.s->cellCount.x;
        const TileHashMap::iterator* cellGlyphMappings = p.cellGlyphMapping.data() + y * p.s->cellCount.x;

        // left/right might intersect a wide glyph. We have to extend left/right
        // to include the entire glyph so that we can properly render it.
        // Since a series of identical narrow glyphs (2 spaces for instance) are stored in cellGlyphMappings
        // just like a single wide glyph (2 references to the same glyph in a row), the only way for us to
        // know where wide glyphs begin and end is to iterate the entire row and use the stored `cellCount`.
        u16 beg = 0;
        for (;;)
        {
            const auto cellCount = cellGlyphMappings[beg]->first.data()->attributes.cellCount;
            const auto begNext = gsl::narrow_cast<u16>(beg + cellCount);

            if (begNext > left)
            {
                break;
            }

            beg = begNext;
        }
        auto end = beg;
        for (;;)
        {
            const auto cellCount = cellGlyphMappings[end]->first.data()->attributes.cellCount;
            end += cellCount;

            if (end >= right)
            {
                break;
            }
        }

        // Draw background.
        {
            _d2dRenderTarget->SetPrimitiveBlend(D2D1_PRIMITIVE_BLEND_COPY);

            auto x1 = beg;
            auto x2 = gsl::narrow_cast<u16>(x1 + 1);
            auto currentColor = cells[x1].color.y;

            for (; x2 < end; ++x2)
            {
                const auto color = cells[x2].color.y;

                if (currentColor != color)
                {
                    const u16r rect{ x1, y, x2, gsl::narrow_cast<u16>(y + 1) };
                    _d2dFillRectangle(p, rect, currentColor);
                    x1 = x2;
                    currentColor = color;
                }
            }

            {
                const u16r rect{ x1, y, x2, gsl::narrow_cast<u16>(y + 1) };
                _d2dFillRectangle(p, rect, currentColor);
            }

            _d2dRenderTarget->SetPrimitiveBlend(D2D1_PRIMITIVE_BLEND_SOURCE_OVER);
        }

        // Draw text.
        for (auto x = beg; x < end;)
        {
            const auto& it = cellGlyphMappings[x];
            const u16x2 coord{ x, y };
            const auto color = cells[x].color.x;
            x += _d2dDrawGlyph(p, it, coord, color);
        }

        // Draw underlines, cursors, selections, etc.
        for (const auto& handler : cellFlagHandlers)
        {
            auto x1 = beg;
            auto currentFlags = CellFlags::None;

            for (auto x2 = beg; x2 < end; ++x2)
            {
                const auto flags = cells[x2].flags & handler.filter;

                if (currentFlags != flags)
                {
                    if (currentFlags != CellFlags::None)
                    {
                        const u16r rect{ x1, y, x2, gsl::narrow_cast<u16>(y + 1) };
                        const auto color = cells[x1].color.x;
                        (this->*handler.func)(p, rect, color);
                    }

                    x1 = x2;
                    currentFlags = flags;
                }
            }

            if (currentFlags != CellFlags::None)
            {
                const u16r rect{ x1, y, right, gsl::narrow_cast<u16>(y + 1) };
                const auto color = cells[x1].color.x;
                (this->*handler.func)(p, rect, color);
            }
        }
    }

    THROW_IF_FAILED(_d2dRenderTarget->EndDraw());
}

void DriverD2D::WaitUntilCanRender() noexcept
{
}

// See _drawGlyph() for reference.
u16 DriverD2D::_d2dDrawGlyph(const RenderingPayload& p, const TileHashMap::iterator& it, const u16x2 coord, const u32 color)
{
    const auto key = it->first.data();
    const auto value = it->second.data();
    const auto charsLength = key->charCount;
    const auto cellCount = key->attributes.cellCount;
    const auto textFormat = p.d.font.textFormats[key->attributes.italic][key->attributes.bold].get();
    const auto coloredGlyph = WI_IsFlagSet(value->flags, CellFlags::ColoredGlyph);

    auto& cachedLayout = it->second.cachedLayout;
    if (!cachedLayout)
    {
        cachedLayout = _getCachedGlyphLayout(p, &key->chars[0], charsLength, cellCount, textFormat, coloredGlyph);
    }

    D2D1_RECT_F rect;
    rect.left = static_cast<float>(coord.x) * p.d.font.cellSizeDIP.x;
    rect.top = static_cast<float>(coord.y) * p.d.font.cellSizeDIP.y;
    rect.right = static_cast<float>(coord.x + cellCount) * p.d.font.cellSizeDIP.x;
    rect.bottom = rect.top + p.d.font.cellSizeDIP.y;

    D2D1_POINT_2F origin;
    origin.x = rect.left;
    origin.y = rect.top;

    _d2dRenderTarget->PushAxisAlignedClip(&rect, D2D1_ANTIALIAS_MODE_ALIASED);

    cachedLayout.applyScaling(_d2dRenderTarget.get(), origin);

    origin.x += cachedLayout.offset.x;
    origin.y += cachedLayout.offset.y;
    _d2dRenderTarget->DrawTextLayout(origin, cachedLayout.textLayout.get(), _brushWithColor(color), cachedLayout.options);

    cachedLayout.undoScaling(_d2dRenderTarget.get());

    _d2dRenderTarget->PopAxisAlignedClip();

    return cellCount;
}

void DriverD2D::_d2dDrawLine(const RenderingPayload& p, u16r rect, u16 pos, u16 width, u32 color, ID2D1StrokeStyle* strokeStyle)
{
    const auto w = static_cast<float>(width) * p.d.font.dipPerPixel;
    const auto y1 = static_cast<float>(rect.top) * p.d.font.cellSizeDIP.y + static_cast<float>(pos) * p.d.font.dipPerPixel + w * 0.5f;
    const auto x1 = static_cast<float>(rect.left) * p.d.font.cellSizeDIP.x;
    const auto x2 = static_cast<float>(rect.right) * p.d.font.cellSizeDIP.x;
    const auto brush = _brushWithColor(color);
    _d2dRenderTarget->DrawLine({ x1, y1 }, { x2, y1 }, brush, w, strokeStyle);
}

void DriverD2D::_d2dFillRectangle(const RenderingPayload& p, u16r rect, u32 color)
{
    const D2D1_RECT_F r{
        .left = static_cast<float>(rect.left) * p.d.font.cellSizeDIP.x,
        .top = static_cast<float>(rect.top) * p.d.font.cellSizeDIP.y,
        .right = static_cast<float>(rect.right) * p.d.font.cellSizeDIP.x,
        .bottom = static_cast<float>(rect.bottom) * p.d.font.cellSizeDIP.y,
    };
    const auto brush = _brushWithColor(color);
    _d2dRenderTarget->FillRectangle(r, brush);
}

void DriverD2D::_d2dCellFlagRendererCursor(const RenderingPayload& p, u16r rect, u32 color)
{
    _drawCursor(p, _d2dRenderTarget.get(), rect, _brushWithColor(p.s->cursor->cursorColor), false);
}

void DriverD2D::_d2dCellFlagRendererSelected(const RenderingPayload& p, u16r rect, u32 color)
{
    _d2dFillRectangle(p, rect, p.s->misc->selectionColor);
}

void DriverD2D::_d2dCellFlagRendererUnderline(const RenderingPayload& p, u16r rect, u32 color)
{
    _d2dDrawLine(p, rect, p.s->font->underlinePos, p.s->font->underlineWidth, color);
}

void DriverD2D::_d2dCellFlagRendererUnderlineDotted(const RenderingPayload& p, u16r rect, u32 color)
{
    if (!_dottedStrokeStyle)
    {
        static constexpr D2D1_STROKE_STYLE_PROPERTIES props{ .dashStyle = D2D1_DASH_STYLE_CUSTOM };
        static constexpr FLOAT dashes[2]{ 1, 2 };
        THROW_IF_FAILED(p.d2dFactory->CreateStrokeStyle(&props, &dashes[0], 2, _dottedStrokeStyle.addressof()));
    }

    _d2dDrawLine(p, rect, p.s->font->underlinePos, p.s->font->underlineWidth, color, _dottedStrokeStyle.get());
}

void DriverD2D::_d2dCellFlagRendererUnderlineDouble(const RenderingPayload& p, u16r rect, u32 color)
{
    _d2dDrawLine(p, rect, p.s->font->doubleUnderlinePos.x, p.s->font->thinLineWidth, color);
    _d2dDrawLine(p, rect, p.s->font->doubleUnderlinePos.y, p.s->font->thinLineWidth, color);
}

void DriverD2D::_d2dCellFlagRendererStrikethrough(const RenderingPayload& p, u16r rect, u32 color)
{
    _d2dDrawLine(p, rect, p.s->font->strikethroughPos, p.s->font->strikethroughWidth, color);
}
