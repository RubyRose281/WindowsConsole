// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

#include "pch.h"
#include "AtlasEngine.h"

#include "dwrite.h"

#include <DirectXMath.h>

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

using namespace Microsoft::Console::Render;

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

static AtlasEngine::f32r getGlyphRunBlackBox(const DWRITE_GLYPH_RUN& glyphRun, float baselineX, float baselineY)
{
    DWRITE_FONT_METRICS fontMetrics;
    glyphRun.fontFace->GetMetrics(&fontMetrics);

    til::small_vector<DWRITE_GLYPH_METRICS, 16> glyphRunMetrics;
    glyphRunMetrics.resize(glyphRun.glyphCount);
    glyphRun.fontFace->GetDesignGlyphMetrics(glyphRun.glyphIndices, glyphRun.glyphCount, glyphRunMetrics.data(), false);

    float const fontScale = glyphRun.fontEmSize / fontMetrics.designUnitsPerEm;
    AtlasEngine::f32r accumulatedBounds;

    for (uint32_t i = 0; i < glyphRun.glyphCount; ++i)
    {
        const auto& glyphMetrics = glyphRunMetrics[i];
        const auto glyphAdvance = glyphRun.glyphAdvances ? glyphRun.glyphAdvances[i] : glyphMetrics.advanceWidth * fontScale;

        const auto left = static_cast<float>(glyphMetrics.leftSideBearing) * fontScale;
        const auto top = static_cast<float>(glyphMetrics.topSideBearing - glyphMetrics.verticalOriginY) * fontScale;
        const auto right = static_cast<float>(gsl::narrow_cast<INT32>(glyphMetrics.advanceWidth) - glyphMetrics.rightSideBearing) * fontScale;
        const auto bottom = static_cast<float>(gsl::narrow_cast<INT32>(glyphMetrics.advanceHeight) - glyphMetrics.bottomSideBearing - glyphMetrics.verticalOriginY) * fontScale;

        if (left < right && top < bottom)
        {
            auto glyphX = baselineX;
            auto glyphY = baselineY;
            if (glyphRun.glyphOffsets)
            {
                const auto advanceOffset = glyphRun.glyphOffsets[i].advanceOffset;
                const auto ascenderOffset = glyphRun.glyphOffsets[i].ascenderOffset;
                glyphX += advanceOffset;
                glyphY += -ascenderOffset;
            }

            accumulatedBounds.left = std::min(accumulatedBounds.left, left + glyphX);
            accumulatedBounds.top = std::min(accumulatedBounds.top, top + glyphY);
            accumulatedBounds.right = std::max(accumulatedBounds.right, right + glyphX);
            accumulatedBounds.bottom = std::max(accumulatedBounds.bottom, bottom + glyphY);
        }

        baselineX += glyphAdvance;
    }

    return accumulatedBounds;
}

static bool drawGlyphRun(IDWriteFactory4* dwriteFactory, ID2D1DeviceContext4* deviceContext, D2D_POINT_2F baselineOrigin, const DWRITE_GLYPH_RUN* glyphRun, ID2D1SolidColorBrush* foregroundBrush) noexcept
{
    static constexpr auto measuringMode = DWRITE_MEASURING_MODE_NATURAL;
    static constexpr auto formats =
        DWRITE_GLYPH_IMAGE_FORMATS_TRUETYPE |
        DWRITE_GLYPH_IMAGE_FORMATS_CFF |
        DWRITE_GLYPH_IMAGE_FORMATS_COLR |
        DWRITE_GLYPH_IMAGE_FORMATS_SVG |
        DWRITE_GLYPH_IMAGE_FORMATS_PNG |
        DWRITE_GLYPH_IMAGE_FORMATS_JPEG |
        DWRITE_GLYPH_IMAGE_FORMATS_TIFF |
        DWRITE_GLYPH_IMAGE_FORMATS_PREMULTIPLIED_B8G8R8A8;

    wil::com_ptr<IDWriteColorGlyphRunEnumerator1> colorRunEnumerator;
    const auto hr = dwriteFactory->TranslateColorGlyphRun(baselineOrigin, glyphRun, nullptr, formats, measuringMode, nullptr, 0, &colorRunEnumerator);
    if (hr == DWRITE_E_NOCOLOR)
    {
        deviceContext->DrawGlyphRun(baselineOrigin, glyphRun, foregroundBrush, measuringMode);
        return false;
    }
    THROW_IF_FAILED(hr);

    wil::com_ptr<ID2D1SolidColorBrush> solidBrush;

    for (;;)
    {
        BOOL hasRun;
        THROW_IF_FAILED(colorRunEnumerator->MoveNext(&hasRun));
        if (!hasRun)
        {
            break;
        }

        const DWRITE_COLOR_GLYPH_RUN1* colorGlyphRun;
        THROW_IF_FAILED(colorRunEnumerator->GetCurrentRun(&colorGlyphRun));

        ID2D1Brush* runBrush;
        if (colorGlyphRun->paletteIndex == /*DWRITE_NO_PALETTE_INDEX*/ 0xffff)
        {
            runBrush = foregroundBrush;
        }
        else
        {
            if (!solidBrush)
            {
                THROW_IF_FAILED(deviceContext->CreateSolidColorBrush(colorGlyphRun->runColor, &solidBrush));
            }
            else
            {
                solidBrush->SetColor(colorGlyphRun->runColor);
            }
            runBrush = solidBrush.get();
        }

        switch (colorGlyphRun->glyphImageFormat)
        {
        case DWRITE_GLYPH_IMAGE_FORMATS_NONE:
            break;
        case DWRITE_GLYPH_IMAGE_FORMATS_PNG:
        case DWRITE_GLYPH_IMAGE_FORMATS_JPEG:
        case DWRITE_GLYPH_IMAGE_FORMATS_TIFF:
        case DWRITE_GLYPH_IMAGE_FORMATS_PREMULTIPLIED_B8G8R8A8:
            deviceContext->DrawColorBitmapGlyphRun(colorGlyphRun->glyphImageFormat, baselineOrigin, &colorGlyphRun->glyphRun, colorGlyphRun->measuringMode, D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION_DEFAULT);
            break;
        case DWRITE_GLYPH_IMAGE_FORMATS_SVG:
            deviceContext->DrawSvgGlyphRun(baselineOrigin, &colorGlyphRun->glyphRun, runBrush, nullptr, 0, colorGlyphRun->measuringMode);
            break;
        default:
            deviceContext->DrawGlyphRun(baselineOrigin, &colorGlyphRun->glyphRun, colorGlyphRun->glyphRunDescription, runBrush, colorGlyphRun->measuringMode);
            break;
        }
    }

    return true;
}

#pragma region IRenderEngine

// Present() is called without the console buffer lock being held.
// --> Put as much in here as possible.
[[nodiscard]] HRESULT AtlasEngine::Present() noexcept
try
{
    const til::rect fullRect{ 0, 0, _r.cellCount.x, _r.cellCount.y };

    // A change in the selection or background color (etc.) forces a full redraw.
    if (WI_IsFlagSet(_r.invalidations, RenderInvalidations::ConstBuffer) || _r.customPixelShader)
    {
        _r.dirtyRect = fullRect;
    }

    if (!_r.dirtyRect)
    {
        return S_OK;
    }

    // See documentation for IDXGISwapChain2::GetFrameLatencyWaitableObject method:
    // > For every frame it renders, the app should wait on this handle before starting any rendering operations.
    // > Note that this requirement includes the first frame the app renders with the swap chain.
    assert(debugGeneralPerformance || _r.frameLatencyWaitableObjectUsed);

    if (!_r.atlasBuffer)
    {
        {
            D3D11_TEXTURE2D_DESC desc{};
            desc.Width = 2048;
            desc.Height = 2048;
            desc.MipLevels = 1;
            desc.ArraySize = 1;
            desc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
            desc.SampleDesc = { 1, 0 };
            desc.BindFlags = D3D11_BIND_SHADER_RESOURCE | D3D11_BIND_RENDER_TARGET;
            THROW_IF_FAILED(_r.device->CreateTexture2D(&desc, nullptr, _r.atlasBuffer.addressof()));
            THROW_IF_FAILED(_r.device->CreateShaderResourceView(_r.atlasBuffer.get(), nullptr, _r.atlasView.addressof()));
        }

        {
            _r.rectPackerData.resize(2048);
            stbrp_init_target(&_r.rectPacker, 2048, 2048, _r.rectPackerData.data(), gsl::narrow_cast<int>(_r.rectPackerData.size()));
        }

        {
            const auto surface = _r.atlasBuffer.query<IDXGISurface>();

            wil::com_ptr<IDWriteRenderingParams1> renderingParams;
            DWrite_GetRenderParams(_sr.dwriteFactory.get(), &_r.gamma, &_r.cleartypeEnhancedContrast, &_r.grayscaleEnhancedContrast, renderingParams.addressof());

            D2D1_RENDER_TARGET_PROPERTIES props{};
            props.type = D2D1_RENDER_TARGET_TYPE_DEFAULT;
            props.pixelFormat = { DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED };
            props.dpiX = static_cast<float>(_r.dpi);
            props.dpiY = static_cast<float>(_r.dpi);
            wil::com_ptr<ID2D1RenderTarget> renderTarget;
            THROW_IF_FAILED(_sr.d2dFactory->CreateDxgiSurfaceRenderTarget(surface.get(), &props, renderTarget.addressof()));
            _r.d2dRenderTarget = renderTarget.query<ID2D1DeviceContext>();

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
            THROW_IF_FAILED(_r.d2dRenderTarget->CreateSolidColorBrush(&color, nullptr, _r.brush.put()));
            _r.brushColor = 0xffffffff;
        }
    }

    {
        _r.vertexData.clear();
        _r.d2dRenderTarget->BeginDraw();

        for (const auto& row : _r.rows)
        {
            for (auto& placement : row)
            {
                const auto [it, ok] = _r.glyphCache.try_emplace(placement.key);
                if (ok)
                {
                    while (true)
                    {
                        DWRITE_GLYPH_RUN glyphRun;
                        glyphRun.fontFace = placement.key.fontFace.get();
                        glyphRun.fontEmSize = _r.fontMetrics.fontSizeInDIP;
                        glyphRun.glyphCount = gsl::narrow_cast<UINT32>(placement.key.glyphs.size());
                        glyphRun.glyphIndices = placement.key.glyphs.data();
                        glyphRun.glyphAdvances = nullptr;
                        glyphRun.glyphOffsets = nullptr;
                        glyphRun.isSideways = FALSE;
                        glyphRun.bidiLevel = 0;

                        auto box = getGlyphRunBlackBox(glyphRun, 0, 0);
                        if (box.left >= box.right || box.top >= box.bottom)
                        {
                            break;
                        }

                        stbrp_rect rect{};
                        rect.w = gsl::narrow_cast<int>(roundf((box.right - box.left) * _r.pixelPerDIP));
                        rect.h = gsl::narrow_cast<int>(roundf((box.bottom - box.top) * _r.pixelPerDIP));
                        if (!stbrp_pack_rects(&_r.rectPacker, &rect, 1))
                        {
                            __debugbreak();
                            break;
                        }

                        const f32x2 offset{
                            roundf(box.left * _r.pixelPerDIP),
                            roundf(box.top * _r.pixelPerDIP),
                        };
                        const D2D1_POINT_2F baseline{
                            (rect.x - offset.x) * _r.dipPerPixel,
                            (rect.y - offset.y) * _r.dipPerPixel,
                        };

                        // My understanding is that a ID2D1DeviceContext4 implies the existence of a IDWriteFactory5, because only
                        // IDWriteFactory5 has a CreateDevice() for ID2D1Device4 with which a ID2D1DeviceContext4 can be created.
                        if (const auto d2dRenderTarget4 = _r.d2dRenderTarget.query<ID2D1DeviceContext4>())
                        {
                            const auto factory4 = _sr.dwriteFactory.query<IDWriteFactory4>();
                            drawGlyphRun(factory4.get(), d2dRenderTarget4.get(), baseline, &glyphRun, _r.brush.get());
                        }
                        else
                        {
                            _r.d2dRenderTarget->DrawGlyphRun(baseline, &glyphRun, _r.brush.get(), DWRITE_MEASURING_MODE_NATURAL);
                        }

                        it->second.uv.x = static_cast<f32>(rect.x);
                        it->second.uv.y = static_cast<f32>(rect.y);
                        it->second.wh.x = static_cast<f32>(rect.w);
                        it->second.wh.y = static_cast<f32>(rect.h);
                        it->second.offset = offset;
                        break;
                    }
                }

                if (it->second.wh != f32x2{})
                {
                    static constexpr f32x2 vertices[]{
                        { 0, 0 },
                        { 1, 0 },
                        { 0, 1 },
                        { 1, 1 },
                        { 0, 1 },
                        { 1, 0 },
                    };

                    for (const auto& v : vertices)
                    {
                        const auto vv = it->second;
                        auto& ref = _r.vertexData.emplace_back();
                        ref.position.x = placement.baseline.x + vv.offset.x + v.x * vv.wh.x;
                        ref.position.y = placement.baseline.y + vv.offset.y + v.y * vv.wh.y;
                        // TODO: matrix transform in the vertex shader
                        ref.position.x = ref.position.x * (2.0f / _api.sizeInPixel.x) - 1.0f;
                        ref.position.y = ref.position.y * (-2.0f / _api.sizeInPixel.y) + 1.0f;
                        ref.texcoord.x = vv.uv.x + v.x * vv.wh.x;
                        ref.texcoord.y = vv.uv.y + v.y * vv.wh.y;
                        ref.color = placement.color;
                    }
                }
            }
        }

        THROW_IF_FAILED(_r.d2dRenderTarget->EndDraw());
    }

    if (_r.vertexData.empty())
    {
        return S_OK;
    }

    if (WI_IsFlagSet(_r.invalidations, RenderInvalidations::ConstBuffer))
    {
        ConstBuffer data;
        DWrite_GetGammaRatios(_r.gamma, data.gammaRatios);
        data.enhancedContrast = _r.grayscaleEnhancedContrast;
#pragma warning(suppress : 26447) // The function is declared 'noexcept' but calls function '...' which may throw exceptions (f.6).
        _r.deviceContext->UpdateSubresource(_r.constantBuffer.get(), 0, nullptr, &data, 0, 0);
        WI_ClearFlag(_r.invalidations, RenderInvalidations::ConstBuffer);
    }

    {
#pragma warning(suppress : 26494) // Variable 'mapped' is uninitialized. Always initialize an object (type.5).
        D3D11_MAPPED_SUBRESOURCE mapped;
        THROW_IF_FAILED(_r.deviceContext->Map(_r.vertexBuffer.get(), 0, D3D11_MAP_WRITE_DISCARD, 0, &mapped));
        memcpy(mapped.pData, _r.vertexData.data(), _r.vertexData.size() * sizeof(VertexData));
        _r.deviceContext->Unmap(_r.vertexBuffer.get(), 0);
    }

    {
        D3D11_VIEWPORT viewport{};
        viewport.Width = static_cast<float>(_api.sizeInPixel.x);
        viewport.Height = static_cast<float>(_api.sizeInPixel.y);

        static constexpr f32 color[4]{ 0.047f, 0.047f, 0.047f, 1 };
        _r.deviceContext->ClearRenderTargetView(_r.renderTargetView.get(), color);

        // IA: Input Assembler
        _r.deviceContext->IASetInputLayout(_r.textInputLayout.get());
        static constexpr UINT stride = sizeof(VertexData);
        static constexpr UINT offset = 0;
        _r.deviceContext->IASetVertexBuffers(0, 1, _r.vertexBuffer.addressof(), &stride, &offset);
        //_r.deviceContext->IASetIndexBuffer()
        _r.deviceContext->IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);

        // VS: Vertex Shader
        _r.deviceContext->VSSetShader(_r.vertexShader.get(), nullptr, 0);

        // RS: Rasterizer Stage
        _r.deviceContext->RSSetViewports(1, &viewport);
        _r.deviceContext->RSSetState(nullptr);

        // PS: Pixel Shader
        _r.deviceContext->PSSetShader(_r.pixelShader.get(), nullptr, 0);
        _r.deviceContext->PSSetShaderResources(0, 1, _r.atlasView.addressof());
        _r.deviceContext->PSSetConstantBuffers(0, 1, _r.constantBuffer.addressof());

        // OM: Output Merger
        _r.deviceContext->OMSetBlendState(_r.textBlendState.get(), nullptr, 0xffffffff);
        _r.deviceContext->OMSetRenderTargets(1, _r.renderTargetView.addressof(), nullptr);

        _r.deviceContext->Draw(gsl::narrow_cast<UINT>(_r.vertexData.size()), 0);
    }

    if constexpr (false)
    {
        _r.deviceContext->RSSetState(_r.wireframeRasterizerState.get());
        _r.deviceContext->PSSetShader(_r.wireframePixelShader.get(), nullptr, 0);
        _r.deviceContext->Draw(gsl::narrow_cast<UINT>(_r.vertexData.size()), 0);
    }

    if (_r.dirtyRect != fullRect)
    {
        auto dirtyRectInPx = _r.dirtyRect;
        dirtyRectInPx.left *= _r.fontMetrics.cellSize.x;
        dirtyRectInPx.top *= _r.fontMetrics.cellSize.y;
        dirtyRectInPx.right *= _r.fontMetrics.cellSize.x;
        dirtyRectInPx.bottom *= _r.fontMetrics.cellSize.y;

        RECT scrollRect{};
        POINT scrollOffset{};
        DXGI_PRESENT_PARAMETERS params{
            .DirtyRectsCount = 1,
            .pDirtyRects = dirtyRectInPx.as_win32_rect(),
        };

        if (_r.scrollOffset)
        {
            scrollRect = {
                0,
                std::max<til::CoordType>(0, _r.scrollOffset),
                _r.cellCount.x,
                _r.cellCount.y + std::min<til::CoordType>(0, _r.scrollOffset),
            };
            scrollOffset = {
                0,
                _r.scrollOffset,
            };

            scrollRect.top *= _r.fontMetrics.cellSize.y;
            scrollRect.right *= _r.fontMetrics.cellSize.x;
            scrollRect.bottom *= _r.fontMetrics.cellSize.y;

            scrollOffset.y *= _r.fontMetrics.cellSize.y;

            params.pScrollRect = &scrollRect;
            params.pScrollOffset = &scrollOffset;
        }

        THROW_IF_FAILED(_r.swapChain->Present1(1, 0, &params));
    }
    else
    {
        THROW_IF_FAILED(_r.swapChain->Present(1, 0));
    }

    _r.waitForPresentation = true;

    if (!_r.dxgiFactory->IsCurrent())
    {
        WI_SetFlag(_api.invalidations, ApiInvalidations::Device);
    }

    return S_OK;
}
catch (const wil::ResultException& exception)
{
    // TODO: this writes to _api.
    return _handleException(exception);
}
CATCH_RETURN()

[[nodiscard]] bool AtlasEngine::RequiresContinuousRedraw() noexcept
{
    return debugGeneralPerformance || _r.requiresContinuousRedraw;
}

void AtlasEngine::WaitUntilCanRender() noexcept
{
    // IDXGISwapChain2::GetFrameLatencyWaitableObject returns an auto-reset event.
    // Once we've waited on the event, waiting on it again will block until the timeout elapses.
    // _r.waitForPresentation guards against this.
    if (std::exchange(_r.waitForPresentation, false))
    {
        WaitForSingleObjectEx(_r.frameLatencyWaitableObject.get(), 100, true);
#ifndef NDEBUG
        _r.frameLatencyWaitableObjectUsed = true;
#endif
    }
}

#pragma endregion
