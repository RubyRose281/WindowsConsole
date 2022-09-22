// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

#include "pch.h"
#include "AtlasEngine.h"

#include "../base/FontCache.h"

// #### NOTE ####
// If you see any code in here that contains "_r." you might be seeing a race condition.
// The AtlasEngine::Present() method is called on a background thread without any locks,
// while any of the API methods (like AtlasEngine::Invalidate) might be called concurrently.
// The usage of _r fields is unsafe as those are accessed and written to by the Present() method.

#pragma warning(disable : 4100) // '...': unreferenced formal parameter
// Disable a bunch of warnings which get in the way of writing performant code.
#pragma warning(disable : 26429) // Symbol 'data' is never tested for nullness, it can be marked as not_null (f.23).
#pragma warning(disable : 26446) // Prefer to use gsl::at() instead of unchecked subscript operator (bounds.4).
#pragma warning(disable : 26459) // You called an STL function '...' with a raw pointer parameter at position '...' that may be unsafe [...].
#pragma warning(disable : 26481) // Don't use pointer arithmetic. Use span instead (bounds.1).
#pragma warning(disable : 26482) // Only index into arrays using constant expressions (bounds.2).

using namespace Microsoft::Console::Render;

// Like gsl::narrow but returns a HRESULT.
#pragma warning(push)
#pragma warning(disable : 26472) // Don't use a static_cast for arithmetic conversions. Use brace initialization, gsl::narrow_cast or gsl::narrow (type.1).
template<typename T, typename U>
constexpr HRESULT api_narrow(U val, T& out) noexcept
{
    out = static_cast<T>(val);
    return static_cast<U>(out) != val || (std::is_signed_v<T> != std::is_signed_v<U> && out < T{} != val < U{}) ? HRESULT_FROM_WIN32(ERROR_ARITHMETIC_OVERFLOW) : S_OK;
}
#pragma warning(pop)

template<typename T, typename U>
constexpr HRESULT vec2_narrow(U x, U y, AtlasEngine::vec2<T>& out) noexcept
{
    return api_narrow(x, out.x) | api_narrow(y, out.y);
}

#pragma region IRenderEngine

[[nodiscard]] HRESULT AtlasEngine::Invalidate(const til::rect* const psrRegion) noexcept
{
    //assert(psrRegion->Top < psrRegion->Bottom && psrRegion->Top >= 0 && psrRegion->Bottom <= _api.cellCount.y);

    // BeginPaint() protects against invalid out of bounds numbers.
    _api.invalidatedRows.x = std::min(_api.invalidatedRows.x, gsl::narrow_cast<u16>(psrRegion->Top));
    _api.invalidatedRows.y = std::max(_api.invalidatedRows.y, gsl::narrow_cast<u16>(psrRegion->Bottom));
    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::InvalidateCursor(const til::rect* const psrRegion) noexcept
{
    //assert(psrRegion->Left <= psrRegion->Right && psrRegion->Left >= 0 && psrRegion->Right <= _api.cellCount.x);
    //assert(psrRegion->Top <= psrRegion->Bottom && psrRegion->Top >= 0 && psrRegion->Bottom <= _api.cellCount.y);

    const auto left = gsl::narrow_cast<u16>(psrRegion->Left);
    const auto top = gsl::narrow_cast<u16>(psrRegion->Top);
    const auto right = gsl::narrow_cast<u16>(psrRegion->Right);
    const auto bottom = gsl::narrow_cast<u16>(psrRegion->Bottom);

    // BeginPaint() protects against invalid out of bounds numbers.
    _api.invalidatedCursorArea.left = std::min(_api.invalidatedCursorArea.left, left);
    _api.invalidatedCursorArea.top = std::min(_api.invalidatedCursorArea.top, top);
    _api.invalidatedCursorArea.right = std::max(_api.invalidatedCursorArea.right, right);
    _api.invalidatedCursorArea.bottom = std::max(_api.invalidatedCursorArea.bottom, bottom);
    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::InvalidateSystem(const til::rect* const prcDirtyClient) noexcept
{
    const auto top = prcDirtyClient->top / _api.fontMetrics.cellSize.y;
    const auto bottom = prcDirtyClient->bottom / _api.fontMetrics.cellSize.y;

    // BeginPaint() protects against invalid out of bounds numbers.
    til::rect rect;
    rect.Top = top;
    rect.Bottom = bottom;
    return Invalidate(&rect);
}

[[nodiscard]] HRESULT AtlasEngine::InvalidateSelection(const std::vector<til::rect>& rectangles) noexcept
{
    for (const auto& rect : rectangles)
    {
        // BeginPaint() protects against invalid out of bounds numbers.
        // TODO: rect can contain invalid out of bounds coordinates when the selection is being
        // dragged outside of the viewport (and the window begins scrolling automatically).
        _api.invalidatedRows.x = gsl::narrow_cast<u16>(std::min<int>(_api.invalidatedRows.x, std::max<int>(0, rect.Top)));
        _api.invalidatedRows.y = gsl::narrow_cast<u16>(std::max<int>(_api.invalidatedRows.y, std::max<int>(0, rect.Bottom)));
    }
    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::InvalidateScroll(const til::point* const pcoordDelta) noexcept
{
    const auto delta = pcoordDelta->Y;
    if (delta == 0)
    {
        return S_OK;
    }

    _api.scrollOffset = gsl::narrow_cast<i16>(clamp<int>(_api.scrollOffset + delta, i16min, i16max));

    // InvalidateScroll() is a "synchronous" API. Any Invalidate()s after
    // a InvalidateScroll() refer to the new viewport after the scroll.
    // --> We need to shift the current invalidation rectangles as well.

    _api.invalidatedCursorArea.top = gsl::narrow_cast<u16>(clamp<int>(_api.invalidatedCursorArea.top + delta, u16min, u16max));
    _api.invalidatedCursorArea.bottom = gsl::narrow_cast<u16>(clamp<int>(_api.invalidatedCursorArea.bottom + delta, u16min, u16max));

    if (delta < 0)
    {
        _api.invalidatedRows.x = gsl::narrow_cast<u16>(clamp<int>(_api.invalidatedRows.x + delta, u16min, u16max));
        _api.invalidatedRows.y = _api.cellCount.y;
    }
    else
    {
        _api.invalidatedRows.x = 0;
        _api.invalidatedRows.y = gsl::narrow_cast<u16>(clamp<int>(_api.invalidatedRows.y + delta, u16min, u16max));
    }

    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::InvalidateAll() noexcept
{
    _api.invalidatedRows = invalidatedRowsAll;
    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::InvalidateFlush(_In_ const bool /*circled*/, _Out_ bool* const pForcePaint) noexcept
{
    RETURN_HR_IF_NULL(E_INVALIDARG, pForcePaint);
    *pForcePaint = false;
    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::InvalidateTitle(const std::wstring_view proposedTitle) noexcept
{
    WI_SetFlag(_api.invalidations, ApiInvalidations::Title);
    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::NotifyNewText(const std::wstring_view newText) noexcept
{
    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::UpdateFont(const FontInfoDesired& fontInfoDesired, _Out_ FontInfo& fontInfo) noexcept
{
    return UpdateFont(fontInfoDesired, fontInfo, {}, {});
}

[[nodiscard]] HRESULT AtlasEngine::UpdateSoftFont(const gsl::span<const uint16_t> bitPattern, const til::size cellSize, const size_t centeringHint) noexcept
{
    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::UpdateDpi(const int dpi) noexcept
{
    u16 newDPI;
    RETURN_IF_FAILED(api_narrow(dpi, newDPI));

    if (_api.dpi != newDPI)
    {
        _api.dpi = newDPI;
        WI_SetFlag(_api.invalidations, ApiInvalidations::Font);
    }

    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::UpdateViewport(const til::inclusive_rect& srNewViewport) noexcept
{
    const u16x2 cellCount{
        gsl::narrow_cast<u16>(srNewViewport.Right - srNewViewport.Left + 1),
        gsl::narrow_cast<u16>(srNewViewport.Bottom - srNewViewport.Top + 1),
    };
    if (_api.cellCount != cellCount)
    {
        _api.cellCount = cellCount;
        WI_SetFlag(_api.invalidations, ApiInvalidations::Size);
    }
    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::GetProposedFont(const FontInfoDesired& fontInfoDesired, _Out_ FontInfo& fontInfo, const int dpi) noexcept
try
{
    // One day I'm going to implement GDI for AtlasEngine...
    // Until then this code is work in progress.
#if 0
    wil::unique_hfont hfont;

    // This block of code (for GDI fonts) is unfinished.
    if (fontInfoDesired.IsDefaultRasterFont())
    {
        hfont.reset(static_cast<HFONT>(GetStockObject(OEM_FIXED_FONT)));
        RETURN_HR_IF(E_FAIL, !hfont);
    }
    else if (requestedFaceName == DEFAULT_RASTER_FONT_FACENAME)
    {
        // GDI Windows Font Mapping reference:
        // https://msdn.microsoft.com/en-us/library/ms969909.aspx

        LOGFONTW lf;
        lf.lfHeight = -MulDiv(requestedSize.Y, dpi, 72);
        lf.lfWidth = 0;
        lf.lfEscapement = 0;
        lf.lfOrientation = 0;
        lf.lfWeight = requestedWeight;
        lf.lfItalic = FALSE;
        lf.lfUnderline = FALSE;
        lf.lfStrikeOut = FALSE;
        lf.lfCharSet = OEM_CHARSET;
        lf.lfOutPrecision = OUT_RASTER_PRECIS;
        lf.lfClipPrecision = CLIP_DEFAULT_PRECIS;
        lf.lfQuality = PROOF_QUALITY; // disables scaling for rasterized fonts
        lf.lfPitchAndFamily = FIXED_PITCH | FF_MODERN;
        // .size() only includes regular characters, but we also want to copy the trailing \0, so +1 it is.
        memcpy(&lf.lfFaceName[0], &DEFAULT_RASTER_FONT_FACENAME[0], sizeof(DEFAULT_RASTER_FONT_FACENAME));

        hfont.reset(CreateFontIndirectW(&lf));
        RETURN_HR_IF(E_FAIL, !hfont);
    }

    if (hfont)
    {
// wil::unique_any_t's constructor says: "should not be WI_NOEXCEPT (may forward to a throwing constructor)".
// The constructor we use by default doesn't throw.
#pragma warning(suppress : 26447) // The function is declared 'noexcept' but calls function '...' which may throw exceptions (f.6).
        wil::unique_hdc hdc{ CreateCompatibleDC(nullptr) };
        RETURN_HR_IF(E_FAIL, !hdc);

        DeleteObject(SelectObject(hdc.get(), hfont.get()));

        til::size sz;
        RETURN_HR_IF(E_FAIL, !GetTextExtentPoint32W(hdc.get(), L"M", 1, &sz));
        resultingCellSize.X = sz.cx;
        resultingCellSize.Y = sz.cy;
    }
#endif

    _resolveFontMetrics(nullptr, fontInfoDesired, fontInfo);
    return S_OK;
}
CATCH_RETURN()

[[nodiscard]] HRESULT AtlasEngine::GetDirtyArea(gsl::span<const til::rect>& area) noexcept
{
    area = gsl::span{ &_api.dirtyRect, 1 };
    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::GetFontSize(_Out_ til::size* pFontSize) noexcept
{
    RETURN_HR_IF_NULL(E_INVALIDARG, pFontSize);
    pFontSize->X = _api.fontMetrics.cellSize.x;
    pFontSize->Y = _api.fontMetrics.cellSize.y;
    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::IsGlyphWideByFont(const std::wstring_view glyph, _Out_ bool* const pResult) noexcept
{
    RETURN_HR_IF_NULL(E_INVALIDARG, pResult);

    wil::com_ptr<IDWriteTextLayout> textLayout;
    RETURN_IF_FAILED(_sr.dwriteFactory->CreateTextLayout(glyph.data(), gsl::narrow_cast<uint32_t>(glyph.size()), _getTextFormat(false, false), FLT_MAX, FLT_MAX, textLayout.addressof()));

    DWRITE_TEXT_METRICS metrics;
    RETURN_IF_FAILED(textLayout->GetMetrics(&metrics));

    *pResult = static_cast<unsigned int>(std::ceilf(metrics.width)) > _api.fontMetrics.cellSize.x;
    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::UpdateTitle(const std::wstring_view newTitle) noexcept
{
    return S_OK;
}

#pragma endregion

#pragma region DxRenderer

HRESULT AtlasEngine::Enable() noexcept
{
    return S_OK;
}

[[nodiscard]] std::wstring_view AtlasEngine::GetPixelShaderPath() noexcept
{
    return _api.customPixelShaderPath;
}

[[nodiscard]] bool AtlasEngine::GetRetroTerminalEffect() const noexcept
{
    return _api.useRetroTerminalEffect;
}

[[nodiscard]] float AtlasEngine::GetScaling() const noexcept
{
    return static_cast<float>(_api.dpi) / static_cast<float>(USER_DEFAULT_SCREEN_DPI);
}

[[nodiscard]] Microsoft::Console::Types::Viewport AtlasEngine::GetViewportInCharacters(const Types::Viewport& viewInPixels) const noexcept
{
    assert(_api.fontMetrics.cellSize.x != 0);
    assert(_api.fontMetrics.cellSize.y != 0);
    return Types::Viewport::FromDimensions(viewInPixels.Origin(), { viewInPixels.Width() / _api.fontMetrics.cellSize.x, viewInPixels.Height() / _api.fontMetrics.cellSize.y });
}

[[nodiscard]] Microsoft::Console::Types::Viewport AtlasEngine::GetViewportInPixels(const Types::Viewport& viewInCharacters) const noexcept
{
    assert(_api.fontMetrics.cellSize.x != 0);
    assert(_api.fontMetrics.cellSize.y != 0);
    return Types::Viewport::FromDimensions(viewInCharacters.Origin(), { viewInCharacters.Width() * _api.fontMetrics.cellSize.x, viewInCharacters.Height() * _api.fontMetrics.cellSize.y });
}

void AtlasEngine::SetAntialiasingMode(const D2D1_TEXT_ANTIALIAS_MODE antialiasingMode) noexcept
{
    const auto mode = gsl::narrow_cast<u8>(antialiasingMode);
    if (_api.antialiasingMode != mode)
    {
        _api.antialiasingMode = mode;
        _resolveTransparencySettings();
        WI_SetFlag(_api.invalidations, ApiInvalidations::Font);
    }
}

void AtlasEngine::SetCallback(std::function<void(HANDLE)> pfn) noexcept
{
    _api.swapChainChangedCallback = std::move(pfn);
}

void AtlasEngine::EnableTransparentBackground(const bool isTransparent) noexcept
{
    if (_api.enableTransparentBackground != isTransparent)
    {
        _api.enableTransparentBackground = isTransparent;
        _resolveTransparencySettings();
        WI_SetFlag(_api.invalidations, ApiInvalidations::SwapChain);
    }
}

void AtlasEngine::SetForceFullRepaintRendering(bool enable) noexcept
{
}

[[nodiscard]] HRESULT AtlasEngine::SetHwnd(const HWND hwnd) noexcept
{
    if (_api.hwnd != hwnd)
    {
        _api.hwnd = hwnd;
        WI_SetFlag(_api.invalidations, ApiInvalidations::SwapChain);
    }
    return S_OK;
}

void AtlasEngine::SetPixelShaderPath(std::wstring_view value) noexcept
{
    if (_api.customPixelShaderPath != value)
    {
        _api.customPixelShaderPath = value;
        _resolveTransparencySettings();
        WI_SetFlag(_api.invalidations, ApiInvalidations::Device);
    }
}

void AtlasEngine::SetRetroTerminalEffect(bool enable) noexcept
{
    if (_api.useRetroTerminalEffect != enable)
    {
        _api.useRetroTerminalEffect = enable;
        _resolveTransparencySettings();
        WI_SetFlag(_api.invalidations, ApiInvalidations::Device);
    }
}

void AtlasEngine::SetSelectionBackground(const COLORREF color, const float alpha) noexcept
{
    const u32 selectionColor = (color & 0xffffff) | gsl::narrow_cast<u32>(std::lroundf(alpha * 255.0f)) << 24;
    if (_api.selectionColor != selectionColor)
    {
        _api.selectionColor = selectionColor;
        WI_SetFlag(_api.invalidations, ApiInvalidations::Settings);
    }
}

void AtlasEngine::SetSoftwareRendering(bool enable) noexcept
{
    if (_api.useSoftwareRendering != enable)
    {
        _api.useSoftwareRendering = enable;
        WI_SetFlag(_api.invalidations, ApiInvalidations::Device);
    }
}

void AtlasEngine::SetWarningCallback(std::function<void(HRESULT)> pfn) noexcept
{
    _api.warningCallback = std::move(pfn);
}

[[nodiscard]] HRESULT AtlasEngine::SetWindowSize(const til::size pixels) noexcept
{
    u16x2 newSize;
    RETURN_IF_FAILED(vec2_narrow(pixels.cx, pixels.cy, newSize));

    // At the time of writing:
    // When Win+D is pressed, `TriggerRedrawCursor` is called and a render pass is initiated.
    // As conhost is in the background, GetClientRect will return {0,0} and we'll get called with {0,0}.
    // This isn't a valid value for _api.sizeInPixel and would crash _recreateSizeDependentResources().
    if (_api.sizeInPixel != newSize && newSize != u16x2{})
    {
        _api.sizeInPixel = newSize;
        WI_SetFlag(_api.invalidations, ApiInvalidations::Size);
    }

    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::UpdateFont(const FontInfoDesired& fontInfoDesired, FontInfo& fontInfo, const std::unordered_map<std::wstring_view, uint32_t>& features, const std::unordered_map<std::wstring_view, float>& axes) noexcept
{
    static constexpr std::array fallbackFaceNames{ static_cast<const wchar_t*>(nullptr), L"Consolas", L"Lucida Console", L"Courier New" };
    auto it = fallbackFaceNames.begin();
    const auto end = fallbackFaceNames.end();

    for (;;)
    {
        try
        {
            _updateFont(*it, fontInfoDesired, fontInfo, features, axes);
            return S_OK;
        }
        catch (...)
        {
            ++it;
            if (it == end)
            {
                RETURN_CAUGHT_EXCEPTION();
            }
            else
            {
                LOG_CAUGHT_EXCEPTION();
            }
        }
    }
}

void AtlasEngine::UpdateHyperlinkHoveredId(const uint16_t hoveredId) noexcept
{
    _api.hyperlinkHoveredId = hoveredId;
}

#pragma endregion

void AtlasEngine::_resolveTransparencySettings() noexcept
{
    // If the user asks for ClearType, but also for a transparent background
    // (which our ClearType shader doesn't simultaneously support)
    // then we need to sneakily force the renderer to grayscale AA.
    _api.realizedAntialiasingMode = _api.enableTransparentBackground && _api.antialiasingMode == D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE ? D2D1_TEXT_ANTIALIAS_MODE_GRAYSCALE : _api.antialiasingMode;
    // An opaque background allows us to use true "independent" flips. See AtlasEngine::_createSwapChain().
    // We can't enable them if custom shaders are specified, because it's unknown, whether they support opaque inputs.
    _api.backgroundOpaqueMixin = _api.enableTransparentBackground || !_api.customPixelShaderPath.empty() || _api.useRetroTerminalEffect ? 0x00000000 : 0xff000000;
}

void AtlasEngine::_updateFont(const wchar_t* faceName, const FontInfoDesired& fontInfoDesired, FontInfo& fontInfo, const std::unordered_map<std::wstring_view, uint32_t>& features, const std::unordered_map<std::wstring_view, float>& axes)
{
    std::vector<DWRITE_FONT_FEATURE> fontFeatures;
    if (!features.empty())
    {
        fontFeatures.reserve(features.size() + 3);

        // All of these features are enabled by default by DirectWrite.
        // If you want to (and can) peek into the source of DirectWrite
        // you can look for the "GenericDefaultGsubFeatures" and "GenericDefaultGposFeatures" arrays.
        // Gsub is for GetGlyphs() and Gpos for GetGlyphPlacements().
        //
        // GH#10774: Apparently specifying all of the features is just redundant.
        fontFeatures.emplace_back(DWRITE_FONT_FEATURE{ DWRITE_FONT_FEATURE_TAG_STANDARD_LIGATURES, 1 });
        fontFeatures.emplace_back(DWRITE_FONT_FEATURE{ DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_LIGATURES, 1 });
        fontFeatures.emplace_back(DWRITE_FONT_FEATURE{ DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_ALTERNATES, 1 });

        for (const auto& p : features)
        {
            if (p.first.size() == 4)
            {
                const auto s = p.first.data();
                switch (const auto tag = DWRITE_MAKE_FONT_FEATURE_TAG(s[0], s[1], s[2], s[3]))
                {
                case DWRITE_FONT_FEATURE_TAG_STANDARD_LIGATURES:
                    fontFeatures[0].parameter = p.second;
                    break;
                case DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_LIGATURES:
                    fontFeatures[1].parameter = p.second;
                    break;
                case DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_ALTERNATES:
                    fontFeatures[2].parameter = p.second;
                    break;
                default:
                    fontFeatures.emplace_back(DWRITE_FONT_FEATURE{ tag, p.second });
                    break;
                }
            }
        }
    }

    std::vector<DWRITE_FONT_AXIS_VALUE> fontAxisValues;
    if (!axes.empty())
    {
        fontAxisValues.reserve(axes.size() + 3);

        // AtlasEngine::_recreateFontDependentResources() relies on these fields to
        // exist in this particular order in order to create appropriate default axes.
        fontAxisValues.emplace_back(DWRITE_FONT_AXIS_VALUE{ DWRITE_FONT_AXIS_TAG_WEIGHT, -1.0f });
        fontAxisValues.emplace_back(DWRITE_FONT_AXIS_VALUE{ DWRITE_FONT_AXIS_TAG_ITALIC, -1.0f });
        fontAxisValues.emplace_back(DWRITE_FONT_AXIS_VALUE{ DWRITE_FONT_AXIS_TAG_SLANT, -1.0f });

        for (const auto& p : axes)
        {
            if (p.first.size() == 4)
            {
                const auto s = p.first.data();
                switch (const auto tag = DWRITE_MAKE_FONT_AXIS_TAG(s[0], s[1], s[2], s[3]))
                {
                case DWRITE_FONT_AXIS_TAG_WEIGHT:
                    fontAxisValues[0].value = p.second;
                    break;
                case DWRITE_FONT_AXIS_TAG_ITALIC:
                    fontAxisValues[1].value = p.second;
                    break;
                case DWRITE_FONT_AXIS_TAG_SLANT:
                    fontAxisValues[2].value = p.second;
                    break;
                default:
                    fontAxisValues.emplace_back(DWRITE_FONT_AXIS_VALUE{ tag, p.second });
                    break;
                }
            }
        }
    }

    const auto previousCellSize = _api.fontMetrics.cellSize;
    _resolveFontMetrics(faceName, fontInfoDesired, fontInfo, &_api.fontMetrics);
    _api.fontFeatures = std::move(fontFeatures);
    _api.fontAxisValues = std::move(fontAxisValues);

    WI_SetFlag(_api.invalidations, ApiInvalidations::Font);

    if (previousCellSize != _api.fontMetrics.cellSize)
    {
        WI_SetFlag(_api.invalidations, ApiInvalidations::Size);
    }
}

void AtlasEngine::_resolveFontMetrics(const wchar_t* requestedFaceName, const FontInfoDesired& fontInfoDesired, FontInfo& fontInfo, FontMetrics* fontMetrics) const
{
    const auto requestedFamily = fontInfoDesired.GetFamily();
    auto requestedWeight = fontInfoDesired.GetWeight();
    auto fontSize = fontInfoDesired.GetFontSize();
    auto requestedSize = fontInfoDesired.GetEngineSize();

    if (!requestedFaceName)
    {
        requestedFaceName = fontInfoDesired.GetFaceName().c_str();
        if (!requestedFaceName)
        {
            requestedFaceName = L"Consolas";
        }
    }
    if (!requestedSize.Y)
    {
        fontSize = 12.0f;
        requestedSize = { 0, 12 };
    }
    if (!requestedWeight)
    {
        requestedWeight = DWRITE_FONT_WEIGHT_NORMAL;
    }

    auto fontCollection = FontCache::GetCached();

    u32 index = 0;
    BOOL exists = false;
    THROW_IF_FAILED(fontCollection->FindFamilyName(requestedFaceName, &index, &exists));
    THROW_HR_IF(DWRITE_E_NOFONT, !exists);

    wil::com_ptr<IDWriteFontFamily> fontFamily;
    THROW_IF_FAILED(fontCollection->GetFontFamily(index, fontFamily.addressof()));

    wil::com_ptr<IDWriteFont> font;
    THROW_IF_FAILED(fontFamily->GetFirstMatchingFont(static_cast<DWRITE_FONT_WEIGHT>(requestedWeight), DWRITE_FONT_STRETCH_NORMAL, DWRITE_FONT_STYLE_NORMAL, font.addressof()));

    wil::com_ptr<IDWriteFontFace> fontFace;
    THROW_IF_FAILED(font->CreateFontFace(fontFace.addressof()));

    DWRITE_FONT_METRICS metrics;
    fontFace->GetMetrics(&metrics);

    // According to Wikipedia:
    // > One em was traditionally defined as the width of the capital 'M' in the current typeface and point size,
    // > because the 'M' was commonly cast the full-width of the square blocks [...] which are used in printing presses.
    // Even today M is often the widest character in a font that supports ASCII.
    // In the future a more robust solution could be written, until then this simple solution works for most cases.
    static constexpr u32 codePoint = L'M';
    u16 glyphIndex;
    THROW_IF_FAILED(fontFace->GetGlyphIndicesW(&codePoint, 1, &glyphIndex));

    DWRITE_GLYPH_METRICS glyphMetrics;
    THROW_IF_FAILED(fontFace->GetDesignGlyphMetrics(&glyphIndex, 1, &glyphMetrics));

    // Point sizes are commonly treated at a 72 DPI scale
    // (including by OpenType), whereas DirectWrite uses 96 DPI.
    // Since we want the height in px we multiply by the display's DPI.
    const auto fontSizeInDIP = fontSize / 72.0f * 96.0f;
    const auto fontSizeInPx = fontSize / 72.0f * _api.dpi;

    const auto designUnitsPerPx = fontSizeInPx / static_cast<float>(metrics.designUnitsPerEm);
    const auto ascent = static_cast<float>(metrics.ascent) * designUnitsPerPx;
    const auto descent = static_cast<float>(metrics.descent) * designUnitsPerPx;
    const auto underlinePosition = static_cast<float>(-metrics.underlinePosition) * designUnitsPerPx;
    const auto underlineThickness = static_cast<float>(metrics.underlineThickness) * designUnitsPerPx;
    const auto strikethroughPosition = static_cast<float>(-metrics.strikethroughPosition) * designUnitsPerPx;
    const auto strikethroughThickness = static_cast<float>(metrics.strikethroughThickness) * designUnitsPerPx;

    const auto advanceWidth = static_cast<float>(glyphMetrics.advanceWidth) * designUnitsPerPx;

    // NOTE: Line-gaps shouldn't be taken into account for lineHeight calculations.
    // Terminals don't really have "gaps" between lines and instead the expectation
    // is that two full block characters above each other don't leave any gaps
    // between the lines. "Terminus TTF" for instance sets a line-gap of 90 units
    // even though its font bitmap only covers the ascend/descend height.
    const auto baseline = std::roundf(ascent);
    const auto lineHeight = std::roundf(baseline + descent);
    const auto underlinePos = std::roundf(baseline + underlinePosition);
    const auto underlineWidth = std::max(1.0f, std::roundf(underlineThickness));
    const auto strikethroughPos = std::roundf(baseline + strikethroughPosition);
    const auto strikethroughWidth = std::max(1.0f, std::roundf(strikethroughThickness));
    const auto thinLineWidth = std::max(1.0f, std::roundf(underlineThickness / 2.0f));

    // For double underlines we loosely follow what Word does:
    // 1. The lines are half the width of an underline (= thinLineWidth)
    // 2. Ideally the bottom line is aligned with the bottom of the underline
    // 3. The top underline is vertically in the middle between baseline and ideal bottom underline
    // 4. If the top line gets too close to the baseline the underlines are shifted downwards
    // 5. The minimum gap between the two lines appears to be similar to Tex (1.2pt)
    // (Additional notes below.)

    // 2.
    auto doubleUnderlinePosBottom = underlinePos + underlineWidth - thinLineWidth;
    // 3. Since we don't align the center of our two lines, but rather the top borders
    //    we need to subtract half a line width from our center point.
    auto doubleUnderlinePosTop = std::roundf((baseline + doubleUnderlinePosBottom - thinLineWidth) / 2.0f);
    // 4.
    doubleUnderlinePosTop = std::max(doubleUnderlinePosTop, baseline + thinLineWidth);
    // 5. The gap is only the distance _between_ the lines, but we need the distance from the
    //    top border of the top and bottom lines, which includes an additional line width.
    const auto doubleUnderlineGap = std::max(1.0f, std::roundf(1.2f / 72.0f * _api.dpi));
    doubleUnderlinePosBottom = std::max(doubleUnderlinePosBottom, doubleUnderlinePosTop + doubleUnderlineGap + thinLineWidth);
    // Our cells can't overlap each other so we additionally clamp the bottom line to be inside the cell boundaries.
    doubleUnderlinePosBottom = std::min(doubleUnderlinePosBottom, lineHeight - thinLineWidth);

    const auto cellWidth = gsl::narrow<u16>(std::lroundf(advanceWidth));
    const auto cellHeight = gsl::narrow<u16>(lineHeight);

    {
        til::size coordSize;
        coordSize.X = cellWidth;
        coordSize.Y = cellHeight;

        if (requestedSize.X == 0)
        {
            // The coordSizeUnscaled parameter to SetFromEngine is used for API functions like GetConsoleFontSize.
            // Since clients expect that settings the font height to Y yields back a font height of Y,
            // we're scaling the X relative/proportional to the actual cellWidth/cellHeight ratio.
            requestedSize.X = gsl::narrow_cast<til::CoordType>(std::lroundf(fontSize / cellHeight * cellWidth));
        }

        fontInfo.SetFromEngine(requestedFaceName, requestedFamily, requestedWeight, false, coordSize, requestedSize);
    }

    if (fontMetrics)
    {
        std::wstring fontName{ requestedFaceName };
        const auto baselineU16 = gsl::narrow_cast<u16>(baseline);
        const auto fontWeightU16 = gsl::narrow_cast<u16>(requestedWeight);
        const auto underlinePosU16 = gsl::narrow_cast<u16>(underlinePos);
        const auto underlineWidthU16 = gsl::narrow_cast<u16>(underlineWidth);
        const auto strikethroughPosU16 = gsl::narrow_cast<u16>(strikethroughPos);
        const auto strikethroughWidthU16 = gsl::narrow_cast<u16>(strikethroughWidth);
        const auto doubleUnderlinePosTopU16 = gsl::narrow_cast<u16>(doubleUnderlinePosTop);
        const auto doubleUnderlinePosBottomU16 = gsl::narrow_cast<u16>(doubleUnderlinePosBottom);
        const auto thinLineWidthU16 = gsl::narrow_cast<u16>(thinLineWidth);

        // NOTE: From this point onward no early returns or throwing code should exist,
        // as we might cause _api to be in an inconsistent state otherwise.

        fontMetrics->fontCollection = std::move(fontCollection);
        fontMetrics->fontName = std::move(fontName);
        fontMetrics->fontSizeInDIP = fontSizeInDIP;
        fontMetrics->advanceScale = cellWidth / advanceWidth;
        fontMetrics->baselineInDIP = baseline / static_cast<f32>(_api.dpi) * 96.0f;
        fontMetrics->baseline = baselineU16;
        fontMetrics->cellSize = { cellWidth, cellHeight };
        fontMetrics->fontWeight = fontWeightU16;
        fontMetrics->underlinePos = underlinePosU16;
        fontMetrics->underlineWidth = underlineWidthU16;
        fontMetrics->strikethroughPos = strikethroughPosU16;
        fontMetrics->strikethroughWidth = strikethroughWidthU16;
        fontMetrics->doubleUnderlinePos = { doubleUnderlinePosTopU16, doubleUnderlinePosBottomU16 };
        fontMetrics->thinLineWidth = thinLineWidthU16;
    }
}
