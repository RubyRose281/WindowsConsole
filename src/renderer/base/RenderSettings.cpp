// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

#include "precomp.h"

#include "../inc/RenderSettings.hpp"
#include "../base/renderer.hpp"
#include "../../types/inc/ColorFix.hpp"
#include "../../types/inc/colorTable.hpp"

#include <DirectXCollision.h>

using namespace Microsoft::Console::Render;
using Microsoft::Console::Utils::InitializeColorTable;

static constexpr size_t AdjustedFgIndex{ 16 };
static constexpr size_t AdjustedBgIndex{ 17 };

RenderSettings::RenderSettings() noexcept
{
    InitializeColorTable(_colorTable);

    SetColorTableEntry(TextColor::DEFAULT_FOREGROUND, INVALID_COLOR);
    SetColorTableEntry(TextColor::DEFAULT_BACKGROUND, INVALID_COLOR);
    SetColorTableEntry(TextColor::FRAME_FOREGROUND, INVALID_COLOR);
    SetColorTableEntry(TextColor::FRAME_BACKGROUND, INVALID_COLOR);
    SetColorTableEntry(TextColor::CURSOR_COLOR, INVALID_COLOR);

    SetColorAliasIndex(ColorAlias::DefaultForeground, TextColor::DARK_WHITE);
    SetColorAliasIndex(ColorAlias::DefaultBackground, TextColor::DARK_BLACK);
    SetColorAliasIndex(ColorAlias::FrameForeground, TextColor::FRAME_FOREGROUND);
    SetColorAliasIndex(ColorAlias::FrameBackground, TextColor::FRAME_BACKGROUND);
}

// Routine Description:
// - Updates the specified render mode.
// Arguments:
// - mode - The render mode to change.
// - enabled - Set to true to enable the mode, false to disable it.
void RenderSettings::SetRenderMode(const Mode mode, const bool enabled) noexcept
{
    _renderMode.set(mode, enabled);
    // If blinking is disabled, make sure blinking content is not faint.
    if (mode == Mode::BlinkAllowed && !enabled)
    {
        _blinkShouldBeFaint = false;
    }
}

// Routine Description:
// - Retrieves the specified render mode.
// Arguments:
// - mode - The render mode to query.
// Return Value:
// - True if the mode is enabled. False if disabled.
bool RenderSettings::GetRenderMode(const Mode mode) const noexcept
{
    return _renderMode.test(mode);
}

// Routine Description:
// - Returns a reference to the active color table array.
const std::array<COLORREF, TextColor::TABLE_SIZE>& RenderSettings::GetColorTable() const noexcept
{
    return _colorTable;
}

// Routine Description:
// - Resets the first 16 color table entries with default values.
void RenderSettings::ResetColorTable() noexcept
{
    InitializeColorTable({ _colorTable.data(), 16 });
}

// Routine Description:
// - Creates the adjusted color array, which contains the possible foreground colors,
//   adjusted for perceivability
// - The adjusted color array is 2-d, and effectively maps a background and foreground
//   color pair to the adjusted foreground for that color pair
void RenderSettings::MakeAdjustedColorArray() noexcept
{
    // The color table has 16 colors, but the adjusted color table needs to be 18
    // to include the default background and default foreground colors
    std::array<COLORREF, 18> colorTableWithDefaults;
    std::copy_n(std::begin(_colorTable), 16, std::begin(colorTableWithDefaults));
    colorTableWithDefaults[AdjustedFgIndex] = GetColorAlias(ColorAlias::DefaultForeground);
    colorTableWithDefaults[AdjustedBgIndex] = GetColorAlias(ColorAlias::DefaultBackground);

    for (auto fgIndex = 0; fgIndex < 18; ++fgIndex)
    {
        const auto fg = til::at(colorTableWithDefaults, fgIndex);
        for (auto bgIndex = 0; bgIndex < 18; ++bgIndex)
        {
            if (fgIndex == bgIndex)
            {
                _adjustedForegroundColors[bgIndex][fgIndex] = fg;
            }
            else
            {
                const auto bg = til::at(colorTableWithDefaults, bgIndex);
                _adjustedForegroundColors[bgIndex][fgIndex] = ColorFix::GetPerceivableColor(fg, bg);
            }
        }
    }
}

// Routine Description:
// - Updates the given index in the color table to a new value.
// Arguments:
// - tableIndex - The index of the color to update.
// - color - The new COLORREF to use as that color table value.
void RenderSettings::SetColorTableEntry(const size_t tableIndex, const COLORREF color)
{
    _colorTable.at(tableIndex) = color;
}

// Routine Description:
// - Retrieves the value in the color table at the specified index.
// Arguments:
// - tableIndex - The index of the color to retrieve.
// Return Value:
// - The COLORREF value for the color at that index in the table.
COLORREF RenderSettings::GetColorTableEntry(const size_t tableIndex) const
{
    return _colorTable.at(tableIndex);
}

// Routine Description:
// - Sets the position in the color table for the given color alias and updates the color.
// Arguments:
// - alias - The color alias to update.
// - tableIndex - The new position of the alias in the color table.
// - color - The new COLORREF to assign to that alias.
void RenderSettings::SetColorAlias(const ColorAlias alias, const size_t tableIndex, const COLORREF color)
{
    SetColorAliasIndex(alias, tableIndex);
    SetColorTableEntry(tableIndex, color);
}

// Routine Description:
// - Retrieves the value in the color table of the given color alias.
// Arguments:
// - alias - The color alias to retrieve.
// Return Value:
// - The COLORREF value of the alias.
COLORREF RenderSettings::GetColorAlias(const ColorAlias alias) const
{
    return GetColorTableEntry(GetColorAliasIndex(alias));
}

// Routine Description:
// - Sets the position in the color table for the given color alias.
// Arguments:
// - alias - The color alias to update.
// - tableIndex - The new position of the alias in the color table.
void RenderSettings::SetColorAliasIndex(const ColorAlias alias, const size_t tableIndex) noexcept
{
    if (tableIndex < TextColor::TABLE_SIZE)
    {
        gsl::at(_colorAliasIndices, static_cast<size_t>(alias)) = tableIndex;
    }
}

// Routine Description:
// - Retrieves the position in the color table of the given color alias.
// Arguments:
// - alias - The color alias to retrieve.
// Return Value:
// - The position in the color table where the color is stored.
size_t RenderSettings::GetColorAliasIndex(const ColorAlias alias) const noexcept
{
    return gsl::at(_colorAliasIndices, static_cast<size_t>(alias));
}

#pragma warning(suppress : 4505)
inline float rsqrtf(float f)
{
#ifdef _M_ARM
    // According to (https://doi.org/10.3390/computation9020021):
    // > Modified Fast Inverse Square Root and Square Root Approximation Algorithms: The Method of Switching Magic Constants
    // the relative error of FRSQRTE is [-3.0354e-3,3.2768e-3], which would lead
    // to rounding errors after multiplication with 255. A single round of FRSQRTS
    // supposedly improves the relative error to [-1.6183e-5,1.3127e-7].
    const auto e = vrsqrte_f32(f);
    e = vmul_f32(vrsqrts_f32(vmul_f32(e, e), f), e);
#else
    return _mm_cvtss_f32(_mm_rsqrt_ss(_mm_set_ss(f)));
#endif
}

#pragma warning(suppress : 4505)
static COLORREF redmean_nudge(COLORREF c1, COLORREF c2)
{
    long r1 = GetRValue(c1), g1 = GetGValue(c1), b1 = GetBValue(c1);
    long r2 = GetRValue(c2), g2 = GetGValue(c2), b2 = GetBValue(c2);
    long dr = r2 - r1;
    long dg = g2 - g1;
    long db = b2 - b1;
    long rmean = (r1 + r2) / 2;
    long d = (512 + rmean) * dr * dr + 4 * 256 * dg * dg + (767 - rmean) * db * db;
    if (d)
    {
        float dist = sqrtf(584970.0f) * rsqrtf(gsl::narrow_cast<float>(d));
        if (dist > 1.0f)
        {
            dr = std::clamp<long>(r1 + lroundf(dr * dist), 0, 255);
            dg = std::clamp<long>(g1 + lroundf(dg * dist), 0, 255);
            db = std::clamp<long>(b1 + lroundf(db * dist), 0, 255);
            c2 = RGB(dr, dg, db);
        }
    }
    return c2;
}

#pragma warning(suppress : 4505)
inline void XM_CALLCONV XMVectorStoreUInt4Clamped(DirectX::XMUINT4* pDestination, DirectX::FXMVECTOR V, uint32_t min, uint32_t max) {
    assert(pDestination);
    assert(min <= max);

#if defined(_XM_NO_INTRINSICS_)
    pDestination->x = std::max(min, std::min(max, static_cast<uint32_t>(V.vector4_f32[0])));
    pDestination->y = std::max(min, std::min(max, static_cast<uint32_t>(V.vector4_f32[1])));
    pDestination->z = std::max(min, std::min(max, static_cast<uint32_t>(V.vector4_f32[2])));
    pDestination->w = std::max(min, std::min(max, static_cast<uint32_t>(V.vector4_f32[3])));
#elif defined(_XM_ARM_NEON_INTRINSICS_)
    uint32x4_t v = vcvtq_u32_f32(V);
    v = vmaxq_u32(vdupq_n_u32(min), v);
    v = vminq_u32(vdupq_n_u32(max), v);
    vst1q_u32(reinterpret_cast<uint32_t*>(pDestination), v);
#elif defined(_XM_SSE_INTRINSICS_)
    __m128i v = _mm_cvttps_epi32(V);
    v = _mm_max_epi32(_mm_set1_epi32(std::bit_cast<int>(min)), v);
    v = _mm_min_epi32(_mm_set1_epi32(std::bit_cast<int>(max)), v);
    _mm_storeu_si128(reinterpret_cast<__m128i*>(pDestination), v);
#endif
}

inline DirectX::XMVECTOR XM_CALLCONV colorrefToRgb(COLORREF c) {
    auto v = DirectX::XMVectorSet(static_cast<float>(GetRValue(c)), static_cast<float>(GetGValue(c)), static_cast<float>(GetBValue(c)), 0.0f);
    return DirectX::XMVectorMultiply(v, DirectX::XMVectorReplicate(1.0f / 255.0f));
}

inline COLORREF XM_CALLCONV rgbToColorref(DirectX::FXMVECTOR rgb) {
    auto v = DirectX::XMVectorMultiply(rgb, DirectX::XMVectorReplicate(255.0f));
    DirectX::XMUINT4 vals;
    XMVectorStoreUInt4Clamped(&vals, v, 0, 255);
    return RGB(vals.x, vals.y, vals.z);
}

static COLORREF wcag2_adjust(COLORREF c1, COLORREF c2) {
    auto v1 = colorrefToRgb(c1);
    auto v2 = colorrefToRgb(c2);
    auto xyz1 = DirectX::XMColorSRGBToXYZ(v1);
    auto xyz2 = DirectX::XMColorSRGBToXYZ(v2);
    auto l1 = DirectX::XMVectorGetY(xyz1);
    auto l2 = DirectX::XMVectorGetY(xyz2);

    if ((std::max(l1, l2) + 0.05f) / (std::min(l1, l2) + 0.05f) < 4.5f) {
        auto h2 = DirectX::XMColorRGBToHSL(v2);
        auto l2new1 = 2.5f * l1 + 0.175f;
        auto l2new2 = (1.0f / 2.5f) * l1 - (0.175f / 2.5f);

        if (l2new1 <= 1) {
            h2 = DirectX::XMVectorSetZ(h2, l2new1);
        } else if (l2new2 >= 0) {
            h2 = DirectX::XMVectorSetZ(h2, l2new2);
        }

        v2 = DirectX::XMColorHSLToRGB(h2);
        c2 = rgbToColorref(v2);
    }

    return c2;
}

// Routine Description:
// - Calculates the RGB colors of a given text attribute, using the current
//   color table configuration and active render settings.
// Arguments:
// - attr - The TextAttribute to retrieve the colors for.
// Return Value:
// - The color values of the attribute's foreground and background.
std::pair<COLORREF, COLORREF> RenderSettings::GetAttributeColors(const TextAttribute& attr) const noexcept
{
    _blinkIsInUse = _blinkIsInUse || attr.IsBlinking();

    const auto fgTextColor = attr.GetForeground();
    const auto bgTextColor = attr.GetBackground();

    const auto defaultFgIndex = GetColorAliasIndex(ColorAlias::DefaultForeground);
    const auto defaultBgIndex = GetColorAliasIndex(ColorAlias::DefaultBackground);

    const auto brightenFg = attr.IsIntense() && GetRenderMode(Mode::IntenseIsBright);
    const auto dimFg = attr.IsFaint() || (_blinkShouldBeFaint && attr.IsBlinking());
    const auto swapFgAndBg = attr.IsReverseVideo() ^ GetRenderMode(Mode::ScreenReversed);

    // We want to nudge the foreground color to make it more perceivable only for the
    // default color pairs within the color table
    {
        auto fg = fgTextColor.GetColor(_colorTable, defaultFgIndex, brightenFg);
        auto bg = bgTextColor.GetColor(_colorTable, defaultBgIndex);

        if (dimFg)
        {
            fg = (fg >> 1) & 0x7F7F7F; // Divide foreground color components by two.
        }
        if (swapFgAndBg)
        {
            std::swap(fg, bg);
        }
        if (attr.IsInvisible())
        {
            fg = bg;
        }
        else
        {
            // C:\Users\lhecker\projects\rainbowbench\cmake-build-release\rainbowbench.exe 120
            //fg = ColorFix::GetPerceivableColor(fg, bg);
            fg = wcag2_adjust(bg, fg);
        }

        return { fg, bg };
    }
}

// Routine Description:
// - Calculates the RGBA colors of a given text attribute, using the current
//   color table configuration and active render settings. This differs from
//   GetAttributeColors in that it also sets the alpha color components.
// Arguments:
// - attr - The TextAttribute to retrieve the colors for.
// Return Value:
// - The color values of the attribute's foreground and background.
std::pair<COLORREF, COLORREF> RenderSettings::GetAttributeColorsWithAlpha(const TextAttribute& attr) const noexcept
{
    auto [fg, bg] = GetAttributeColors(attr);

    fg |= 0xff000000;
    // We only care about alpha for the default BG (which enables acrylic)
    // If the bg isn't the default bg color, or reverse video is enabled, make it fully opaque.
    if (!attr.BackgroundIsDefault() || (attr.IsReverseVideo() ^ GetRenderMode(Mode::ScreenReversed)) || attr.IsInvisible())
    {
        bg |= 0xff000000;
    }

    return { fg, bg };
}

// Routine Description:
// - Increments the position in the blink cycle, toggling the blink rendition
//   state on every second call, potentially triggering a redraw of the given
//   renderer if there are blinking cells currently in view.
// Arguments:
// - renderer: the renderer that will be redrawn.
void RenderSettings::ToggleBlinkRendition(Renderer& renderer) noexcept
try
{
    if (GetRenderMode(Mode::BlinkAllowed))
    {
        // This method is called with the frequency of the cursor blink rate,
        // but we only want our cells to blink at half that frequency. We thus
        // have a blink cycle that loops through four phases...
        _blinkCycle = (_blinkCycle + 1) % 4;
        // ... and two of those four render the blink attributes as faint.
        _blinkShouldBeFaint = _blinkCycle >= 2;
        // Every two cycles (when the state changes), we need to trigger a
        // redraw, but only if there are actually blink attributes in use.
        if (_blinkIsInUse && _blinkCycle % 2 == 0)
        {
            // We reset the _blinkIsInUse flag before redrawing, so we can
            // get a fresh assessment of the current blink attribute usage.
            _blinkIsInUse = false;
            renderer.TriggerRedrawAll();
        }
    }
}
CATCH_LOG()
