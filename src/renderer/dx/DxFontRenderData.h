// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

#pragma once

#include "../../renderer/inc/FontInfoDesired.hpp"
#include "BoxDrawingEffect.h"

#include <dwrite.h>
#include <dwrite_1.h>
#include <dwrite_2.h>
#include <dwrite_3.h>

#include <wrl.h>

namespace Microsoft::Console::Render
{
    enum class AxisTagPresence : BYTE
    {
        None = 0x00,
        Weight = 0x01,
        Width = 0x02,
        Italic = 0x04,
        Slant = 0x08,
    };
    DEFINE_ENUM_FLAG_OPERATORS(AxisTagPresence);

    class DxFontRenderData
    {
    public:
        struct LineMetrics
        {
            float gridlineWidth;
            float underlineOffset;
            float underlineOffset2;
            float underlineWidth;
            float strikethroughOffset;
            float strikethroughWidth;
        };

        DxFontRenderData(::Microsoft::WRL::ComPtr<IDWriteFactory1> dwriteFactory) noexcept;

        // DirectWrite text analyzer from the factory
        [[nodiscard]] Microsoft::WRL::ComPtr<IDWriteTextAnalyzer1> Analyzer();

        [[nodiscard]] Microsoft::WRL::ComPtr<IDWriteFontFallback> SystemFontFallback();

        [[nodiscard]] til::size GlyphCell() const noexcept;
        [[nodiscard]] LineMetrics GetLineMetrics() const noexcept;

        // The weight of default font
        [[nodiscard]] DWRITE_FONT_WEIGHT DefaultFontWeight() const noexcept;

        // The style of default font
        [[nodiscard]] static DWRITE_FONT_STYLE DefaultFontStyle() noexcept;

        // The stretch of default font
        [[nodiscard]] static DWRITE_FONT_STRETCH DefaultFontStretch() noexcept;

        // The font features of the default font
        [[nodiscard]] const std::vector<DWRITE_FONT_FEATURE>& DefaultFontFeatures() const noexcept;

        // The DirectWrite format object representing the size and other text properties to be applied (by default)
        [[nodiscard]] IDWriteTextFormat* DefaultTextFormat() const noexcept;

        // The DirectWrite font face to use while calculating layout (by default)
        [[nodiscard]] IDWriteFontFace1* DefaultFontFace() const noexcept;

        // Box drawing scaling effects that are cached for the base font across layouts
        [[nodiscard]] IBoxDrawingEffect* DefaultBoxDrawingEffect();

        // The attributed variants of the format object representing the size and other text properties
        [[nodiscard]] IDWriteTextFormat* TextFormatWithAttribute(DWRITE_FONT_WEIGHT weight, DWRITE_FONT_STYLE style) const noexcept;

        // The attributed variants of the font face to use while calculating layout
        [[nodiscard]] IDWriteFontFace1* FontFaceWithAttribute(DWRITE_FONT_WEIGHT weight, DWRITE_FONT_STYLE style) const noexcept;

        [[nodiscard]] const std::vector<DWRITE_FONT_AXIS_VALUE>& GetAxisVector(DWRITE_FONT_WEIGHT weight, DWRITE_FONT_STYLE style) const noexcept;

        [[nodiscard]] HRESULT UpdateFont(const FontInfoDesired& desired, FontInfo& fiFontInfo, const int dpi, const std::unordered_map<std::wstring_view, uint32_t>& features = {}, const std::unordered_map<std::wstring_view, float>& axes = {}) noexcept;

        [[nodiscard]] static HRESULT s_CalculateBoxEffect(IDWriteTextFormat* format, size_t widthPixels, IDWriteFontFace1* face, float fontScale, IBoxDrawingEffect** effect) noexcept;

        bool DidUserSetFeatures() const noexcept;
        bool DidUserSetAxes() const noexcept;

    private:
        void _RefreshUserLocaleName();
        void _BuildFontRenderData(const FontInfoDesired& desired, FontInfo& actual, const int dpi);
        ::Microsoft::WRL::ComPtr<IDWriteFont> _ResolveFontWithFallback(std::wstring familyName, DWRITE_FONT_WEIGHT weight, DWRITE_FONT_STYLE style);
        [[nodiscard]] Microsoft::WRL::ComPtr<IDWriteFont> _FindFont(const wchar_t* familyName, DWRITE_FONT_WEIGHT weight, DWRITE_FONT_STYLE style);
        [[nodiscard]] IDWriteFontCollection* _NearbyCollection();
        [[nodiscard]] static std::vector<std::filesystem::path> s_GetNearbyFonts();

        ::Microsoft::WRL::ComPtr<IDWriteFontFace1> _fontFaces[2][2];
        ::Microsoft::WRL::ComPtr<IDWriteTextFormat> _textFormats[2][2];
        std::vector<DWRITE_FONT_AXIS_VALUE> _textFormatAxes[2][2];
        std::vector<DWRITE_FONT_FEATURE> _featureVector;

        ::Microsoft::WRL::ComPtr<IBoxDrawingEffect> _boxDrawingEffect;
        ::Microsoft::WRL::ComPtr<IDWriteFontFallback> _systemFontFallback;
        ::Microsoft::WRL::ComPtr<IDWriteFontCollection> _nearbyFontCollection;
        ::Microsoft::WRL::ComPtr<IDWriteFactory1> _dwriteFactory;
        ::Microsoft::WRL::ComPtr<IDWriteTextAnalyzer1> _dwriteTextAnalyzer;

        wil::unique_process_heap_string _userLocaleName;
        til::size _glyphCell;
        DWRITE_LINE_SPACING _lineSpacing;
        LineMetrics _lineMetrics;
        DWRITE_FONT_WEIGHT _userFontWeight;
        float _fontSize;
    };
}
