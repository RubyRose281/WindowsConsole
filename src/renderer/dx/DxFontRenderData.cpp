// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

#include "precomp.h"

#include "DxFontRenderData.h"

#include "unicode.hpp"

#include <VersionHelpers.h>

static constexpr float POINTS_PER_INCH = 72.0f;
static constexpr const wchar_t* FALLBACK_FONT_FACES[] = { L"Consolas", L"Lucida Console", L"Courier New" };
static constexpr wchar_t FALLBACK_LOCALE[]{ L"en-us" };

using namespace Microsoft::Console::Render;

DxFontRenderData::DxFontRenderData(::Microsoft::WRL::ComPtr<IDWriteFactory1> dwriteFactory) noexcept :
    _dwriteFactory(dwriteFactory),
    _fontSize{},
    _lineMetrics{},
    _lineSpacing{}
{
}

[[nodiscard]] Microsoft::WRL::ComPtr<IDWriteTextAnalyzer1> DxFontRenderData::Analyzer()
{
    if (!_dwriteTextAnalyzer)
    {
        Microsoft::WRL::ComPtr<IDWriteTextAnalyzer> analyzer;
        THROW_IF_FAILED(_dwriteFactory->CreateTextAnalyzer(&analyzer));
        THROW_IF_FAILED(analyzer.As(&_dwriteTextAnalyzer));
    }

    return _dwriteTextAnalyzer;
}

[[nodiscard]] Microsoft::WRL::ComPtr<IDWriteFontFallback> DxFontRenderData::SystemFontFallback()
{
    if (!_systemFontFallback)
    {
        ::Microsoft::WRL::ComPtr<IDWriteFactory2> factory2;
        THROW_IF_FAILED(_dwriteFactory.As(&factory2));
        factory2->GetSystemFontFallback(&_systemFontFallback);
    }

    return _systemFontFallback;
}

void DxFontRenderData::_RefreshUserLocaleName()
{
    std::array<wchar_t, LOCALE_NAME_MAX_LENGTH> buffer;
    const wchar_t* localeName = buffer.data();
    auto length = GetUserDefaultLocaleName(buffer.data(), gsl::narrow_cast<int>(buffer.size()));

    if (length <= 0)
    {
        localeName = &FALLBACK_LOCALE[0];
        length = sizeof(FALLBACK_LOCALE);
    }

    // length is including the trailing null byte.
    // See GetUserDefaultLocaleName()'s docs.
    _userLocaleName = wil::make_process_heap_string_nothrow(localeName, length - 1);
}

[[nodiscard]] til::size DxFontRenderData::GlyphCell() const noexcept
{
    return _glyphCell;
}

[[nodiscard]] DxFontRenderData::LineMetrics DxFontRenderData::GetLineMetrics() const noexcept
{
    return _lineMetrics;
}

[[nodiscard]] DWRITE_FONT_WEIGHT DxFontRenderData::DefaultFontWeight() const noexcept
{
    return _userFontWeight;
}

[[nodiscard]] DWRITE_FONT_STYLE DxFontRenderData::DefaultFontStyle() noexcept
{
    return DWRITE_FONT_STYLE_NORMAL;
}

[[nodiscard]] DWRITE_FONT_STRETCH DxFontRenderData::DefaultFontStretch() noexcept
{
    return DWRITE_FONT_STRETCH_NORMAL;
}

[[nodiscard]] const std::vector<DWRITE_FONT_FEATURE>& DxFontRenderData::DefaultFontFeatures() const noexcept
{
    return _featureVector;
}

[[nodiscard]] IDWriteTextFormat* DxFontRenderData::DefaultTextFormat() const noexcept
{
    return _textFormats[0][0].Get();
}

[[nodiscard]] IDWriteFontFace1* DxFontRenderData::DefaultFontFace() const noexcept
{
    return _fontFaces[0][0].Get();
}

[[nodiscard]] IBoxDrawingEffect* DxFontRenderData::DefaultBoxDrawingEffect()
{
    if (!_boxDrawingEffect)
    {
        // Calculate and cache the box effect for the base font. Scale is 1.0f because the base font is exactly the scale we want already.
        THROW_IF_FAILED(s_CalculateBoxEffect(DefaultTextFormat(), _glyphCell.width(), DefaultFontFace(), 1.0f, &_boxDrawingEffect));
    }

    return _boxDrawingEffect.Get();
}

[[nodiscard]] IDWriteTextFormat* DxFontRenderData::TextFormatWithAttribute(DWRITE_FONT_WEIGHT weight, DWRITE_FONT_STYLE style) const noexcept
{
    return _textFormats[style != DWRITE_FONT_STYLE_NORMAL][weight != _userFontWeight].Get();
}

[[nodiscard]] IDWriteFontFace1* DxFontRenderData::FontFaceWithAttribute(DWRITE_FONT_WEIGHT weight, DWRITE_FONT_STYLE style) const noexcept
{
    return _fontFaces[style != DWRITE_FONT_STYLE_NORMAL][weight != _userFontWeight].Get();
}

const std::vector<DWRITE_FONT_AXIS_VALUE>& DxFontRenderData::GetAxisVector(DWRITE_FONT_WEIGHT weight, DWRITE_FONT_STYLE style) const noexcept
{
    return _textFormatAxes[style != DWRITE_FONT_STYLE_NORMAL][weight != _userFontWeight];
}

// Routine Description:
// - Updates the font used for drawing
// Arguments:
// - desired - Information specifying the font that is requested
// - actual - Filled with the nearest font actually chosen for drawing
// - dpi - The DPI of the screen
// Return Value:
// - S_OK or relevant DirectX error
[[nodiscard]] HRESULT DxFontRenderData::UpdateFont(const FontInfoDesired& desired, FontInfo& actual, const int dpi, const std::unordered_map<std::wstring_view, uint32_t>& features, const std::unordered_map<std::wstring_view, float>& axes) noexcept
try
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

        // Just a few lines below we configure IDWriteTextFormat3 instances with these font axes.
        // We need at least all 3 of these axes below, in order to ensure that if we get a request for an italic
        // text format, we can supply an italic one, no matter what font axes the user set (or didn't set).
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

    _RefreshUserLocaleName();
    _BuildFontRenderData(desired, actual, dpi);
    _featureVector = std::move(fontFeatures);
    _userFontWeight = static_cast<DWRITE_FONT_WEIGHT>(actual.GetWeight());
    _boxDrawingEffect.Reset();

    {
        // Just a few lines above we hardcode indices 0/1/2 in fontAxisValues to the weight/italic/slant axes.
        // If they're -1.0f they haven't been set by the user and must be filled by us.
        // When we call SetFontAxisValues() we basically override (disable) DirectWrite's internal font axes,
        // and if either of the 3 aren't set we'd make it impossible for the user to see bold/italic text.
#pragma warning(suppress : 26494) // Variable 'standardAxes' is uninitialized. Always initialize an object (type.5).
        std::array<DWRITE_FONT_AXIS_VALUE, 3> standardAxes;

        if (!fontAxisValues.empty())
        {
            Expects(fontAxisValues.size() >= standardAxes.size());
            memcpy(standardAxes.data(), fontAxisValues.data(), sizeof(standardAxes));
        }

        for (auto italic = 0; italic < 2; ++italic)
        {
            for (auto bold = 0; bold < 2; ++bold)
            {
                const auto fontWeight = bold ? DWRITE_FONT_WEIGHT_BOLD : _userFontWeight;
                const auto fontStyle = italic ? DWRITE_FONT_STYLE_ITALIC : DWRITE_FONT_STYLE_NORMAL;
                auto& textFormat = _textFormats[italic][bold];

                // If the given face name couldn't be found this will call _NearbyCollection at some point.
                // Thanks to that we can use _nearbyFontCollection.Get() below to get the font collection for our textFormat as well.
                const auto font = _ResolveFontWithFallback(actual.GetFaceName(), fontWeight, fontStyle);

                Microsoft::WRL::ComPtr<IDWriteFontFace> face;
                THROW_IF_FAILED(font->CreateFontFace(&face));
                THROW_IF_FAILED(face.As(&_fontFaces[italic][bold]));

                THROW_IF_FAILED(_dwriteFactory->CreateTextFormat(actual.GetFaceName().c_str(), _nearbyFontCollection.Get(), font->GetWeight(), font->GetStyle(), DWRITE_FONT_STRETCH_NORMAL, _fontSize, _userLocaleName.get(), &textFormat));
                THROW_IF_FAILED(textFormat->SetLineSpacing(_lineSpacing.method, _lineSpacing.height, _lineSpacing.baseline));
                THROW_IF_FAILED(textFormat->SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_NEAR));
                THROW_IF_FAILED(textFormat->SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP));

                if (!fontAxisValues.empty())
                {
                    Microsoft::WRL::ComPtr<IDWriteTextFormat3> textFormat3;
                    if (SUCCEEDED(textFormat.As(&textFormat3)))
                    {
                        // The wght axis defaults to the font weight.
                        fontAxisValues[0].value = bold || standardAxes[0].value == -1.0f ? static_cast<float>(fontWeight) : standardAxes[0].value;
                        // The ital axis defaults to 1 if this is italic and 0 otherwise.
                        fontAxisValues[1].value = italic ? 1.0f : (standardAxes[1].value == -1.0f ? 0.0f : standardAxes[1].value);
                        // The slnt axis defaults to -12 if this is italic and 0 otherwise.
                        fontAxisValues[2].value = italic ? -12.0f : (standardAxes[2].value == -1.0f ? 0.0f : standardAxes[2].value);

                        THROW_IF_FAILED(textFormat3->SetFontAxisValues(fontAxisValues.data(), gsl::narrow_cast<UINT32>(fontAxisValues.size())));
                        _textFormatAxes[italic][bold] = fontAxisValues;
                    }
                }
            }
        }
    }

    return S_OK;
}
CATCH_RETURN()

// Routine Description:
// - Calculates the box drawing scale/translate matrix values to fit a box glyph into the cell as perfectly as possible.
// Arguments:
// - format - Text format used to determine line spacing (height including ascent & descent) as calculated from the base font.
// - widthPixels - The pixel width of the available cell.
// - face - The font face that is currently being used, may differ from the base font from the layout.
// - fontScale -  if the given font face is going to be scaled versus the format, we need to know so we can compensate for that. pass 1.0f for no scaling.
// - effect - Receives the effect to apply to box drawing characters. If no effect is received, special treatment isn't required.
// Return Value:
// - S_OK, GSL/WIL errors, DirectWrite errors, or math errors.
[[nodiscard]] HRESULT DxFontRenderData::s_CalculateBoxEffect(IDWriteTextFormat* format, size_t widthPixels, IDWriteFontFace1* face, float fontScale, IBoxDrawingEffect** effect) noexcept
try
{
    // Check for bad in parameters.
    RETURN_HR_IF(E_INVALIDARG, !format);
    RETURN_HR_IF(E_INVALIDARG, !face);

    // Check the out parameter and fill it up with null.
    RETURN_HR_IF(E_INVALIDARG, !effect);
    *effect = nullptr;

    // The format is based around the main font that was specified by the user.
    // We need to know its size as well as the final spacing that was calculated around
    // it when it was first selected to get an idea of how large the bounding box is.
    const auto fontSize = format->GetFontSize();

    DWRITE_LINE_SPACING_METHOD spacingMethod;
    float lineSpacing; // total height of the cells
    float baseline; // vertical position counted down from the top where the characters "sit"
    RETURN_IF_FAILED(format->GetLineSpacing(&spacingMethod, &lineSpacing, &baseline));

    const float ascentPixels = baseline;
    const float descentPixels = lineSpacing - baseline;

    // We need this for the designUnitsPerEm which will be required to move back and forth between
    // Design Units and Pixels. I'll elaborate below.
    DWRITE_FONT_METRICS1 fontMetrics;
    face->GetMetrics(&fontMetrics);

    // If we had font fallback occur, the size of the font given to us (IDWriteFontFace1) can be different
    // than the font size used for the original format (IDWriteTextFormat).
    const auto scaledFontSize = fontScale * fontSize;

    // This is Unicode FULL BLOCK U+2588.
    // We presume that FULL BLOCK should be filling its entire cell in all directions so it should provide a good basis
    // in knowing exactly where to touch every single edge.
    // We're also presuming that the other box/line drawing glyphs were authored in this font to perfectly inscribe
    // inside of FULL BLOCK, with the same left/top/right/bottom bearings so they would look great when drawn adjacent.
    const UINT32 blockCodepoint = L'\x2588';

    // Get the index of the block out of the font.
    UINT16 glyphIndex;
    RETURN_IF_FAILED(face->GetGlyphIndicesW(&blockCodepoint, 1, &glyphIndex));

    // If it was 0, it wasn't found in the font. We're going to try again with
    // Unicode BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL U+253C which should be touching
    // all the edges of the possible rectangle, much like a full block should.
    if (glyphIndex == 0)
    {
        const UINT32 alternateCp = L'\x253C';
        RETURN_IF_FAILED(face->GetGlyphIndicesW(&alternateCp, 1, &glyphIndex));
    }

    // If we still didn't find the glyph index, we haven't implemented any further logic to figure out the box dimensions.
    // So we're just going to leave successfully as is and apply no scaling factor. It might look not-right, but it won't
    // stop the rendering pipeline.
    RETURN_HR_IF(S_FALSE, glyphIndex == 0);

    // Get the metrics of the given glyph, which we're going to treat as the outline box in which all line/block drawing
    // glyphs will be inscribed within, perfectly touching each edge as to align when two cells meet.
    DWRITE_GLYPH_METRICS boxMetrics = { 0 };
    RETURN_IF_FAILED(face->GetDesignGlyphMetrics(&glyphIndex, 1, &boxMetrics));

    // NOTE: All metrics we receive from DWRITE are going to be in "design units" which are a somewhat agnostic
    //       way of describing proportions.
    //       Converting back and forth between real pixels and design units is possible using
    //       any font's specific fontSize and the designUnitsPerEm FONT_METRIC value.
    //
    // Here's what to know about the boxMetrics:
    //
    //
    //
    //   topLeft --> +--------------------------------+    ---
    //               |         ^                      |     |
    //               |         |  topSide             |     |
    //               |         |  Bearing             |     |
    //               |         v                      |     |
    //               |      +-----------------+       |     |
    //               |      |                 |       |     |
    //               |      |                 |       |     | a
    //               |      |                 |       |     | d
    //               |      |                 |       |     | v
    //               +<---->+                 |       |     | a
    //               |      |                 |       |     | n
    //               | left |                 |       |     | c
    //               | Side |                 |       |     | e
    //               | Bea- |                 |       |     | H
    //               | ring |                 | right |     | e
    //  vertical     |      |                 | Side  |     | i
    //  OriginY -->  x      |                 | Bea-  |     | g
    //               |      |                 | ring  |     | h
    //               |      |                 |       |     | t
    //               |      |                 +<----->+     |
    //               |      +-----------------+       |     |
    //               |                     ^          |     |
    //               |       bottomSide    |          |     |
    //               |          Bearing    |          |     |
    //               |                     v          |     |
    //               +--------------------------------+    ---
    //
    //
    //               |                                |
    //               +--------------------------------+
    //               |         advanceWidth           |
    //
    //
    // NOTE: The bearings can be negative, in which case it is specifying that the glyphs overhang the box
    // as defined by the advanceHeight/width.
    // See also: https://docs.microsoft.com/en-us/windows/win32/api/dwrite/ns-dwrite-dwrite_glyph_metrics

    // The scale is a multiplier and the translation is addition. So *1 and +0 will mean nothing happens.
    const float defaultBoxVerticalScaleFactor = 1.0f;
    float boxVerticalScaleFactor = defaultBoxVerticalScaleFactor;
    const float defaultBoxVerticalTranslation = 0.0f;
    float boxVerticalTranslation = defaultBoxVerticalTranslation;
    {
        // First, find the dimensions of the glyph representing our fully filled box.

        // Ascent is how far up from the baseline we'll draw.
        // verticalOriginY is the measure from the topLeft corner of the bounding box down to where
        // the glyph's version of the baseline is.
        // topSideBearing is how much "gap space" is left between that topLeft and where the glyph
        // starts drawing. Subtract the gap space to find how far is drawn upward from baseline.
        const auto boxAscentDesignUnits = boxMetrics.verticalOriginY - boxMetrics.topSideBearing;

        // Descent is how far down from the baseline we'll draw.
        // advanceHeight is the total height of the drawn bounding box.
        // verticalOriginY is how much was given to the ascent, so subtract that out.
        // What remains is then the descent value. Remove the
        // bottomSideBearing as the "gap space" on the bottom to find how far is drawn downward from baseline.
        const auto boxDescentDesignUnits = boxMetrics.advanceHeight - boxMetrics.verticalOriginY - boxMetrics.bottomSideBearing;

        // The height, then, of the entire box is just the sum of the ascent above the baseline and the descent below.
        const auto boxHeightDesignUnits = boxAscentDesignUnits + boxDescentDesignUnits;

        // Second, find the dimensions of the cell we're going to attempt to fit within.
        // We know about the exact ascent/descent units in pixels as calculated when we chose a font and
        // adjusted the ascent/descent for a nice perfect baseline and integer total height.
        // All we need to do is adapt it into Design Units so it meshes nicely with the Design Units above.
        // Use the formula: Pixels * Design Units Per Em / Font Size = Design Units
        const auto cellAscentDesignUnits = ascentPixels * fontMetrics.designUnitsPerEm / scaledFontSize;
        const auto cellDescentDesignUnits = descentPixels * fontMetrics.designUnitsPerEm / scaledFontSize;
        const auto cellHeightDesignUnits = cellAscentDesignUnits + cellDescentDesignUnits;

        // OK, now do a few checks. If the drawn box touches the top and bottom of the cell
        // and the box is overall tall enough, then we'll not bother adjusting.
        // We will presume the font author has set things as they wish them to be.
        const auto boxTouchesCellTop = boxAscentDesignUnits >= cellAscentDesignUnits;
        const auto boxTouchesCellBottom = boxDescentDesignUnits >= cellDescentDesignUnits;
        const auto boxIsTallEnoughForCell = boxHeightDesignUnits >= cellHeightDesignUnits;

        // If not...
        if (!(boxTouchesCellTop && boxTouchesCellBottom && boxIsTallEnoughForCell))
        {
            // Find a scaling factor that will make the total height drawn of this box
            // perfectly fit the same number of design units as the cell.
            // Since scale factor is a multiplier, it doesn't matter that this is design units.
            // The fraction between the two heights in pixels should be exactly the same
            // (which is what will matter when we go to actually render it... the pixels that is.)
            // Don't scale below 1.0. If it'd shrink, just center it at the prescribed scale.
            boxVerticalScaleFactor = std::max(cellHeightDesignUnits / boxHeightDesignUnits, 1.0f);

            // The box as scaled might be hanging over the top or bottom of the cell (or both).
            // We find out the amount of overhang/underhang on both the top and the bottom.
            const auto extraAscent = boxAscentDesignUnits * boxVerticalScaleFactor - cellAscentDesignUnits;
            const auto extraDescent = boxDescentDesignUnits * boxVerticalScaleFactor - cellDescentDesignUnits;

            // This took a bit of time and effort and it's difficult to put into words, but here goes.
            // We want the average of the two magnitudes to find out how much to "take" from one and "give"
            // to the other such that both are equal. We presume the glyphs are designed to be drawn
            // centered in their box vertically to look good.
            // The ordering around subtraction is required to ensure that the direction is correct with a negative
            // translation moving up (taking excess descent and adding to ascent) and positive is the opposite.
            const auto boxVerticalTranslationDesignUnits = (extraAscent - extraDescent) / 2;

            // The translation is just a raw movement of pixels up or down. Since we were working in Design Units,
            // we need to run the opposite algorithm shown above to go from Design Units to Pixels.
            boxVerticalTranslation = boxVerticalTranslationDesignUnits * scaledFontSize / fontMetrics.designUnitsPerEm;
        }
    }

    // The horizontal adjustments follow the exact same logic as the vertical ones.
    const float defaultBoxHorizontalScaleFactor = 1.0f;
    float boxHorizontalScaleFactor = defaultBoxHorizontalScaleFactor;
    const float defaultBoxHorizontalTranslation = 0.0f;
    float boxHorizontalTranslation = defaultBoxHorizontalTranslation;
    {
        // This is the only difference. We don't have a horizontalOriginX from the metrics.
        // However, https://docs.microsoft.com/en-us/windows/win32/api/dwrite/ns-dwrite-dwrite_glyph_metrics says
        // the X coordinate is specified by half the advanceWidth to the right of the horizontalOrigin.
        // So we'll use that as the "center" and apply it the role that verticalOriginY had above.

        const auto boxCenterDesignUnits = boxMetrics.advanceWidth / 2;
        const auto boxLeftDesignUnits = boxCenterDesignUnits - boxMetrics.leftSideBearing;
        const auto boxRightDesignUnits = boxMetrics.advanceWidth - boxMetrics.rightSideBearing - boxCenterDesignUnits;
        const auto boxWidthDesignUnits = boxLeftDesignUnits + boxRightDesignUnits;

        const auto cellWidthDesignUnits = widthPixels * fontMetrics.designUnitsPerEm / scaledFontSize;
        const auto cellLeftDesignUnits = cellWidthDesignUnits / 2;
        const auto cellRightDesignUnits = cellLeftDesignUnits;

        const auto boxTouchesCellLeft = boxLeftDesignUnits >= cellLeftDesignUnits;
        const auto boxTouchesCellRight = boxRightDesignUnits >= cellRightDesignUnits;
        const auto boxIsWideEnoughForCell = boxWidthDesignUnits >= cellWidthDesignUnits;

        if (!(boxTouchesCellLeft && boxTouchesCellRight && boxIsWideEnoughForCell))
        {
            boxHorizontalScaleFactor = std::max(cellWidthDesignUnits / boxWidthDesignUnits, 1.0f);
            const auto extraLeft = boxLeftDesignUnits * boxHorizontalScaleFactor - cellLeftDesignUnits;
            const auto extraRight = boxRightDesignUnits * boxHorizontalScaleFactor - cellRightDesignUnits;

            const auto boxHorizontalTranslationDesignUnits = (extraLeft - extraRight) / 2;

            boxHorizontalTranslation = boxHorizontalTranslationDesignUnits * scaledFontSize / fontMetrics.designUnitsPerEm;
        }
    }

    // If we set anything, make a drawing effect. Otherwise, there isn't one.
    if (defaultBoxVerticalScaleFactor != boxVerticalScaleFactor ||
        defaultBoxVerticalTranslation != boxVerticalTranslation ||
        defaultBoxHorizontalScaleFactor != boxHorizontalScaleFactor ||
        defaultBoxHorizontalTranslation != boxHorizontalTranslation)
    {
        // OK, make the object that will represent our effect, stuff the metrics into it, and return it.
        RETURN_IF_FAILED(WRL::MakeAndInitialize<BoxDrawingEffect>(effect, boxVerticalScaleFactor, boxVerticalTranslation, boxHorizontalScaleFactor, boxHorizontalTranslation));
    }

    return S_OK;
}
CATCH_RETURN()

// Routine Description:
// - Returns whether the user set or updated any of the font features to be applied
bool DxFontRenderData::DidUserSetFeatures() const noexcept
{
    return !_featureVector.empty();
}

// Routine Description:
// - Returns whether the user set or updated any of the font axes to be applied
bool DxFontRenderData::DidUserSetAxes() const noexcept
{
    return !_textFormatAxes[0][0].empty();
}

// Routine Description:
// - Build the needed data for rendering according to the font used
// Arguments:
// - desired - Information specifying the font that is requested
// - actual - Filled with the nearest font actually chosen for drawing
// - dpi - The DPI of the screen
// Return Value:
// - None
void DxFontRenderData::_BuildFontRenderData(const FontInfoDesired& desired, FontInfo& actual, const int dpi)
{
    // This is the first attempt to resolve font face after `UpdateFont`.
    // Note that the following line may cause property changes _inside_ `_defaultFontInfo` because the desired font may not exist.
    // See the implementation of `ResolveFontFaceWithFallback` for details.
    const auto font = _ResolveFontWithFallback(desired.GetFaceName(), static_cast<DWRITE_FONT_WEIGHT>(desired.GetWeight()), DWRITE_FONT_STYLE_NORMAL);

    Microsoft::WRL::ComPtr<IDWriteFontFace> face;
    THROW_IF_FAILED(font->CreateFontFace(&face));

    DWRITE_FONT_METRICS1 fontMetrics;
    face->GetMetrics(&fontMetrics);

    const UINT32 spaceCodePoint = L'M';
    UINT16 spaceGlyphIndex;
    THROW_IF_FAILED(face->GetGlyphIndicesW(&spaceCodePoint, 1, &spaceGlyphIndex));

    DWRITE_GLYPH_METRICS spaceMetrics = { 0 };
    THROW_IF_FAILED(face->GetDesignGlyphMetrics(&spaceGlyphIndex, 1, &spaceMetrics));

    // The math here is actually:
    // Requested Size in Points * DPI scaling factor * Points to Pixels scaling factor.
    // - DPI = dots per inch
    // - PPI = points per inch or "points" as usually seen when choosing a font size
    // - The DPI scaling factor is the current monitor DPI divided by 96, the default DPI.
    // - The Points to Pixels factor is based on the typography definition of 72 points per inch.
    //    As such, converting requires taking the 96 pixel per inch default and dividing by the 72 points per inch
    //    to get a factor of 1 and 1/3.
    // This turns into something like:
    // - 12 ppi font * (96 dpi / 96 dpi) * (96 dpi / 72 points per inch) = 16 pixels tall font for 100% display (96 dpi is 100%)
    // - 12 ppi font * (144 dpi / 96 dpi) * (96 dpi / 72 points per inch) = 24 pixels tall font for 150% display (144 dpi is 150%)
    // - 12 ppi font * (192 dpi / 96 dpi) * (96 dpi / 72 points per inch) = 32 pixels tall font for 200% display (192 dpi is 200%)
    float heightDesired = static_cast<float>(desired.GetEngineSize().Y) * static_cast<float>(USER_DEFAULT_SCREEN_DPI) / POINTS_PER_INCH;

    // The advance is the number of pixels left-to-right (X dimension) for the given font.
    // We're finding a proportional factor here with the design units in "ems", not an actual pixel measurement.

    // Now we play trickery with the font size. Scale by the DPI to get the height we expect.
    heightDesired *= (static_cast<float>(dpi) / static_cast<float>(USER_DEFAULT_SCREEN_DPI));

    const float widthAdvance = static_cast<float>(spaceMetrics.advanceWidth) / fontMetrics.designUnitsPerEm;

    // Use the real pixel height desired by the "em" factor for the width to get the number of pixels
    // we will need per character in width. This will almost certainly result in fractional X-dimension pixels.
    const float widthApprox = heightDesired * widthAdvance;

    // Since we can't deal with columns of the presentation grid being fractional pixels in width, round to the nearest whole pixel.
    const float widthExact = round(widthApprox);

    // Now reverse the "em" factor from above to turn the exact pixel width into a (probably) fractional
    // height in pixels of each character. It's easier for us to pad out height and align vertically
    // than it is horizontally.
    const auto fontSize = widthExact / widthAdvance;
    _fontSize = fontSize;

    // Now figure out the basic properties of the character height which include ascent and descent
    // for this specific font size.
    const float ascent = (fontSize * fontMetrics.ascent) / fontMetrics.designUnitsPerEm;
    const float descent = (fontSize * fontMetrics.descent) / fontMetrics.designUnitsPerEm;

    // Get the gap.
    const float gap = (fontSize * fontMetrics.lineGap) / fontMetrics.designUnitsPerEm;
    const float halfGap = gap / 2;

    // We're going to build a line spacing object here to track all of this data in our format.
    DWRITE_LINE_SPACING lineSpacing = {};
    lineSpacing.method = DWRITE_LINE_SPACING_METHOD_UNIFORM;

    // We need to make sure the baseline falls on a round pixel (not a fractional pixel).
    // If the baseline is fractional, the text appears blurry, especially at small scales.
    // Since we also need to make sure the bounding box as a whole is round pixels
    // (because the entire console system maths in full cell units),
    // we're just going to ceiling up the ascent and descent to make a full pixel amount
    // and set the baseline to the full round pixel ascent value.
    //
    // For reference, for the letters "ag":
    // ...
    //          gggggg      bottom of previous line
    //
    // -----------------    <===========================================|
    //                         | topSideBearing       |  1/2 lineGap    |
    // aaaaaa   ggggggg     <-------------------------|-------------|   |
    //      a   g    g                                |             |   |
    //  aaaaa   ggggg                                 |<-ascent     |   |
    // a    a   g                                     |             |   |---- lineHeight
    // aaaaa a  gggggg      <----baseline, verticalOriginY----------|---|
    //          g     g                               |<-descent    |   |
    //          gggggg      <-------------------------|-------------|   |
    //                         | bottomSideBearing    | 1/2 lineGap     |
    // -----------------    <===========================================|
    //
    // aaaaaa   ggggggg     top of next line
    // ...
    //
    // Also note...
    // We're going to add half the line gap to the ascent and half the line gap to the descent
    // to ensure that the spacing is balanced vertically.
    // Generally speaking, the line gap is added to the ascent by DirectWrite itself for
    // horizontally drawn text which can place the baseline and glyphs "lower" in the drawing
    // box than would be desired for proper alignment of things like line and box characters
    // which will try to sit centered in the area and touch perfectly with their neighbors.

    const auto fullPixelAscent = ceil(ascent + halfGap);
    const auto fullPixelDescent = ceil(descent + halfGap);
    lineSpacing.height = fullPixelAscent + fullPixelDescent;
    lineSpacing.baseline = fullPixelAscent;

    // According to MSDN (https://docs.microsoft.com/en-us/windows/win32/api/dwrite_3/ne-dwrite_3-dwrite_font_line_gap_usage)
    // Setting "ENABLED" means we've included the line gapping in the spacing numbers given.
    lineSpacing.fontLineGapUsage = DWRITE_FONT_LINE_GAP_USAGE_ENABLED;

    _lineSpacing = lineSpacing;

    // The scaled size needs to represent the pixel box that each character will fit within for the purposes
    // of hit testing math and other such multiplication/division.
    COORD coordSize = { 0 };
    coordSize.X = gsl::narrow<SHORT>(widthExact);
    coordSize.Y = gsl::narrow_cast<SHORT>(lineSpacing.height);

    // Unscaled is for the purposes of re-communicating this font back to the renderer again later.
    // As such, we need to give the same original size parameter back here without padding
    // or rounding or scaling manipulation.
    const COORD unscaled = desired.GetEngineSize();

    const COORD scaled = coordSize;

    actual.SetFromEngine(desired.GetFaceName().c_str(),
                         desired.GetFamily(),
                         desired.GetWeight(),
                         false,
                         scaled,
                         unscaled);

    LineMetrics lineMetrics;
    // There is no font metric for the grid line width, so we use a small
    // multiple of the font size, which typically rounds to a pixel.
    lineMetrics.gridlineWidth = std::round(fontSize * 0.025f);

    // All other line metrics are in design units, so to get a pixel value,
    // we scale by the font size divided by the design-units-per-em.
    const auto scale = fontSize / fontMetrics.designUnitsPerEm;
    lineMetrics.underlineOffset = std::round(fontMetrics.underlinePosition * scale);
    lineMetrics.underlineWidth = std::round(fontMetrics.underlineThickness * scale);
    lineMetrics.strikethroughOffset = std::round(fontMetrics.strikethroughPosition * scale);
    lineMetrics.strikethroughWidth = std::round(fontMetrics.strikethroughThickness * scale);

    // We always want the lines to be visible, so if a stroke width ends up
    // at zero after rounding, we need to make it at least 1 pixel.
    lineMetrics.gridlineWidth = std::max(lineMetrics.gridlineWidth, 1.0f);
    lineMetrics.underlineWidth = std::max(lineMetrics.underlineWidth, 1.0f);
    lineMetrics.strikethroughWidth = std::max(lineMetrics.strikethroughWidth, 1.0f);

    // Offsets are relative to the base line of the font, so we subtract
    // from the ascent to get an offset relative to the top of the cell.
    lineMetrics.underlineOffset = fullPixelAscent - lineMetrics.underlineOffset;
    lineMetrics.strikethroughOffset = fullPixelAscent - lineMetrics.strikethroughOffset;

    // For double underlines we need a second offset, just below the first,
    // but with a bit of a gap (about double the grid line width).
    lineMetrics.underlineOffset2 = lineMetrics.underlineOffset +
                                   lineMetrics.underlineWidth +
                                   std::round(fontSize * 0.05f);

    // However, we don't want the underline to extend past the bottom of the
    // cell, so we clamp the offset to fit just inside.
    const auto maxUnderlineOffset = lineSpacing.height - lineMetrics.underlineWidth;
    lineMetrics.underlineOffset2 = std::min(lineMetrics.underlineOffset2, maxUnderlineOffset);

    // But if the resulting gap isn't big enough even to register as a thicker
    // line, it's better to place the second line slightly above the first.
    if (lineMetrics.underlineOffset2 < lineMetrics.underlineOffset + lineMetrics.gridlineWidth)
    {
        lineMetrics.underlineOffset2 = lineMetrics.underlineOffset - lineMetrics.gridlineWidth;
    }

    // We also add half the stroke width to the offsets, since the line
    // coordinates designate the center of the line.
    lineMetrics.underlineOffset += lineMetrics.underlineWidth / 2.0f;
    lineMetrics.underlineOffset2 += lineMetrics.underlineWidth / 2.0f;
    lineMetrics.strikethroughOffset += lineMetrics.strikethroughWidth / 2.0f;

    _lineMetrics = lineMetrics;

    _glyphCell = actual.GetSize();
}

// Routine Description:
// - Attempts to locate the font given, but then begins falling back if we cannot find it.
// - We'll try to fall back to Consolas with the given weight/stretch/style first,
//   then try Consolas again with normal weight/stretch/style,
//   and if nothing works, then we'll throw an error.
// Arguments:
// - dwriteFactory - The DWrite factory to use
// - localeName - Locale to search for appropriate fonts
// Return Value:
// - Smart pointer holding interface reference for queryable font data.
[[nodiscard]] Microsoft::WRL::ComPtr<IDWriteFont> DxFontRenderData::_ResolveFontWithFallback(std::wstring familyName, DWRITE_FONT_WEIGHT weight, DWRITE_FONT_STYLE style)
{
    if (familyName.empty())
    {
        familyName = L"Consolas";
    }
    if (!weight)
    {
        weight = DWRITE_FONT_WEIGHT_NORMAL;
    }
    if (!style)
    {
        style = DWRITE_FONT_STYLE_NORMAL;
    }

    // First attempt to find exactly what the user asked for.
    Microsoft::WRL::ComPtr<IDWriteFont> font;

    // GH#10211 - wrap this all up in a try/catch. If the nearby fonts are
    // corrupted, then we don't want to throw out of this top half of this
    // method. We still want to fall back to a font that's reasonable, below.
    try
    {
        font = _FindFont(familyName.c_str(), weight, style);

        if (!font)
        {
            // If we missed, try looking a little more by trimming the last word off the requested family name a few times.
            // Quite often, folks are specifying weights or something in the familyName and it causes failed resolution and
            // an unexpected error dialog. We theoretically could detect the weight words and convert them, but this
            // is the quick fix for the majority scenario.
            // The long/full fix is backlogged to GH#9744
            // Also this doesn't count as a fallback because we don't want to annoy folks with the warning dialog over
            // this resolution.
            while (!font && !familyName.empty())
            {
                const auto lastSpace = familyName.find_last_of(UNICODE_SPACE);

                // value is unsigned and npos will be greater than size.
                // if we didn't find anything to trim, leave.
                if (lastSpace >= familyName.size())
                {
                    break;
                }

                // trim string down to just before the found space
                // (space found at 6... trim from 0 for 6 length will give us 0-5 as the new string)
                familyName = familyName.substr(0, lastSpace);

                // Try to find it with the shortened family name
                font = _FindFont(familyName.c_str(), weight, style);
            }
        }
    }
    CATCH_LOG();

    // Alright, if our quick shot at trimming didn't work either...
    // move onto looking up a font from our hardcoded list of fonts
    // that should really always be available.
    if (!font)
    {
        for (const auto fallbackFont : FALLBACK_FONT_FACES)
        {
            // With these fonts, don't attempt the nearby lookup. We're looking
            // for system fonts only. If one of the nearby fonts is causing us
            // problems (like in GH#10211), then we don't want to go anywhere

            // near it in this part.
            font = _FindFont(fallbackFont, weight, style);
            if (font)
            {
                break;
            }

            font = _FindFont(fallbackFont, DWRITE_FONT_WEIGHT_NORMAL, DWRITE_FONT_STYLE_NORMAL);
            if (font)
            {
                break;
            }
        }
    }

    THROW_HR_IF_NULL(E_FAIL, font);
    return font;
}

// Routine Description:
// - Locates a suitable font face from the given information
// Arguments:
// - dwriteFactory - The DWrite factory to use
// - localeName - Locale to search for appropriate fonts
// Return Value:
// - Smart pointer holding interface reference for queryable font data.
[[nodiscard]] Microsoft::WRL::ComPtr<IDWriteFont> DxFontRenderData::_FindFont(const wchar_t* familyName, DWRITE_FONT_WEIGHT weight, DWRITE_FONT_STYLE style)
{
    Microsoft::WRL::ComPtr<IDWriteFont> font;

    Microsoft::WRL::ComPtr<IDWriteFontCollection> fontCollection;
    THROW_IF_FAILED(_dwriteFactory->GetSystemFontCollection(&fontCollection, false));

    UINT32 familyIndex;
    BOOL familyExists;
    THROW_IF_FAILED(fontCollection->FindFamilyName(familyName, &familyIndex, &familyExists));

    // If the system collection missed, try the files sitting next to our binary.
    if (!familyExists)
    {
        // May be null on OS below Windows 10. If null, just skip the attempt.
        if (const auto nearbyCollection = _NearbyCollection())
        {
            THROW_IF_FAILED(nearbyCollection->FindFamilyName(familyName, &familyIndex, &familyExists));
            fontCollection = nearbyCollection;
        }
    }

    if (familyExists)
    {
        Microsoft::WRL::ComPtr<IDWriteFontFamily> fontFamily;
        THROW_IF_FAILED(fontCollection->GetFontFamily(familyIndex, &fontFamily));
        THROW_IF_FAILED(fontFamily->GetFirstMatchingFont(weight, DWRITE_FONT_STRETCH_NORMAL, style, &font));
    }

    return font;
}

// Routine Description:
// - Creates a DirectWrite font collection of font files that are sitting next to the running
//   binary (in the same directory as the EXE).
// Arguments:
// - dwriteFactory - The DWrite factory to use
// Return Value:
// - DirectWrite font collection. May be null if one cannot be created.
[[nodiscard]] IDWriteFontCollection* DxFontRenderData::_NearbyCollection()
{
    if (_nearbyFontCollection)
    {
        return _nearbyFontCollection.Get();
    }

    // The convenience interfaces for loading fonts from files
    // are only available on Windows 10+.
    ::Microsoft::WRL::ComPtr<IDWriteFactory6> factory6;
    if (FAILED(_dwriteFactory->QueryInterface<IDWriteFactory6>(&factory6)))
    {
        return nullptr;
    }

    ::Microsoft::WRL::ComPtr<IDWriteFontCollection1> systemFontCollection;
    THROW_IF_FAILED(factory6->GetSystemFontCollection(false, &systemFontCollection, 0));

    ::Microsoft::WRL::ComPtr<IDWriteFontSet> systemFontSet;
    THROW_IF_FAILED(systemFontCollection->GetFontSet(&systemFontSet));

    ::Microsoft::WRL::ComPtr<IDWriteFontSetBuilder2> fontSetBuilder2;
    THROW_IF_FAILED(factory6->CreateFontSetBuilder(&fontSetBuilder2));

    THROW_IF_FAILED(fontSetBuilder2->AddFontSet(systemFontSet.Get()));

    // Magic static so we only attempt to grovel the hard disk once no matter how many instances
    // of the font collection itself we require.
    static const auto knownPaths = s_GetNearbyFonts();
    for (auto& p : knownPaths)
    {
        fontSetBuilder2->AddFontFile(p.c_str());
    }

    ::Microsoft::WRL::ComPtr<IDWriteFontSet> fontSet;
    THROW_IF_FAILED(fontSetBuilder2->CreateFontSet(&fontSet));

    ::Microsoft::WRL::ComPtr<IDWriteFontCollection1> fontCollection;
    THROW_IF_FAILED(factory6->CreateFontCollectionFromFontSet(fontSet.Get(), &fontCollection));

    _nearbyFontCollection = fontCollection;
    return _nearbyFontCollection.Get();
}

// Routine Description:
// - Digs through the directory that the current executable is running within to find
//   any TTF files sitting next to it.
// Arguments:
// - <none>
// Return Value:
// - Iterable collection of filesystem paths, one per font file that was found
[[nodiscard]] std::vector<std::filesystem::path> DxFontRenderData::s_GetNearbyFonts()
{
    std::vector<std::filesystem::path> paths;

    // Find the directory we're running from then enumerate all the TTF files
    // sitting next to us.
    const std::filesystem::path module{ wil::GetModuleFileNameW<std::wstring>(nullptr) };
    const auto folder{ module.parent_path() };

    for (const auto& p : std::filesystem::directory_iterator(folder))
    {
        if (til::ends_with_insensitive_ascii(p.path().native(), L".ttf"))
        {
            paths.push_back(p.path());
        }
    }

    return paths;
}
