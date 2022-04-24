/*++
Copyright (c) Microsoft Corporation
Licensed under the MIT license.

Module Name:
- textBuffer.hpp

Abstract:
- This module contains structures and functions for manipulating a text
  based buffer within the console host window.

Author(s):
- Michael Niksa (miniksa) 10-Apr-2014
- Paul Campbell (paulcam) 10-Apr-2014

Revision History:
- From components of output.h/.c
  by Therese Stowell (ThereseS) 1990-1991

Notes:
ScreenBuffer data structure overview:

each screen buffer has an array of ROW structures.  each ROW structure
contains the data for one row of text.  the data stored for one row of
text is a character array and an attribute array.  the character array
is allocated the full length of the row from the heap, regardless of the
non-space length. we also maintain the non-space length.  the character
array is initialized to spaces.  the attribute
array is run length encoded (i.e 5 BLUE, 3 RED). if there is only one
attribute for the whole row (the normal case), it is stored in the ATTR_ROW
structure.  otherwise the attr string is allocated from the heap.

ROW - CHAR_ROW - CHAR string
\          \ length of char string
\
ATTR_ROW - ATTR_PAIR string
\ length of attr pair string
ROW
ROW
ROW

ScreenInfo->Rows points to the ROW array. ScreenInfo->Rows[0] is not
necessarily the top row. ScreenInfo->BufferInfo.TextInfo->FirstRow contains the index of
the top row.  That means scrolling (if scrolling entire screen)
merely involves changing the FirstRow index,
filling in the last row, and updating the screen.

--*/

#pragma once

#include <vector>

#include "cursor.h"
#include "Row.hpp"
#include "TextAttribute.hpp"
#include "UnicodeStorage.hpp"
#include "../types/inc/Viewport.hpp"

#include "../buffer/out/textBufferCellIterator.hpp"
#include "../buffer/out/textBufferTextIterator.hpp"

namespace Microsoft::Console::Render
{
    class Renderer;
}

class TextBuffer final
{
public:
    TextBuffer(COORD screenBufferSize,
               TextAttribute defaultAttributes,
               UINT cursorSize,
               bool isActiveBuffer,
               Microsoft::Console::Render::Renderer& renderer);
    TextBuffer(const TextBuffer& a) = delete;

    // Used for duplicating properties to another text buffer
    void CopyProperties(const TextBuffer& OtherBuffer) noexcept;

    // row manipulation
    const ROW& GetRowByOffset(size_t index) const;
    ROW& GetRowByOffset(size_t index);

    TextBufferCellIterator GetCellDataAt(COORD at) const;
    TextBufferCellIterator GetCellLineDataAt(COORD at) const;
    TextBufferCellIterator GetCellDataAt(COORD at, Microsoft::Console::Types::Viewport limit) const;
    TextBufferTextIterator GetTextDataAt(COORD at) const;
    TextBufferTextIterator GetTextLineDataAt(COORD at) const;
    TextBufferTextIterator GetTextDataAt(COORD at, Microsoft::Console::Types::Viewport limit) const;

    // Text insertion functions
    OutputCellIterator Write(OutputCellIterator givenIt);

    OutputCellIterator Write(OutputCellIterator givenIt,
                             COORD target,
                             std::optional<bool> wrap = true);

    OutputCellIterator WriteLine(OutputCellIterator givenIt,
                                 COORD target,
                                 std::optional<bool> setWrap = std::nullopt,
                                 std::optional<size_t> limitRight = std::nullopt);

    bool InsertCharacter(wchar_t wch, DbcsAttribute dbcsAttribute, TextAttribute attr);
    bool InsertCharacter(std::wstring_view chars, DbcsAttribute dbcsAttribute, TextAttribute attr);
    bool IncrementCursor();
    bool NewlineCursor();

    // Scroll needs access to this to quickly rotate around the buffer.
    bool IncrementCircularBuffer(bool inVtMode = false);

    COORD GetLastNonSpaceCharacter(std::optional<const Microsoft::Console::Types::Viewport> viewOptional = std::nullopt) const;

    Cursor& GetCursor() noexcept;
    const Cursor& GetCursor() const noexcept;

    const SHORT GetFirstRowIndex() const noexcept;

    const Microsoft::Console::Types::Viewport GetSize() const noexcept;

    void ScrollRows(SHORT firstRow, SHORT size, SHORT delta);

    UINT TotalRowCount() const noexcept;

    [[nodiscard]] TextAttribute GetCurrentAttributes() const noexcept;

    void SetCurrentAttributes(const TextAttribute& currentAttributes) noexcept;

    void SetCurrentLineRendition(LineRendition lineRendition);
    void ResetLineRenditionRange(size_t startRow, size_t endRow);
    LineRendition GetLineRendition(size_t row) const;
    bool IsDoubleWidthLine(size_t row) const;

    SHORT GetLineWidth(size_t row) const;
    til::point ClampPositionWithinLine(til::point position) const;
    COORD ScreenToBufferPosition(COORD position) const;
    COORD BufferToScreenPosition(COORD position) const;

    void Reset();

    [[nodiscard]] HRESULT ResizeTraditional(COORD newSize) noexcept;

    const UnicodeStorage& GetUnicodeStorage() const noexcept;
    UnicodeStorage& GetUnicodeStorage() noexcept;

    void SetAsActiveBuffer(bool isActiveBuffer) noexcept;
    bool IsActiveBuffer() const noexcept;

    Microsoft::Console::Render::Renderer& GetRenderer() noexcept;

    void TriggerRedraw(const Microsoft::Console::Types::Viewport& viewport);
    void TriggerRedrawCursor(COORD position);
    void TriggerRedrawAll();
    void TriggerScroll();
    void TriggerScroll(COORD delta);
    void TriggerNewTextNotification(std::wstring_view newText);

    til::point GetWordStart(til::point target, std::wstring_view wordDelimiters, bool accessibilityMode = false, std::optional<til::point> limitOptional = std::nullopt) const;
    til::point GetWordEnd(til::point target, std::wstring_view wordDelimiters, bool accessibilityMode = false, std::optional<til::point> limitOptional = std::nullopt) const;
    bool MoveToNextWord(til::point& pos, std::wstring_view wordDelimiters, std::optional<til::point> limitOptional = std::nullopt) const;
    bool MoveToPreviousWord(til::point& pos, std::wstring_view wordDelimiters) const;

    til::point GetGlyphStart(til::point pos, std::optional<til::point> limitOptional = std::nullopt) const;
    til::point GetGlyphEnd(til::point pos, bool accessibilityMode = false, std::optional<til::point> limitOptional = std::nullopt) const;
    bool MoveToNextGlyph(til::point& pos, bool allowBottomExclusive = false, std::optional<til::point> limitOptional = std::nullopt) const;
    bool MoveToPreviousGlyph(til::point& pos, std::optional<til::point> limitOptional = std::nullopt) const;

    const std::vector<SMALL_RECT> GetTextRects(COORD start, COORD end, bool blockSelection, bool bufferCoordinates) const;

    void AddHyperlinkToMap(std::wstring_view uri, uint16_t id);
    std::wstring GetHyperlinkUriFromId(uint16_t id) const;
    uint16_t GetHyperlinkId(std::wstring_view uri, std::wstring_view id);
    void RemoveHyperlinkFromMap(uint16_t id) noexcept;
    std::wstring GetCustomIdFromId(uint16_t id) const;
    void CopyHyperlinkMaps(const TextBuffer& OtherBuffer);

    class TextAndColor
    {
    public:
        std::vector<std::wstring> text;
        std::vector<std::vector<COLORREF>> FgAttr;
        std::vector<std::vector<COLORREF>> BkAttr;
    };

    const TextAndColor GetText(bool includeCRLF,
                               bool trimTrailingWhitespace,
                               const std::vector<SMALL_RECT>& textRects,
                               std::function<std::pair<COLORREF, COLORREF>(const TextAttribute&)> GetAttributeColors = nullptr,
                               bool formatWrappedRows = false) const;

    static std::string GenHTML(const TextAndColor& rows,
                               int fontHeightPoints,
                               std::wstring_view fontFaceName,
                               COLORREF backgroundColor);

    static std::string GenRTF(const TextAndColor& rows,
                              int fontHeightPoints,
                              std::wstring_view fontFaceName,
                              COLORREF backgroundColor);

    struct PositionInformation
    {
        short mutableViewportTop{ 0 };
        short visibleViewportTop{ 0 };
    };

    static HRESULT Reflow(TextBuffer& oldBuffer,
                          TextBuffer& newBuffer,
                          std::optional<Microsoft::Console::Types::Viewport> lastCharacterViewport,
                          std::optional<std::reference_wrapper<PositionInformation>> positionInfo);

    const size_t AddPatternRecognizer(std::wstring_view regexString);
    void ClearPatternRecognizers() noexcept;
    void CopyPatterns(const TextBuffer& OtherBuffer);
    interval_tree::IntervalTree<til::point, size_t> GetPatterns(size_t firstRow, size_t lastRow) const;

private:
    void _UpdateSize();
    Microsoft::Console::Types::Viewport _size;
    std::vector<ROW> _storage;
    Cursor _cursor;

    SHORT _firstRow; // indexes top row (not necessarily 0)

    TextAttribute _currentAttributes;

    // storage location for glyphs that can't fit into the buffer normally
    UnicodeStorage _unicodeStorage;

    bool _isActiveBuffer;
    Microsoft::Console::Render::Renderer& _renderer;

    std::unordered_map<uint16_t, std::wstring> _hyperlinkMap;
    std::unordered_map<std::wstring, uint16_t> _hyperlinkCustomIdMap;
    uint16_t _currentHyperlinkId;

    void _RefreshRowIDs(std::optional<SHORT> newRowWidth);

    void _SetFirstRowIndex(SHORT FirstRowIndex) noexcept;

    COORD _GetPreviousFromCursor() const;

    void _SetWrapOnCurrentRow();
    void _AdjustWrapOnCurrentRow(bool fSet);

    // Assist with maintaining proper buffer state for Double Byte character sequences
    bool _PrepareForDoubleByteSequence(DbcsAttribute dbcsAttribute);
    bool _AssertValidDoubleByteSequence(DbcsAttribute dbcsAttribute);

    ROW& _GetFirstRow();
    ROW& _GetPrevRowNoWrap(const ROW& row);

    void _ExpandTextRow(SMALL_RECT& selectionRow) const;

    DelimiterClass _GetDelimiterClassAt(til::point pos, std::wstring_view wordDelimiters) const;
    til::point _GetWordStartForAccessibility(til::point target, std::wstring_view wordDelimiters) const;
    til::point _GetWordStartForSelection(til::point target, std::wstring_view wordDelimiters) const;
    til::point _GetWordEndForAccessibility(til::point target, std::wstring_view wordDelimiters, til::point limit) const;
    til::point _GetWordEndForSelection(til::point target, std::wstring_view wordDelimiters) const;

    void _PruneHyperlinks();

    static void _AppendRTFText(std::ostringstream& contentBuilder, const std::wstring_view& text);

    std::unordered_map<size_t, std::wstring> _idsAndPatterns;
    size_t _currentPatternId;

#ifdef UNIT_TESTING
    friend class TextBufferTests;
    friend class UiaTextRangeTests;
#endif
};
