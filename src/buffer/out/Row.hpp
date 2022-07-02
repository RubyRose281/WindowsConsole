/*++
Copyright (c) Microsoft Corporation
Licensed under the MIT license.

Module Name:
- Row.hpp

Abstract:
- data structure for information associated with one row of screen buffer

Author(s):
- Michael Niksa (miniksa) 10-Apr-2014
- Paul Campbell (paulcam) 10-Apr-2014

Revision History:
- From components of output.h/.c
  by Therese Stowell (ThereseS) 1990-1991
- Pulled into its own file from textBuffer.hpp/cpp (AustDi, 2017)
--*/

#pragma once

#include "til/rle.h"
#include "LineRendition.hpp"
#include "OutputCell.hpp"
#include "OutputCellIterator.hpp"

#pragma warning(push, 1)

class TextBuffer;

enum class DelimiterClass
{
    ControlChar,
    DelimiterChar,
    RegularChar
};

struct RowTextIterator
{
    RowTextIterator(wchar_t* chars, uint16_t* indices, uint16_t indicesCount, uint16_t beg, uint16_t end) noexcept;

    bool operator==(const RowTextIterator& other) const noexcept;
    RowTextIterator& operator++() noexcept;
    const RowTextIterator& operator*() const noexcept;

    std::wstring_view Text() const noexcept;
    til::CoordType Cols() const noexcept;
    DbcsAttribute DbcsAttr() const noexcept;

private:
    static constexpr uint16_t IndicesTrailer = 0x8000;
    static constexpr uint16_t IndicesMask = 0x7fff;

    uint16_t _indexAt(size_t col) const noexcept
    {
        assert(_indices && col <= _indicesCount);
        return _indices[col] & IndicesMask;
    }

    bool _isTrailer(size_t col) const noexcept
    {
        assert(_indices && col <= _indicesCount);
        return (_indices[col] & IndicesTrailer) == IndicesTrailer;
    }

    const wchar_t* _chars;
    const uint16_t* _indices;
    uint16_t _indicesCount;
    uint16_t _beg;
    uint16_t _end;
};

class ROW final
{
public:
    ROW(wchar_t* buffer, uint16_t* indices, uint16_t rowWidth, const TextAttribute& fillAttribute);
    ~ROW();

    void SetWrapForced(const bool wrap) noexcept { _wrapForced = wrap; }
    bool WasWrapForced() const noexcept { return _wrapForced; }
    void SetDoubleBytePadded(const bool doubleBytePadded) noexcept { _doubleBytePadded = doubleBytePadded; }
    bool WasDoubleBytePadded() const noexcept { return _doubleBytePadded; }
    LineRendition GetLineRendition() const noexcept { return _lineRendition; }
    void SetLineRendition(const LineRendition lineRendition) noexcept { _lineRendition = lineRendition; }

    bool Reset(const TextAttribute& Attr) noexcept;
    void Resize(wchar_t* chars, uint16_t* indices, uint16_t newWidth);
    void TransferAttributes(const til::small_rle<TextAttribute, uint16_t, 1>& attr, til::CoordType newWidth);

    void ClearCell(til::CoordType column);
    OutputCellIterator WriteCells(OutputCellIterator it, til::CoordType index, std::optional<bool> wrap = std::nullopt, std::optional<til::CoordType> limitRight = std::nullopt);
    bool SetAttrToEnd(til::CoordType beginIndex, TextAttribute attr);
    void ReplaceAttributes(til::CoordType beginIndex, til::CoordType endIndex, const TextAttribute& newAttr);
    til::CoordType PrecedingColumn(til::CoordType column) const noexcept;
    til::CoordType ReplaceCharacters(til::CoordType beginIndex, std::wstring_view& chars);
    void ReplaceCharacters(til::CoordType beginIndex, til::CoordType endIndex, const std::wstring_view& chars);

    const til::small_rle<TextAttribute, uint16_t, 1>& Attributes() const noexcept;
    TextAttribute GetAttrByColumn(til::CoordType column) const;
    std::vector<uint16_t> GetHyperlinks() const;
    uint16_t size() const noexcept;
    til::CoordType MeasureLeft() const noexcept;
    til::CoordType MeasureRight() const noexcept;
    bool ContainsText() const noexcept;
    std::wstring_view GlyphAt(til::CoordType column) const noexcept;
    DbcsAttribute DbcsAttrAt(til::CoordType column) const noexcept;
    std::wstring_view GetText() const noexcept;
    DelimiterClass DelimiterClassAt(til::CoordType column, const std::wstring_view& wordDelimiters) const noexcept;
    RowTextIterator CharsBegin() const noexcept;
    RowTextIterator CharsEnd() const noexcept;

    auto AttrBegin() const noexcept { return _attr.begin(); }
    auto AttrEnd() const noexcept { return _attr.end(); }

#ifdef UNIT_TESTING
    friend constexpr bool operator==(const ROW& a, const ROW& b) noexcept;
    friend class RowTests;
#endif

private:
    static constexpr uint16_t IndicesTrailer = 0x8000;
    static constexpr uint16_t IndicesMask = 0x7fff;

    template<typename T>
    static constexpr uint16_t clampedUint16(T v) noexcept
    {
        return static_cast<uint16_t>(std::max(0, std::min(65535, v)));
    }

    template<typename T>
    constexpr uint16_t clampedColumn(T v) const noexcept
    {
        return static_cast<uint16_t>(std::max(0, std::min(_indicesCount - 1, v)));
    }

    template<typename T>
    constexpr uint16_t clampedColumnExclusive(T v) const noexcept
    {
        return static_cast<uint16_t>(std::max(0, std::min<T>(_indicesCount, v)));
    }

    // clang-format off
#ifndef NDEBUG
    constexpr gsl::span<wchar_t>::iterator _charsBegin() noexcept { return gsl::make_span(_chars, _charsCapacity).begin(); }
    constexpr gsl::span<const wchar_t>::iterator _charsBegin() const noexcept { return gsl::make_span(_chars, _charsCapacity).begin(); }

    constexpr gsl::span<uint16_t>::iterator _indicesBegin() noexcept { return gsl::make_span(_indices, _indicesCount + 1).begin(); }
    constexpr gsl::span<const uint16_t>::iterator _indicesBegin() const noexcept { return gsl::make_span(_indices, _indicesCount + 1).begin(); }
#else
    constexpr wchar_t* _charsBegin() noexcept { return _chars; }
    constexpr const wchar_t*  _charsBegin() const noexcept { return _chars; }

    constexpr uint16_t* _indicesBegin() noexcept { return _indices; }
    constexpr const uint16_t* _indicesBegin() const noexcept { return _indices; }
#endif
    // clang-format on

    wchar_t _charAt(size_t col) const noexcept
    {
        assert(_chars && col < _charsCapacity);
        return _chars[col];
    }

    uint16_t _indexAt(size_t col) const noexcept
    {
        assert(_indices && col <= _indicesCount);
        return _indices[col] & IndicesMask;
    }

    bool _isTrailer(size_t col) const noexcept
    {
        assert(_indices && col <= _indicesCount);
        return (_indices[col] & IndicesTrailer) == IndicesTrailer;
    }

    void _setLeaderAt(size_t col, uint16_t idx) noexcept
    {
        assert(_indices && col <= _indicesCount);
        _indices[col] = idx;
    }

    void _setTrailerAt(size_t col, uint16_t idx) noexcept
    {
        assert(_indices && col <= _indicesCount);
        _indices[col] = idx | IndicesTrailer;
    }

    uint16_t _charsSize() const noexcept
    {
        assert(_indices && _indices[_indicesCount] <= _charsCapacity);
        return _indices[_indicesCount];
    }

    void _dealloc() const noexcept;
    void _init() noexcept;
    void _resizeChars(uint16_t ch0, uint16_t ch3, size_t ch3new, uint16_t col3);

    wchar_t* _charsBuffer = nullptr;
    wchar_t* _chars = nullptr;
    uint16_t* _indices = nullptr;

    uint16_t _charsCapacity = 0;
    uint16_t _indicesCount = 0;

    til::small_rle<TextAttribute, uint16_t, 1> _attr;

    LineRendition _lineRendition = LineRendition::SingleWidth;
    // Occurs when the user runs out of text in a given row and we're forced to wrap the cursor to the next line
    bool _wrapForced = false;
    // Occurs when the user runs out of text to support a double byte character and we're forced to the next line
    bool _doubleBytePadded = false;
};

#ifdef UNIT_TESTING
constexpr bool operator==(const ROW& a, const ROW& b) noexcept
{
    // comparison is only used in the tests; this should suffice.
    return a._chars == b._chars;
}
#endif

#pragma warning(pop)
