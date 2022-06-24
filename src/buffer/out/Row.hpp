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
    RowTextIterator(wchar_t* chars, uint16_t* indices, uint16_t cols, uint16_t beg, uint16_t end) noexcept :
        _chars{ chars },
        _indices{ indices },
        _cols{ cols },
        _beg{ beg },
        _end{ end }
    {
        operator++();
    }

    bool operator==(const RowTextIterator& other) const noexcept
    {
        return _beg == other._beg;
    }

    RowTextIterator& operator++()
    {
        _beg = _end;

        const auto current = _indices[_end];
        while (_end < _cols && _indices[++_end] == current)
        {
        }

        return *this;
    }

    const RowTextIterator& operator*() const noexcept
    {
        return *this;
    }

    std::wstring_view Text() const noexcept
    {
        return { _chars + _indices[_beg], gsl::narrow_cast<size_t>(_indices[_end] - _indices[_beg]) };
    }

    til::CoordType Cols() const noexcept
    {
        return _end - _beg;
    }

    DbcsAttribute DbcsAttr() const noexcept
    {
        return Cols() == 2 ? DbcsAttribute::Attribute::Leading : DbcsAttribute::Attribute::Single;
    }

private:
    const wchar_t* _chars;
    const uint16_t* _indices;
    uint16_t _cols;
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
    // clang-format off
#ifndef NDEBUG
    constexpr gsl::span<wchar_t>::iterator _charsBegin() noexcept { return gsl::make_span(_chars, _charsCapacity).begin(); }
    constexpr gsl::span<const wchar_t>::iterator _charsBegin() const noexcept { return gsl::make_span(_chars, _charsCapacity).begin(); }

    constexpr gsl::span<uint16_t>::iterator _indicesBegin() noexcept { return gsl::make_span(_indices, _indicesCount + 1).begin(); }
    constexpr gsl::span<const uint16_t>::iterator _indicesBegin() const noexcept { return gsl::make_span(_indices, _indicesCount + 1).begin(); }

    wchar_t& _charsAt(size_t idx) noexcept { Expects(_chars && idx < _charsCapacity); return _chars[idx]; }
    const wchar_t& _charsAt(size_t idx) const noexcept { Expects(_chars && idx < _charsCapacity); return _chars[idx]; }
    
    uint16_t& _indicesAt(size_t idx) noexcept { Expects(_indices && idx < _indicesCount + 1); return _indices[idx]; }
    const uint16_t& _indicesAt(size_t idx) const noexcept { Expects(_indices && idx < _indicesCount + 1); return _indices[idx]; }
#else
    constexpr wchar_t* _charsBegin() noexcept { return _chars; }
    constexpr const wchar_t*  _charsBegin() const noexcept { return _chars; }

    constexpr uint16_t* _indicesBegin() noexcept { return _indices; }
    constexpr const uint16_t* _indicesBegin() const noexcept { return _indices; }

    wchar_t& _charsAt(size_t idx) noexcept { return _chars[idx]; }
    const wchar_t& _charsAt(size_t idx) const noexcept { return _chars[idx]; }
    
    uint16_t& _indicesAt(size_t idx) noexcept { return _indices[idx]; }
    const uint16_t& _indicesAt(size_t idx) const noexcept { return _indices[idx]; }
#endif
    // clang-format on

    void _dealloc() const noexcept;
    void _init() noexcept;
    uint16_t _processUnicode(std::wstring_view::iterator& it, std::wstring_view::iterator end, uint16_t& col2, uint16_t& ch2, uint16_t& ch3ref);
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
