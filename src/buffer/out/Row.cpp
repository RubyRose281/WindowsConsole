// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

#include "precomp.h"
#include "Row.hpp"
#include "textBuffer.hpp"

#include "../../types/inc/GlyphWidth.hpp"

#define ASSERT(b) do { if (!(b)) __debugbreak(); } while (false)

#if TIL_FEATURE_UNICODETEXTSEGMENTATION_ENABLED
#include <icu.h>
#pragma comment(lib, "icu.lib")

using UniqueUBreakIterator = wil::unique_any<UBreakIterator*, decltype(&ubrk_close), &ubrk_close>;
static const UniqueUBreakIterator icuBreakIterator = []() noexcept {
    UErrorCode error = U_ZERO_ERROR;
    UniqueUBreakIterator iterator{ ubrk_open(UBRK_CHARACTER, "", nullptr, 0, &error) };
    FAIL_FAST_IF_MSG(error > U_ZERO_ERROR, "ubrk_open failed with %hs", u_errorName(error));
    return iterator;
}();
#endif

#pragma warning(push, 1)

template<typename T>
constexpr uint16_t clampedUint16(T v) noexcept
{
    return static_cast<uint16_t>(std::max(0, std::min(65535, v)));
}

// Routine Description:
// - constructor
// Arguments:
// - rowWidth - the width of the row, cell elements
// - fillAttribute - the default text attribute
// Return Value:
// - constructed object
ROW::ROW(wchar_t* buffer, uint16_t* indices, const uint16_t rowWidth, const TextAttribute& fillAttribute) :
    _charsBuffer{ buffer },
    _chars{ buffer },
    _indices{ indices },
    _charsCapacity{ rowWidth },
    _indicesCount{ rowWidth },
    _attr{ rowWidth, fillAttribute }
{
    _init();
}

ROW::~ROW()
{
    _dealloc();
}

void ROW::_dealloc() const noexcept
{
    if (_chars != _charsBuffer)
    {
        delete[] _chars;
    }
}

void ROW::_init() noexcept
{
    if (_chars)
    {
        std::fill_n(_charsBegin(), _indicesCount, UNICODE_SPACE);
        std::iota(_indicesBegin(), _indicesBegin() + _indicesCount + 1, static_cast<uint16_t>(0));
    }
}

// Routine Description:
// - Sets all properties of the ROW to default values
// Arguments:
// - Attr - The default attribute (color) to fill
// Return Value:
// - <none>
bool ROW::Reset(const TextAttribute& Attr) noexcept
{
    _dealloc();

    _chars = _charsBuffer;
    _charsCapacity = _indicesCount;
    _attr = { _indicesCount, Attr };
    _lineRendition = LineRendition::SingleWidth;
    _wrapForced = false;
    _doubleBytePadded = false;

    _init();
    return true;
}

// Routine Description:
// - resizes ROW to new width
// Arguments:
// - width - the new width, in cells
// Return Value:
// - S_OK if successful, otherwise relevant error
void ROW::Resize(wchar_t* const charsBuffer, uint16_t* const indices, const uint16_t newWidth)
{
    uint16_t colsToCopy = 0;
    uint16_t charsToCopy = 0;
    if (_indices)
    {
        colsToCopy = std::min(_indicesCount, newWidth);
        charsToCopy = _indicesAt(colsToCopy);
        for (; colsToCopy != 0 && _indicesAt(colsToCopy - 1) == charsToCopy; --colsToCopy)
        {
        }
    }

    const uint16_t trailingWhitespace = newWidth - colsToCopy;
    const uint16_t charsCapacity = charsToCopy + trailingWhitespace;
    auto chars = charsBuffer;
    if (charsCapacity > newWidth)
    {
        chars = new wchar_t[charsCapacity];
    }

    {
        const auto it = std::copy_n(_charsBegin(), charsToCopy, chars);
        std::fill_n(it, trailingWhitespace, L' ');
    }
    {
        const auto it = std::copy_n(_indicesBegin(), colsToCopy, indices);
        // The _indices array is 1 wider than newWidth indicates.
        // This is because the extra column contains the past-the-end index into _chars.
        std::iota(it, it + trailingWhitespace + 1, charsToCopy);
    }

    _dealloc();

    _charsBuffer = charsBuffer;
    _chars = chars;
    _indices = indices;
    _charsCapacity = charsCapacity;
    _indicesCount = newWidth;
    _attr.resize_trailing_extent(newWidth);
}

void ROW::TransferAttributes(const til::small_rle<TextAttribute, uint16_t, 1>& attr, til::CoordType newWidth)
{
    _attr = attr;
    _attr.resize_trailing_extent(gsl::narrow<uint16_t>(newWidth));
}

// Routine Description:
// - clears char data in column in row
// Arguments:
// - column - 0-indexed column index
// Return Value:
// - <none>
void ROW::ClearCell(const til::CoordType column)
{
    static constexpr std::wstring_view space{ L" " };
    ReplaceCharacters(column, column + 1, space);
}

// Routine Description:
// - writes cell data to the row
// Arguments:
// - it - custom console iterator to use for seeking input data. bool() false when it becomes invalid while seeking.
// - index - column in row to start writing at
// - wrap - change the wrap flag if we hit the end of the row while writing and there's still more data in the iterator.
// - limitRight - right inclusive column ID for the last write in this row. (optional, will just write to the end of row if nullopt)
// Return Value:
// - iterator to first cell that was not written to this row.
OutputCellIterator ROW::WriteCells(OutputCellIterator it, const til::CoordType index, const std::optional<bool> wrap, std::optional<til::CoordType> limitRight)
{
    THROW_HR_IF(E_INVALIDARG, index >= size());
    THROW_HR_IF(E_INVALIDARG, limitRight.value_or(0) >= size());

    // If we're given a right-side column limit, use it. Otherwise, the write limit is the final column index available in the char row.
    const auto finalColumnInRow = limitRight.value_or(size() - 1);

    auto currentColor = it->TextAttr();
    uint16_t colorUses = 0;
    auto colorStarts = gsl::narrow_cast<uint16_t>(index);
    auto currentIndex = colorStarts;

    while (it && currentIndex <= finalColumnInRow)
    {
        // Fill the color if the behavior isn't set to keeping the current color.
        if (it->TextAttrBehavior() != TextAttributeBehavior::Current)
        {
            // If the color of this cell is the same as the run we're currently on,
            // just increment the counter.
            if (currentColor == it->TextAttr())
            {
                ++colorUses;
            }
            else
            {
                // Otherwise, commit this color into the run and save off the new one.
                // Now commit the new color runs into the attr row.
                ReplaceAttributes(colorStarts, currentIndex, currentColor);
                currentColor = it->TextAttr();
                colorUses = 1;
                colorStarts = currentIndex;
            }
        }

        // Fill the text if the behavior isn't set to saying there's only a color stored in this iterator.
        if (it->TextAttrBehavior() != TextAttributeBehavior::StoredOnly)
        {
            const auto fillingLastColumn = currentIndex == finalColumnInRow;
            const auto attr = it->DbcsAttr();
            const auto& chars = it->Chars();

            if (attr.IsSingle())
            {
                ReplaceCharacters(currentIndex, currentIndex + 1, chars);
                ++it;
            }
            else if (attr.IsLeading())
            {
                if (fillingLastColumn)
                {
                    // If we're trying to fill the last cell with a leading byte, pad it out instead by clearing it.
                    // Don't increment iterator. We'll exit because we couldn't write a lead at the end of a line.
                    ClearCell(currentIndex);
                    SetDoubleBytePadded(true);
                }
                else
                {
                    ReplaceCharacters(currentIndex, currentIndex + 2, chars);
                    ++it;
                }
            }
            else
            {
                ++it;
            }

            // If we're asked to (un)set the wrap status and we just filled the last column with some text...
            // NOTE:
            //  - wrap = std::nullopt    --> don't change the wrap value
            //  - wrap = true            --> we're filling cells as a steam, consider this a wrap
            //  - wrap = false           --> we're filling cells as a block, unwrap
            if (wrap.has_value() && fillingLastColumn)
            {
                // set wrap status on the row to parameter's value.
                SetWrapForced(*wrap);
            }
        }
        else
        {
            ++it;
        }

        // Move to the next cell for the next time through the loop.
        ++currentIndex;
    }

    // Now commit the final color into the attr row
    if (colorUses)
    {
        ReplaceAttributes(colorStarts, currentIndex, currentColor);
    }

    return it;
}

bool ROW::SetAttrToEnd(const til::CoordType beginIndex, const TextAttribute attr)
{
    _attr.replace(clampedUint16(beginIndex), _attr.size(), attr);
    return true;
}

void ROW::ReplaceAttributes(const til::CoordType beginIndex, const til::CoordType endIndex, const TextAttribute& newAttr)
{
    _attr.replace(clampedUint16(beginIndex), clampedUint16(endIndex), newAttr);
}

til::CoordType ROW::ReplaceCharacters(til::CoordType beginIndex, std::wstring_view& text)
try
{
#if TIL_FEATURE_UNICODETEXTSEGMENTATION_ENABLED
    const auto col1 = clampedUint16(beginIndex);

    if ((col1 >= _indicesCount) | text.empty())
    {
        return col1;
    }

    ASSERT(_charsCapacity < 256);
    ASSERT(_indicesCount < 256);
    wchar_t charsBackup[256];
    uint16_t indicesBackup[256];
    std::copy_n(_chars, _charsCapacity, &charsBackup[0]);
    std::copy_n(_indices, _indicesCount, &indicesBackup[0]);

    uint16_t col0 = col1;
    const uint16_t ch0 = _indicesAt(col0);
    for (; col0 != 0 && _indicesAt(col0 - 1) == ch0; --col0)
    {
    }
    const uint16_t leadingSpaces = col1 - col0;

    const uint16_t ch1 = ch0 + leadingSpaces;
    uint16_t ch2 = ch1;
    uint16_t col2 = col1;
    uint16_t paddingSpaces = 0;
    uint16_t ch3ref = 0;

    const auto beg = text.begin();
    const auto end = text.end();
    auto it = beg;
    {
        // ASCII "fast" pass
        const auto asciiMax = beg + std::min<size_t>(_indicesCount - col2, text.size());
        const auto asciiEnd = std::find_if(beg, asciiMax, [](const auto& ch) { return ch >= 0x80; });
        for (; it != asciiEnd; ++it, ++col2, ++ch2)
        {
            ch3ref = _indicesAt(col2);
            _indicesAt(col2) = ch2;
        }

        // Regular Unicode processing
        if (it != asciiMax)
        {
            if (it != beg)
            {
                --it;
                --col2;
                --ch2;
            }
            paddingSpaces = _processUnicode(it, end, col2, ch2, ch3ref);
        }
    }

    uint16_t col3 = col2 - 1 + paddingSpaces;
    uint16_t ch3;
    {
        while ((ch3 = _indicesAt(++col3)) == ch3ref)
        {
        }
    }
    const uint16_t trailingSpaces = col3 - col2;

    const size_t copiedChars = ch2 - ch1;
    const size_t insertedChars = copiedChars + leadingSpaces + trailingSpaces;
    const size_t ch3new = insertedChars + ch0;

    if (ch3new != ch3)
    {
        _resizeChars(ch0, ch3, ch3new, col3);
    }

    {
        std::fill_n(_charsBegin() + ch0, leadingSpaces, L' ');
        std::iota(_indicesBegin() + col0, _indicesBegin() + col1, ch0);

        std::copy_n(text.data(), copiedChars, _charsBegin() + ch1);

        std::fill_n(_charsBegin() + ch2, trailingSpaces, L' ');
        std::iota(_indicesBegin() + col2, _indicesBegin() + col3 + 1, ch2);
    }

    ASSERT(_indicesAt(0) == 0);

    for (uint16_t i = 0; i <= _indicesCount; ++i)
    {
        ASSERT(_indices[i] <= _charsCapacity);
        if (i)
        {
            const auto delta = _indices[i] - _indices[i - 1];
            ASSERT(delta >= 0 && delta <= 20);
        }
    }

    {
        auto it = CharsBegin();
        const auto end = CharsEnd();

        UErrorCode error = U_ZERO_ERROR;
        ubrk_setText(icuBreakIterator.get(), reinterpret_cast<const char16_t*>(_chars), _indicesAt(_indicesCount), &error);
        THROW_HR_IF_MSG(E_UNEXPECTED, error > U_ZERO_ERROR, "ubrk_setText failed with %hs", u_errorName(error));

        for (int32_t ubrk0 = 0, ubrk1; (ubrk1 = ubrk_next(icuBreakIterator.get())) != UBRK_DONE; ubrk0 = ubrk1)
        {
            ASSERT(it != end);

            const auto expectedCols = it.Cols();
            const auto expectedText = it.Text();
            const auto advance = clampedUint16(ubrk1 - ubrk0);
            const auto width = 1 + IsGlyphFullWidth({ _chars + ubrk0, advance });

            ASSERT(width == expectedCols);
            ASSERT(expectedText.data() == _chars + ubrk0 && expectedText.size() == advance);

            ++it;
        }

        ASSERT(it == end);
    }

    text = { it, end };
    return col3;
#else
    UNREFERENCED_PARAMETER(beginIndex);
    UNREFERENCED_PARAMETER(text);
    abort();
#endif
}
catch (...)
{
    // Due to this function writing _indices first, then calling _processUnicode/_resizeChars
    // (which may throw) and only then finally filling in _chars, we might end up
    // in a situation were _indices contains offsets outside of the _chars array.
    // --> Restore this row to a known "okay"-state.
    Reset(TextAttribute{});
    throw;
}

uint16_t ROW::_processUnicode(std::wstring_view::iterator& it, std::wstring_view::iterator end, uint16_t& col2, uint16_t& ch2, uint16_t& ch3ref)
{
#if TIL_FEATURE_UNICODETEXTSEGMENTATION_ENABLED
    const auto text = reinterpret_cast<const char16_t*>(&*it);
    const auto textLength = gsl::narrow<int32_t>(end - it);
    uint16_t paddingSpaces = 0;

    UErrorCode error = U_ZERO_ERROR;
    ubrk_setText(icuBreakIterator.get(), text, textLength, &error);
    THROW_HR_IF_MSG(E_UNEXPECTED, error > U_ZERO_ERROR, "ubrk_setText failed with %hs", u_errorName(error));

    for (int32_t ubrk0 = 0, ubrk1; (ubrk1 = ubrk_next(icuBreakIterator.get())) != UBRK_DONE; ubrk0 = ubrk1)
    {
        const auto advance = clampedUint16(ubrk1 - ubrk0);
        //(UEastAsianWidth)u_getIntPropertyValue(input, UCHAR_EAST_ASIAN_WIDTH);
        auto width = 1 + IsGlyphFullWidth({ &*it, advance });

        if (width >= _indicesCount - col2)
        {
            SetDoubleBytePadded(true);
            // Normally this should be something like `col2 + width - _indicesCount`.
            // But `width` can only ever be either 1 or 2 which means paddingSpaces can only be 1.
            paddingSpaces = 1;
            break;
        }

        do
        {
            ch3ref = _indicesAt(col2);
            _indicesAt(col2) = ch2;
            ASSERT(_indicesAt(0) == 0);
            ASSERT(ch2 < 1000);
            ASSERT(col2 < _indicesCount);
            ++col2;
        } while (--width);

        ASSERT(static_cast<uint16_t>(ch2 + advance) > ch2);

        it += advance;
        ch2 += advance;
    }

    return paddingSpaces;
#else
    UNREFERENCED_PARAMETER(it);
    UNREFERENCED_PARAMETER(end);
    UNREFERENCED_PARAMETER(col2);
    UNREFERENCED_PARAMETER(ch2);
    abort();
#endif
}

void ROW::ReplaceCharacters(til::CoordType beginIndex, til::CoordType endIndex, const std::wstring_view& chars)
{
    const auto col1 = clampedUint16(beginIndex);
    const auto col2 = clampedUint16(endIndex);

    if ((col1 >= col2) | (col2 > _indicesCount) | chars.empty())
    {
        return;
    }

    uint16_t col0 = col1;
    const uint16_t ch0 = _indicesAt(col0);
    for (; col0 != 0 && _indicesAt(col0 - 1) == ch0; --col0)
    {
    }

    uint16_t col3 = col2 - 1;
    uint16_t ch3;
    {
        const uint16_t ch3ref = _indicesAt(col3);
        while ((ch3 = _indicesAt(++col3)) == ch3ref)
        {
        }
    }

    const size_t leadingSpaces = col1 - col0;
    const size_t trailingSpaces = col3 - col2;
    const size_t insertedChars = chars.size() + leadingSpaces + trailingSpaces;
    const size_t newCh1 = insertedChars + ch0;

    if (newCh1 != ch3)
    {
        _resizeChars(ch0, ch3, newCh1, col3);
    }

    {
        auto ch = _charsBegin() + ch0;
        auto in0 = _indicesBegin() + col0;
        const auto in1 = _indicesBegin() + col1;
        auto in2 = _indicesBegin() + col2;
        const auto in3 = _indicesBegin() + col3;
        auto chPos = ch0;

        for (; in0 != in1; ++ch, ++in0, ++chPos)
        {
            *ch = L' ';
            *in0 = chPos;
        }

        ch = std::copy_n(chars.data(), chars.size(), ch);
        std::fill(in1, in2, chPos);
        chPos += chars.size();

        for (; in2 != in3; ++ch, ++in2, ++chPos)
        {
            *ch = L' ';
            *in2 = chPos;
        }
    }
}

void ROW::_resizeChars(uint16_t ch0, uint16_t ch3, size_t ch3new, uint16_t col3)
{
    const auto diff = ch3new - ch3;
    const auto currentLength = _indicesAt(_indicesCount);
    const auto newLength = currentLength + diff;

    if (newLength <= _charsCapacity)
    {
        std::copy_n(_charsBegin() + ch3, currentLength - ch3, _charsBegin() + ch3new);
    }
    else
    {
        const auto minCapacity = static_cast<size_t>(_charsCapacity) + (_charsCapacity >> 1);
        const auto newCapacity = gsl::narrow<uint16_t>(std::max(newLength, minCapacity));
        const auto chars = new wchar_t[newCapacity];

        std::copy_n(_charsBegin(), ch0, chars);
        std::copy_n(_charsBegin() + ch3, currentLength - ch3, chars + ch3new);

        if (_chars != _charsBuffer)
        {
            delete[] _chars;
        }

        _chars = chars;
        _charsCapacity = newCapacity;
    }

    for (auto it = &_indices[col3], end = &_indices[_indicesCount + 1]; it != end; ++it)
    {
        *it += diff;
    }
}

const til::small_rle<TextAttribute, uint16_t, 1>& ROW::Attributes() const noexcept
{
    return _attr;
}

TextAttribute ROW::GetAttrByColumn(const til::CoordType column) const
{
    return _attr.at(clampedUint16(column));
}

std::vector<uint16_t> ROW::GetHyperlinks() const
{
    std::vector<uint16_t> ids;
    for (const auto& run : _attr.runs())
    {
        if (run.value.IsHyperlink())
        {
            ids.emplace_back(run.value.GetHyperlinkId());
        }
    }
    return ids;
}

uint16_t ROW::size() const noexcept
{
    return _indicesCount;
}

til::CoordType ROW::MeasureLeft() const noexcept
{
    const auto beg = _charsBegin();
    const auto end = beg + _indicesAt(_indicesCount);
    auto it = beg;

    for (; it != end; ++it)
    {
        if (*it != L' ')
        {
            break;
        }
    }

    return static_cast<til::CoordType>(it - beg);
}

til::CoordType ROW::MeasureRight() const noexcept
{
    const auto beg = _charsBegin();
    const auto end = beg + _indicesAt(_indicesCount);
    auto it = end;

    for (; it != beg; --it)
    {
        if (it[-1] != L' ')
        {
            break;
        }
    }

    // We're supposed to return the measurement in cells and not characters
    // and therefore simply calculating `it - beg` would be wrong.
    //
    // An example: The row is 10 cells wide and `it` points to the second character.
    // `it - beg` would return 1, but it's possible it's actually 1 wide glyph and 8 whitespaces.
    return static_cast<til::CoordType>(_indicesCount - (end - it));
}

bool ROW::ContainsText() const noexcept
{
    auto it = _charsBegin();
    const auto end = it + _indicesAt(_indicesCount);

    for (; it != end; ++it)
    {
        if (*it != L' ')
        {
            return true;
        }
    }

    return false;
}

std::wstring_view ROW::GlyphAt(til::CoordType column) const noexcept
{
    column = std::min(column, _indicesCount - 1);

    const auto current = _indicesAt(column);
    while (column <= _indicesCount && _indicesAt(++column) == current)
    {
    }

    const auto len = gsl::narrow_cast<size_t>(_indicesAt(column) - current);
    return { _chars + current, len };
}

DbcsAttribute ROW::DbcsAttrAt(til::CoordType column) const noexcept
{
    column = std::min(column, _indicesCount - 1);

    const auto idx = _indicesAt(column);

    auto attr = DbcsAttribute::Attribute::Single;
    if (column > 0 && _indicesAt(column - 1) == idx)
    {
        attr = DbcsAttribute::Attribute::Trailing;
    }
    else if (column < _indicesCount && _indicesAt(column + 1) == idx)
    {
        attr = DbcsAttribute::Attribute::Leading;
    }

    return { attr };
}

std::wstring_view ROW::GetText() const noexcept
{
    return { _chars, _indicesAt(_indicesCount) };
}

DelimiterClass ROW::DelimiterClassAt(til::CoordType column, const std::wstring_view& wordDelimiters) const noexcept
{
    column = std::min(column, _indicesCount - 1);

    const auto glyph = _charsAt(_indicesAt(column));

    if (glyph <= L' ')
    {
        return DelimiterClass::ControlChar;
    }
    else if (wordDelimiters.find(glyph) != std::wstring_view::npos)
    {
        return DelimiterClass::DelimiterChar;
    }
    else
    {
        return DelimiterClass::RegularChar;
    }
}

RowTextIterator ROW::CharsBegin() const noexcept
{
    return { _chars, _indices, _indicesCount, 0, 0 };
}

RowTextIterator ROW::CharsEnd() const noexcept
{
    return { _chars, _indices, _indicesCount, _indicesCount, _indicesCount };
}

#pragma warning(pop)
