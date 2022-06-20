// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

#include "precomp.h"
#include "Row.hpp"
#include "textBuffer.hpp"

#include "../../types/inc/GlyphWidth.hpp"
#include "../../types/inc/Utf16Parser.hpp"

#if TIL_FEATURE_UNICODETEXTSEGMENTATION_ENABLED
#include <icu.h>
#pragma comment(lib, "icu.lib")

using UniqueUBreakIterator = wil::unique_any<UBreakIterator*, decltype(&ubrk_close), &ubrk_close>;
static const UniqueUBreakIterator icuBreakIterator = []() noexcept {
    UErrorCode error = U_ZERO_ERROR;
    return UniqueUBreakIterator{ ubrk_open(UBRK_CHARACTER, "", nullptr, 0, &error) };
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

void ROW::_init() const noexcept
{
    if (_chars)
    {
        std::fill_n(_chars, _indicesCount, UNICODE_SPACE);
        std::iota(_indices, _indices + _indicesCount + 1, static_cast<uint16_t>(0));
    }
}

// Routine Description:
// - Sets all properties of the ROW to default values
// Arguments:
// - Attr - The default attribute (color) to fill
// Return Value:
// - <none>
bool ROW::Reset(const TextAttribute& Attr)
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
        charsToCopy = _indices[colsToCopy];
        for (; colsToCopy != 0 && _indices[colsToCopy - 1] == charsToCopy; --colsToCopy)
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
        const auto it = std::copy_n(_chars, charsToCopy, chars);
        std::fill_n(it, trailingWhitespace, L' ');
    }
    {
        const auto it = std::copy_n(_indices, colsToCopy, indices);
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

til::CoordType ROW::ReplaceCharacters(til::CoordType beginIndex, const std::wstring_view& text)
{
    const auto col1 = clampedUint16(beginIndex);

    if ((col1 >= _indicesCount) | text.empty())
    {
        return col1;
    }

    auto it = text.begin();
    const auto end = text.end();

    uint16_t col0 = col1;
    const uint16_t ch0 = _indices[col0];
    for (; col0 != 0 && _indices[col0 - 1] == ch0; --col0)
    {
    }
    const uint16_t leadingSpaces = col1 - col0;

    const uint16_t ch1 = ch0 + leadingSpaces;
    uint16_t ch2 = ch1;
    uint16_t col2 = col1;

#if TIL_FEATURE_UNICODETEXTSEGMENTATION_ENABLED
    {
        do
        {
            if (*it >= 0x80)
            {
                break;
            }

            _indices[col2] = ch2;
            ++it;
            ++col2;
            ++ch2;
        } while (it != end);

        if (it != end)
        {
            const auto text = reinterpret_cast<const char16_t*>(&*it);
            const auto textLength = gsl::narrow<int32_t>(end - it);
            UErrorCode error = U_ZERO_ERROR;
            ubrk_setText(icuBreakIterator.get(), text, textLength, &error);

            for (int32_t ubrk0 = 0, ubrk1; (ubrk1 = ubrk_next(icuBreakIterator.get())) != UBRK_DONE; ubrk0 = ubrk1)
            {
                const size_t advance = ubrk1 - ubrk0;
                auto width = 1 + IsGlyphFullWidth({ &*it, advance });
                if (width > _indicesCount - col2)
                {
                    // TODO: ClearCell(currentIndex); SetDoubleBytePadded(true);
                    break;
                }

                do
                {
                    _indices[col2++] = ch2;
                } while (--width);

                it += advance;
                ch2 += advance;
            }
        }
    }
#else
    {
        const auto last = end - 1;

        do
        {
            if (*it >= 0x80)
            {
                break;
            }

            _indices[col2] = ch2;
            ++it;
            ++col2;
            ++ch2;
        } while (it != end);

        while (it != end)
        {
            uint16_t advance = 1;
            if (Utf16Parser::IsLeadingSurrogate(*it) && it != last)
            {
                ++advance;
            }

            auto width = 1 + IsGlyphFullWidth({ &*it, advance });
            if (width > _indicesCount - col2)
            {
                // TODO: ClearCell(currentIndex); SetDoubleBytePadded(true);
                break;
            }

            do
            {
                _indices[col2++] = ch2;
            } while (--width);

            it += advance;
            ch2 += advance;
        }
    }
#endif

    uint16_t col3 = col2 - 1;
    uint16_t ch3;
    {
        const uint16_t ch3ref = _indices[col3];
        while ((ch3 = _indices[++col3]) == ch3ref)
        {
        }
    }
    const uint16_t trailingSpaces = col3 - col2;

    const size_t insertedChars = text.size() + leadingSpaces + trailingSpaces;
    const size_t ch3new = insertedChars + ch0;

    if (ch3new != ch3)
    {
        _resizeChars(ch0, ch3, ch3new, col3);
    }

    {
        auto ch = _chars + ch0;
        auto in0 = _indices + col0;
        const auto in1 = _indices + col1;
        auto in2 = _indices + col2;
        const auto in3 = _indices + col3;
        auto chPos = ch0;

        for (; in0 != in1; ++ch, ++in0, ++chPos)
        {
            *ch = L' ';
            *in0 = chPos;
        }

        ch = std::copy_n(text.data(), text.size(), ch);
        chPos += text.size();

        for (; in2 != in3; ++ch, ++in2, ++chPos)
        {
            *ch = L' ';
            *in2 = chPos;
        }
    }

    return col3;
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
    const uint16_t ch0 = _indices[col0];
    for (; col0 != 0 && _indices[col0 - 1] == ch0; --col0)
    {
    }

    uint16_t col3 = col2 - 1;
    uint16_t ch3;
    {
        const uint16_t ch3ref = _indices[col3];
        while ((ch3 = _indices[++col3]) == ch3ref)
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
        auto ch = _chars + ch0;
        auto in0 = _indices + col0;
        const auto in1 = _indices + col1;
        auto in2 = _indices + col2;
        const auto in3 = _indices + col3;
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
    const auto currentLength = _indices[_indicesCount];
    const auto newLength = currentLength + diff;

    if (newLength <= _charsCapacity)
    {
        std::copy_n(_chars + ch3, currentLength - ch3, _chars + ch3new);
    }
    else
    {
        const auto minCapacity = static_cast<size_t>(_charsCapacity) + (_charsCapacity >> 1);
        const auto newCapacity = gsl::narrow<uint16_t>(std::max(newLength, minCapacity));
        const auto chars = new wchar_t[newCapacity];

        std::copy_n(_chars, ch0, chars);
        std::copy_n(_chars + ch3, currentLength - ch3, chars + ch3new);

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
    const auto beg = _chars;
    const auto end = beg + _indices[_indicesCount];
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
    const auto beg = _chars;
    const auto end = beg + _indices[_indicesCount];
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
    auto it = _chars;
    const auto end = it + _indices[_indicesCount];

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

    const auto current = _indices[column];
    while (column <= _indicesCount && _indices[++column] == current)
    {
    }

    const auto len = gsl::narrow_cast<size_t>(_indices[column] - current);
    return { _chars + current, len };
}

DbcsAttribute ROW::DbcsAttrAt(til::CoordType column) const noexcept
{
    column = std::min(column, _indicesCount - 1);

    const auto idx = _indices[column];

    auto attr = DbcsAttribute::Attribute::Single;
    if (column > 0 && _indices[column - 1] == idx)
    {
        attr = DbcsAttribute::Attribute::Trailing;
    }
    else if (column < _indicesCount && _indices[column + 1] == idx)
    {
        attr = DbcsAttribute::Attribute::Leading;
    }

    return { attr };
}

std::wstring_view ROW::GetText() const noexcept
{
    return { _chars, _indices[_indicesCount] };
}

DelimiterClass ROW::DelimiterClassAt(til::CoordType column, const std::wstring_view& wordDelimiters) const noexcept
{
    column = std::min(column, _indicesCount - 1);

    const auto glyph = _chars[_indices[column]];

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
