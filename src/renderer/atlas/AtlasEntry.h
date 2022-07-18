#pragma once

#include "Cell.h"

namespace Microsoft::Console::Render::Atlas
{
    enum class AtlasEntryKeyAttributes : u16
    {
        None = 0x0,
        Intense = 0x1,
        Italic = 0x2,
        // The Intense and Italic flags are used to directly index into arrays.
        // If you ever add more flags here, make sure to fix _getTextFormat()
        // and _getTextFormatAxis() and to add a `& 3` mask for instance.
    };
    ATLAS_FLAG_OPS(AtlasEntryKeyAttributes, u16)

    // AtlasEntryKey will be hashed as a series of u32 values.
    struct alignas(u32) AtlasEntryKey
    {
        AtlasEntryKeyAttributes attributes;
        u16 charCount;
        u16 coordCount;
        wchar_t chars[1];
    };

    struct alignas(u32) AtlasEntryValue
    {
        CellFlags flags = CellFlags::None;
        u16x2 coords[1];
    };

    struct AtlasEntry
    {
        // map linked list
        AtlasEntry* next;
        AtlasEntry* prev;

        // LRU linked list
        AtlasEntry* newer;
        AtlasEntry* older;

        AtlasEntryValue* value;

        u32 hash;
        AtlasEntryKey key;
        // Past this point we transparently store .key.chars[1] and following on the heap.
        // Past that we store the AtlasEntryValue.

        static constexpr size_t calculateKeySize(size_t charCount) noexcept
        {
            return nextMultipleOf(sizeof(AtlasEntryKey) - sizeof(AtlasEntryKey::chars) + charCount * sizeof(AtlasEntryKey::chars[0]), sizeof(u32));
        }

        static constexpr size_t calculateValueSize(size_t coordCount) noexcept
        {
            return sizeof(AtlasEntryValue) - sizeof(AtlasEntryValue::coords) + coordCount * sizeof(AtlasEntryValue::coords[0]);
        }

        static constexpr size_t calculateEntrySize(size_t charCount, size_t coordCount) noexcept
        {
            return offsetof(AtlasEntry, key) + calculateKeySize(charCount) + calculateValueSize(coordCount);
        }

    private:
        static constexpr size_t nextMultipleOf(size_t n, size_t powerOf2) noexcept
        {
            return (n + powerOf2 - 1) & ~(powerOf2 - 1);
        }
    };
}
