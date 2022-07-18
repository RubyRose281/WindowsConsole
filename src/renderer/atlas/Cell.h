#pragma once

#include "util.h"

namespace Microsoft::Console::Render::Atlas
{
    // These flags are shared with shader_ps.hlsl.
    // If you change this be sure to copy it over to shader_ps.hlsl.
    //
    // clang-format off
    enum class CellFlags : u32
    {
        None            = 0x00000000,

        ColoredGlyph    = 0x00000001,

        Cursor          = 0x00000002,
        Selected        = 0x00000004,

        BorderLeft      = 0x00000008,
        BorderTop       = 0x00000010,
        BorderRight     = 0x00000020,
        BorderBottom    = 0x00000040,
        Underline       = 0x00000080,
        UnderlineDotted = 0x00000100,
        UnderlineDouble = 0x00000200,
        Strikethrough   = 0x00000400,
    };
    // clang-format on
    ATLAS_FLAG_OPS(CellFlags, u32)

    // This structure is shared with the GPU shader and needs to follow certain alignment rules.
    // You can generally assume that only u32 or types of that alignment are allowed.
    struct Cell
    {
        alignas(u32) u16x2 tileIndex;
        alignas(u32) CellFlags flags = CellFlags::None;
        u32x2 color;
    };
}
