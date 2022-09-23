// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

#include "dwrite.hlsl"

Texture2D<float4> glyphAtlas : register(t0);

// clang-format off
float4 main(
    float4 position : SV_Position,
    float2 texCoord : TEXCOORD,
    float4 color : COLOR
) : SV_Target
// clang-format on
{
    return glyphAtlas[texCoord];
}
