// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

#include "dwrite.hlsl"

cbuffer ConstBuffer : register(b0)
{
    float4 gammaRatios;
    float grayscaleEnhancedContrast;
};

Texture2D<float4> glyphAtlas : register(t0);

// clang-format off
float4 main(
    float4 position : SV_Position,
    float2 texCoord : TEXCOORD,
    float4 color : COLOR
) : SV_Target
// clang-format on
{
    return DWrite_GrayscaleBlend(gammaRatios, grayscaleEnhancedContrast, false, color, glyphAtlas[texCoord].a);
}
