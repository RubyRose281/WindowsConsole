// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

#include "dwrite.hlsl"

cbuffer ConstBuffer : register(b0)
{
    float4 gammaRatios;
    float cleartypeEnhancedContrast;
    float grayscaleEnhancedContrast;
};

struct Output
{
    float4 color0;
    float4 color1;
};

Texture2D<float4> glyphAtlas : register(t0);

// clang-format off
Output main(
    float4 position : SV_Position,
    float2 texCoord : TEXCOORD,
    float4 color : COLOR
) : SV_Target
// clang-format on
{
    float3 foregroundStraight = DWrite_UnpremultiplyColor(color);
    float blendEnhancedContrast = DWrite_ApplyLightOnDarkContrastAdjustment(cleartypeEnhancedContrast, foregroundStraight);
    float3 contrasted = DWrite_EnhanceContrast3(glyphAtlas[texCoord].rgb, blendEnhancedContrast);
    float3 alphaCorrected = DWrite_ApplyAlphaCorrection3(contrasted, foregroundStraight, gammaRatios);
    Output output;
    output.color0 = float4(foregroundStraight, 1);
    output.color1 = float4(alphaCorrected * color.a, 1);
    return output;
}
