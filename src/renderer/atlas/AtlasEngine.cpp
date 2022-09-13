// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

#include "pch.h"
#include "AtlasEngine.h"

#include "dwrite.h"
#include "../../interactivity/win32/CustomWindowMessages.h"

#include <custom_shader_ps.h>
#include <custom_shader_vs.h>

// #### NOTE ####
// This file should only contain methods that are only accessed by the caller of Present() (the "Renderer" class).
// Basically this file poses the "synchronization" point between the concurrently running
// general IRenderEngine API (like the Invalidate*() methods) and the Present() method
// and thus may access both _r and _api.

#pragma warning(disable : 4100) // '...': unreferenced formal parameter
// Disable a bunch of warnings which get in the way of writing performant code.
#pragma warning(disable : 26429) // Symbol 'data' is never tested for nullness, it can be marked as not_null (f.23).
#pragma warning(disable : 26446) // Prefer to use gsl::at() instead of unchecked subscript operator (bounds.4).
#pragma warning(disable : 26459) // You called an STL function '...' with a raw pointer parameter at position '...' that may be unsafe [...].
#pragma warning(disable : 26481) // Don't use pointer arithmetic. Use span instead (bounds.1).
#pragma warning(disable : 26482) // Only index into arrays using constant expressions (bounds.2).

using namespace Microsoft::Console::Render::Atlas;

#pragma warning(suppress : 26455) // Default constructor may not throw. Declare it 'noexcept' (f.6).
AtlasEngine::AtlasEngine()
{
#ifdef NDEBUG
    THROW_IF_FAILED(D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED, _p.d2dFactory.addressof()));
#else
    static constexpr D2D1_FACTORY_OPTIONS options{ D2D1_DEBUG_LEVEL_INFORMATION };
    THROW_IF_FAILED(D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED, options, _p.d2dFactory.addressof()));
#endif

    THROW_IF_FAILED(DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED, __uuidof(_p.dwriteFactory), _p.dwriteFactory.put_unknown()));
    THROW_IF_FAILED(_p.dwriteFactory->GetSystemFontFallback(_p.systemFontFallback.addressof()));

    {
        wil::com_ptr<IDWriteTextAnalyzer> textAnalyzer;
        THROW_IF_FAILED(_p.dwriteFactory->CreateTextAnalyzer(textAnalyzer.addressof()));
        _p.textAnalyzer = textAnalyzer.query<IDWriteTextAnalyzer1>();
    }

    DWrite_GetRenderParams(_p.dwriteFactory.get(), &_p.gamma, &_p.cleartypeEnhancedContrast, &_p.grayscaleEnhancedContrast, _p.renderingParams.addressof());
}

#pragma region IRenderEngine

// StartPaint() is called while the console buffer lock is being held.
// --> Put as little in here as possible.
[[nodiscard]] HRESULT AtlasEngine::StartPaint() noexcept
try
{
    if (_api.invalidatedTitle)
    {
        LOG_IF_WIN32_BOOL_FALSE(PostMessageW(_api.s->target->hwnd, CM_UPDATE_TITLE, 0, 0));
        _api.invalidatedTitle = false;
    }

    if (_p.s != _api.s) [[unlikely]]
    {
        const auto fontChanged = _p.s->font != _api.s->font;
        const auto targetSizeChanged = _p.s->targetSize != _api.s->targetSize;
        const auto cellCountChanged = _p.s->cellCount != _api.s->cellCount;

        _p.s = _api.s;

        if (fontChanged)
        {
            _recreateFontDependentResources();
        }
        // This block has to be after _recreateFontDependentResources(),
        // because it accesses _p.tileAllocator which is only created there.
        if (targetSizeChanged)
        {
            _p.tileAllocator.setMaxArea(_p.s->targetSize);
        }
        if (fontChanged || cellCountChanged)
        {
            _recreateSizeDependentResources();
        }

        _api.invalidatedRows = invalidatedRowsAll;
    }

    if constexpr (debugGlyphGenerationPerformance)
    {
        _p.glyphs = {};
        _p.tileAllocator = TileAllocator{ _p.s->font->cellSize };
        _p.tileAllocator.setMaxArea(_p.s->targetSize);
    }
    if constexpr (debugTextParsingPerformance)
    {
        _api.invalidatedRows = invalidatedRowsAll;
        _api.scrollOffset = 0;
    }

    // Clamp invalidation rects into valid value ranges.
    {
        _api.invalidatedCursorArea.left = std::min(_api.invalidatedCursorArea.left, _api.s->cellCount.x);
        _api.invalidatedCursorArea.top = std::min(_api.invalidatedCursorArea.top, _api.s->cellCount.y);
        _api.invalidatedCursorArea.right = clamp(_api.invalidatedCursorArea.right, _api.invalidatedCursorArea.left, _api.s->cellCount.x);
        _api.invalidatedCursorArea.bottom = clamp(_api.invalidatedCursorArea.bottom, _api.invalidatedCursorArea.top, _api.s->cellCount.y);
    }
    {
        _api.invalidatedRows.x = std::min(_api.invalidatedRows.x, _api.s->cellCount.y);
        _api.invalidatedRows.y = clamp(_api.invalidatedRows.y, _api.invalidatedRows.x, _api.s->cellCount.y);
    }
    {
        const auto limit = gsl::narrow_cast<i16>(_api.s->cellCount.y & 0x7fff);
        _api.scrollOffset = gsl::narrow_cast<i16>(clamp<int>(_api.scrollOffset, -limit, limit));
    }

    // Scroll the buffer by the given offset and mark the newly uncovered rows as "invalid".
    if (_api.scrollOffset != 0)
    {
        const auto nothingInvalid = _api.invalidatedRows.x == _api.invalidatedRows.y;
        const auto offset = static_cast<ptrdiff_t>(_api.scrollOffset) * _api.s->cellCount.x;

        if (_api.scrollOffset < 0)
        {
            // Scroll up (for instance when new text is being written at the end of the buffer).
            const u16 endRow = _api.s->cellCount.y + _api.scrollOffset;
            _api.invalidatedRows.x = nothingInvalid ? endRow : std::min<u16>(_api.invalidatedRows.x, endRow);
            _api.invalidatedRows.y = _api.s->cellCount.y;

            // scrollOffset/offset = -1
            // +----------+    +----------+
            // |          |    | xxxxxxxxx|         + dst  < beg
            // | xxxxxxxxx| -> |xxxxxxx   |  + src  |      < beg - offset
            // |xxxxxxx   |    |          |  |      v
            // +----------+    +----------+  v             < end
            {
                const auto beg = _p.cells.begin();
                const auto end = _p.cells.end();
                std::move(beg - offset, end, beg);
            }
            {
                const auto beg = _p.cellGlyphMapping.begin();
                const auto end = _p.cellGlyphMapping.end();
                std::move(beg - offset, end, beg);
            }
        }
        else
        {
            // Scroll down.
            _api.invalidatedRows.x = 0;
            _api.invalidatedRows.y = nothingInvalid ? _api.scrollOffset : std::max<u16>(_api.invalidatedRows.y, _api.scrollOffset);

            // scrollOffset/offset = 1
            // +----------+    +----------+
            // | xxxxxxxxx|    |          |  + src         < beg
            // |xxxxxxx   | -> | xxxxxxxxx|  |      ^
            // |          |    |xxxxxxx   |  v      |      < end - offset
            // +----------+    +----------+         + dst  < end
            {
                const auto beg = _p.cells.begin();
                const auto end = _p.cells.end();
                std::move_backward(beg, end - offset, end);
            }
            {
                const auto beg = _p.cellGlyphMapping.begin();
                const auto end = _p.cellGlyphMapping.end();
                std::move_backward(beg, end - offset, end);
            }
        }
    }

    _api.dirtyRect = til::rect{ 0, _api.invalidatedRows.x, _api.s->cellCount.x, _api.invalidatedRows.y };
    _p.dirtyRect = _api.dirtyRect;
    _p.scrollOffset = _api.scrollOffset;

    // This is an important block of code for our TileHashMap.
    // We only process glyphs within the dirtyRect, but glyphs outside of the
    // dirtyRect are still in use and shouldn't be discarded. This is critical
    // if someone uses a tool like tmux to split the terminal horizontally.
    // If they then print a lot of Unicode text on just one side, we have to
    // ensure that the (for example) plain ASCII glyphs on the other half of the
    // viewport are still retained. This bit of code "refreshes" those glyphs and
    // brings them to the front of the LRU queue to prevent them from being reused.
    {
        const std::array<til::point, 2> ranges{ {
            { 0, _api.dirtyRect.top },
            { _api.dirtyRect.bottom, _api.s->cellCount.y },
        } };
        const auto stride = static_cast<size_t>(_p.s->cellCount.x);

        for (const auto& p : ranges)
        {
            // We (ab)use the .x/.y members of the til::point as the
            // respective [from,to) range of rows we need to makeNewest().
            const auto from = p.x;
            const auto to = p.y;

            for (auto y = from; y < to; ++y)
            {
                auto it = _p.cellGlyphMapping.data() + stride * y;
                const auto end = it + stride;
                for (; it != end; ++it)
                {
                    _p.glyphs.makeNewest(*it);
                }
            }
        }
    }

    return S_OK;
}
CATCH_RETURN()

[[nodiscard]] HRESULT AtlasEngine::EndPaint() noexcept
try
{
    _flushBufferLine();

    _api.invalidatedCursorArea = invalidatedAreaNone;
    _api.invalidatedRows = invalidatedRowsNone;
    _api.scrollOffset = 0;
    return S_OK;
}
CATCH_RETURN()

[[nodiscard]] HRESULT AtlasEngine::PrepareForTeardown(_Out_ bool* const pForcePaint) noexcept
{
    RETURN_HR_IF_NULL(E_INVALIDARG, pForcePaint);
    *pForcePaint = false;
    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::ScrollFrame() noexcept
{
    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::PrepareRenderInfo(const RenderFrameInfo& info) noexcept
{
    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::ResetLineTransform() noexcept
{
    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::PrepareLineTransform(const LineRendition lineRendition, const til::CoordType targetRow, const til::CoordType viewportLeft) noexcept
{
    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::PaintBackground() noexcept
{
    return S_OK;
}

[[nodiscard]] HRESULT AtlasEngine::PaintBufferLine(const gsl::span<const Cluster> clusters, const til::point coord, const bool fTrimLeft, const bool lineWrapped) noexcept
try
{
    const auto x = gsl::narrow_cast<u16>(clamp<int>(coord.X, 0, _api.s->cellCount.x));
    const auto y = gsl::narrow_cast<u16>(clamp<int>(coord.Y, 0, _api.s->cellCount.y));

    if (_api.lastPaintBufferLineCoord.y != y)
    {
        _flushBufferLine();
    }

    _api.lastPaintBufferLineCoord = { x, y };
    _api.bufferLineWasHyperlinked = false;

    // Due to the current IRenderEngine interface (that wasn't refactored yet) we need to assemble
    // the current buffer line first as the remaining function operates on whole lines of text.
    {
        if (!_api.bufferLineColumn.empty())
        {
            _api.bufferLineColumn.pop_back();
        }

        auto column = x;
        for (const auto& cluster : clusters)
        {
            for (const auto& ch : cluster.GetText())
            {
                _api.bufferLine.emplace_back(ch);
                _api.bufferLineColumn.emplace_back(column);
            }

            column += gsl::narrow_cast<u16>(cluster.GetColumns());
        }

        _api.bufferLineColumn.emplace_back(column);

        const BufferLineMetadata metadata{ _api.currentColor, _api.flags };
        std::fill_n(_api.bufferLineMetadata.data() + x, column - x, metadata);
    }

    return S_OK;
}
CATCH_RETURN()

[[nodiscard]] HRESULT AtlasEngine::PaintBufferGridLines(const GridLineSet lines, const COLORREF color, const size_t cchLine, const til::point coordTarget) noexcept
try
{
    if (!_api.bufferLineWasHyperlinked && lines.test(GridLines::Underline) && WI_IsFlagClear(_api.flags, CellFlags::Underline))
    {
        _api.bufferLineWasHyperlinked = true;

        WI_UpdateFlagsInMask(_api.flags, CellFlags::Underline | CellFlags::UnderlineDotted | CellFlags::UnderlineDouble, CellFlags::Underline);

        const BufferLineMetadata metadata{ _api.currentColor, _api.flags };
        const size_t x = _api.lastPaintBufferLineCoord.x;
        std::fill_n(_api.bufferLineMetadata.data() + x, _api.bufferLineMetadata.size() - x, metadata);
    }
    return S_OK;
}
CATCH_RETURN()

[[nodiscard]] HRESULT AtlasEngine::PaintSelection(const til::rect& rect) noexcept
try
{
    // Unfortunately there's no step after Renderer::_PaintBufferOutput that
    // would inform us that it's done with the last AtlasEngine::PaintBufferLine.
    // As such we got to call _flushBufferLine() here just to be sure.
    _flushBufferLine();

    const u16r u16rect{
        rect.narrow_left<u16>(),
        rect.narrow_top<u16>(),
        rect.narrow_right<u16>(),
        rect.narrow_bottom<u16>(),
    };
    _setCellFlags(u16rect, CellFlags::Selected, CellFlags::Selected);
    _p.dirtyRect |= rect;
    return S_OK;
}
CATCH_RETURN()

[[nodiscard]] HRESULT AtlasEngine::PaintCursor(const CursorOptions& options) noexcept
try
{
    // Unfortunately there's no step after Renderer::_PaintBufferOutput that
    // would inform us that it's done with the last AtlasEngine::PaintBufferLine.
    // As such we got to call _flushBufferLine() here just to be sure.
    _flushBufferLine();

    {
        const CursorSettings cursorSettings{
            gsl::narrow_cast<u32>(options.fUseColor ? options.cursorColor | 0xff000000 : INVALID_COLOR),
            gsl::narrow_cast<u16>(options.cursorType),
            gsl::narrow_cast<u8>(options.ulCursorHeightPercent),
        };
        if (*_p.s->cursor != cursorSettings)
        {
            *_p.s.write()->cursor.write() = cursorSettings;
            *_api.s.write()->cursor.write() = cursorSettings;
        }
    }

    // Clear the previous cursor
    if (const auto r = _api.invalidatedCursorArea; r.non_empty())
    {
        _setCellFlags(r, CellFlags::Cursor, CellFlags::None);
        _p.dirtyRect |= til::rect{ r.left, r.top, r.right, r.bottom };
    }

    if (options.isOn)
    {
        const auto point = options.coordCursor;
        // TODO: options.coordCursor can contain invalid out of bounds coordinates when
        // the window is being resized and the cursor is on the last line of the viewport.
        const auto x = gsl::narrow_cast<uint16_t>(clamp(point.X, 0, _p.s->cellCount.x - 1));
        const auto y = gsl::narrow_cast<uint16_t>(clamp(point.Y, 0, _p.s->cellCount.y - 1));
        const auto cursorWidth = 1 + (options.fIsDoubleWidth & (options.cursorType != CursorType::VerticalBar));
        const auto right = gsl::narrow_cast<uint16_t>(clamp(x + cursorWidth, 0, _p.s->cellCount.x - 0));
        const auto bottom = gsl::narrow_cast<uint16_t>(y + 1);
        _setCellFlags({ x, y, right, bottom }, CellFlags::Cursor, CellFlags::Cursor);
        _p.dirtyRect |= til::rect{ x, y, right, bottom };
    }

    return S_OK;
}
CATCH_RETURN()

[[nodiscard]] HRESULT AtlasEngine::UpdateDrawingBrushes(const TextAttribute& textAttributes, const RenderSettings& renderSettings, const gsl::not_null<IRenderData*> /*pData*/, const bool usingSoftFont, const bool isSettingDefaultBrushes) noexcept
try
{
    auto [fg, bg] = renderSettings.GetAttributeColorsWithAlpha(textAttributes);
    fg |= 0xff000000;
    bg |= _api.backgroundOpaqueMixin;

    if (!isSettingDefaultBrushes)
    {
        const auto hyperlinkId = textAttributes.GetHyperlinkId();

        auto flags = CellFlags::None;
        WI_SetFlagIf(flags, CellFlags::BorderLeft, textAttributes.IsLeftVerticalDisplayed());
        WI_SetFlagIf(flags, CellFlags::BorderTop, textAttributes.IsTopHorizontalDisplayed());
        WI_SetFlagIf(flags, CellFlags::BorderRight, textAttributes.IsRightVerticalDisplayed());
        WI_SetFlagIf(flags, CellFlags::BorderBottom, textAttributes.IsBottomHorizontalDisplayed());
        WI_SetFlagIf(flags, CellFlags::Underline, textAttributes.IsUnderlined());
        WI_SetFlagIf(flags, CellFlags::UnderlineDotted, hyperlinkId != 0);
        WI_SetFlagIf(flags, CellFlags::UnderlineDouble, textAttributes.IsDoublyUnderlined());
        WI_SetFlagIf(flags, CellFlags::Strikethrough, textAttributes.IsCrossedOut());

        if (_api.hyperlinkHoveredId && _api.hyperlinkHoveredId == hyperlinkId)
        {
            WI_SetFlag(flags, CellFlags::Underline);
            WI_ClearAllFlags(flags, CellFlags::UnderlineDotted | CellFlags::UnderlineDouble);
        }

        const u32x2 newColors{ gsl::narrow_cast<u32>(fg), gsl::narrow_cast<u32>(bg) };
        const AtlasKeyAttributes attributes{ 0, textAttributes.IsIntense() && renderSettings.GetRenderMode(RenderSettings::Mode::IntenseIsBold), textAttributes.IsItalic(), 0 };

        if (_api.attributes != attributes)
        {
            _flushBufferLine();
        }

        _api.currentColor = newColors;
        _api.attributes = attributes;
        _api.flags = flags;
    }
    else if (textAttributes.BackgroundIsDefault() && bg != _p.s->misc->backgroundColor)
    {
        _p.s.write()->misc.write()->backgroundColor = bg;
        _api.s.write()->misc.write()->backgroundColor = bg;
    }

    return S_OK;
}
CATCH_RETURN()

#pragma endregion

[[nodiscard]] HRESULT AtlasEngine::_handleException(const wil::ResultException& exception) noexcept
{
    const auto hr = exception.GetErrorCode();
    if (hr == DXGI_ERROR_DEVICE_REMOVED || hr == DXGI_ERROR_DEVICE_RESET || hr == D2DERR_RECREATE_TARGET)
    {
        _p.dxgiFactory.reset();
        _r = {};
        return E_PENDING; // Indicate a retry to the renderer
    }

    if (_p.warningCallback)
    {
        try
        {
            _p.warningCallback(hr);
        }
        CATCH_LOG()
    }

    return hr;
}

void AtlasEngine::_recreateBackend()
{
#if !defined(NDEBUG)
    // DXGIGetDebugInterface1 returns E_NOINTERFACE on systems without the Windows SDK installed.
    if (wil::com_ptr<IDXGIInfoQueue> infoQueue; SUCCEEDED_LOG(DXGIGetDebugInterface1(0, IID_PPV_ARGS(infoQueue.addressof()))))
    {
        // I didn't want to link with dxguid.lib just for getting DXGI_DEBUG_ALL. This GUID is publicly documented.
        static constexpr GUID dxgiDebugAll{ 0xe48ae283, 0xda80, 0x490b, { 0x87, 0xe6, 0x43, 0xe9, 0xa9, 0xcf, 0xda, 0x8 } };
        for (const auto severity : std::array{ DXGI_INFO_QUEUE_MESSAGE_SEVERITY_CORRUPTION, DXGI_INFO_QUEUE_MESSAGE_SEVERITY_ERROR, DXGI_INFO_QUEUE_MESSAGE_SEVERITY_WARNING })
        {
            infoQueue->SetBreakOnSeverity(dxgiDebugAll, severity, true);
        }
    }
#endif

#if defined(NDEBUG)
    static constexpr UINT flags = 0;
#else
    static constexpr UINT flags = DXGI_CREATE_FACTORY_DEBUG;
#endif

    THROW_IF_FAILED(CreateDXGIFactory2(flags, __uuidof(IDXGIFactory3), _p.dxgiFactory.put_void()));

    auto d2dMode = debugForceD2DMode;
    auto deviceFlags = D3D11_CREATE_DEVICE_SINGLETHREADED
#if !defined(NDEBUG)
                       | D3D11_CREATE_DEVICE_DEBUG
#endif
                       // This flag prevents the driver from creating a large thread pool for things like shader computations
                       // that would be advantageous for games. For us this has only a minimal performance benefit,
                       // but comes with a large memory usage overhead. At the time of writing the Nvidia
                       // driver launches $cpu_thread_count more worker threads without this flag.
                       | D3D11_CREATE_DEVICE_PREVENT_INTERNAL_THREADING_OPTIMIZATIONS
                       // Direct2D support.
                       | D3D11_CREATE_DEVICE_BGRA_SUPPORT;

    wil::com_ptr<IDXGIAdapter1> dxgiAdapter;
    THROW_IF_FAILED(_p.dxgiFactory->EnumAdapters1(0, dxgiAdapter.addressof()));

    {
        auto findSoftwareAdapter = _r.useSoftwareRendering;
        auto adapter = dxgiAdapter;
        UINT i = 0;

        for (;;)
        {
            DXGI_ADAPTER_DESC1 desc;
            THROW_IF_FAILED(adapter->GetDesc1(&desc));

            // Switch to D2D mode if any adapter is a remote adapter (RDP).
            d2dMode |= WI_IsFlagSet(desc.Flags, DXGI_ADAPTER_FLAG_REMOTE);

            // If useSoftwareRendering is true we search for the first WARP adapter.
            if (findSoftwareAdapter && WI_IsFlagSet(desc.Flags, DXGI_ADAPTER_FLAG_SOFTWARE))
            {
                WI_ClearFlag(deviceFlags, D3D11_CREATE_DEVICE_PREVENT_INTERNAL_THREADING_OPTIMIZATIONS);
                dxgiAdapter = std::move(adapter);
                findSoftwareAdapter = false;
            }

            ++i;
            if (_p.dxgiFactory->EnumAdapters1(i, adapter.put()) == DXGI_ERROR_NOT_FOUND)
            {
                break;
            }
        }
    }

    wil::com_ptr<ID3D11Device> device0;
    wil::com_ptr<ID3D11DeviceContext> deviceContext0;
    D3D_FEATURE_LEVEL featureLevel{};

    static constexpr std::array featureLevels{
        D3D_FEATURE_LEVEL_11_1,
        D3D_FEATURE_LEVEL_11_0,
        D3D_FEATURE_LEVEL_10_1,
        D3D_FEATURE_LEVEL_10_0,
        D3D_FEATURE_LEVEL_9_3,
        D3D_FEATURE_LEVEL_9_2,
        D3D_FEATURE_LEVEL_9_1,
    };

    THROW_IF_FAILED(D3D11CreateDevice(
        /* pAdapter */ dxgiAdapter.get(),
        /* DriverType */ D3D_DRIVER_TYPE_UNKNOWN,
        /* Software */ nullptr,
        /* Flags */ deviceFlags,
        /* pFeatureLevels */ featureLevels.data(),
        /* FeatureLevels */ gsl::narrow_cast<UINT>(featureLevels.size()),
        /* SDKVersion */ D3D11_SDK_VERSION,
        /* ppDevice */ device0.put(),
        /* pFeatureLevel */ &featureLevel,
        /* ppImmediateContext */ deviceContext0.put()));

    const auto device = device0.query<ID3D11Device2>();
    const auto deviceContext = deviceContext0.query<ID3D11DeviceContext2>();

    d2dMode |= featureLevel < D3D_FEATURE_LEVEL_10_0;

    if (d2dMode)
    {
        _r.driver = std::make_unique<DriverD2D>(std::move(device), std::move(deviceContext));
    }
    else
    {
        _r.driver = std::make_unique<DriverD3D11>(std::move(device), std::move(deviceContext));
    }
}

void DriverD3D11::_createCustomShaderResources(const RenderingPayload& p)
{
    _customOffscreenTexture.reset();
    _customOffscreenTextureView.reset();
    _customOffscreenTextureTargetView.reset();
    _customVertexShader.reset();
    _customPixelShader.reset();
    _customShaderConstantBuffer.reset();
    _customShaderSamplerState.reset();

    if (!p.s->misc->customPixelShaderPath.empty())
    {
        const char* target = nullptr;
        switch (_device->GetFeatureLevel())
        {
        case D3D_FEATURE_LEVEL_10_0:
            target = "ps_4_0";
            break;
        case D3D_FEATURE_LEVEL_10_1:
            target = "ps_4_1";
            break;
        default:
            target = "ps_5_0";
            break;
        }

        static constexpr auto flags = D3DCOMPILE_PACK_MATRIX_COLUMN_MAJOR | D3DCOMPILE_ENABLE_STRICTNESS | D3DCOMPILE_WARNINGS_ARE_ERRORS
#ifdef NDEBUG
                                      | D3DCOMPILE_OPTIMIZATION_LEVEL3;
#else
                                      | D3DCOMPILE_DEBUG | D3DCOMPILE_SKIP_OPTIMIZATION;
#endif

        wil::com_ptr<ID3DBlob> error;
        wil::com_ptr<ID3DBlob> blob;
        const auto hr = D3DCompileFromFile(
            /* pFileName   */ p.s->misc->customPixelShaderPath.c_str(),
            /* pDefines    */ nullptr,
            /* pInclude    */ D3D_COMPILE_STANDARD_FILE_INCLUDE,
            /* pEntrypoint */ "main",
            /* pTarget     */ target,
            /* Flags1      */ flags,
            /* Flags2      */ 0,
            /* ppCode      */ blob.addressof(),
            /* ppErrorMsgs */ error.addressof());

        // Unless we can determine otherwise, assume this shader requires evaluation every frame
        _requiresContinuousRedraw = true;

        if (SUCCEEDED(hr))
        {
            THROW_IF_FAILED(_device->CreatePixelShader(blob->GetBufferPointer(), blob->GetBufferSize(), nullptr, _customPixelShader.put()));

            // Try to determine whether the shader uses the Time variable
            wil::com_ptr<ID3D11ShaderReflection> reflector;
            if (SUCCEEDED_LOG(D3DReflect(blob->GetBufferPointer(), blob->GetBufferSize(), IID_PPV_ARGS(reflector.put()))))
            {
                if (ID3D11ShaderReflectionConstantBuffer* constantBufferReflector = reflector->GetConstantBufferByIndex(0)) // shader buffer
                {
                    if (ID3D11ShaderReflectionVariable* variableReflector = constantBufferReflector->GetVariableByIndex(0)) // time
                    {
                        D3D11_SHADER_VARIABLE_DESC variableDescriptor;
                        if (SUCCEEDED_LOG(variableReflector->GetDesc(&variableDescriptor)))
                        {
                            // only if time is used
                            _requiresContinuousRedraw = WI_IsFlagSet(variableDescriptor.uFlags, D3D_SVF_USED);
                        }
                    }
                }
            }
        }
        else
        {
            if (error)
            {
                LOG_HR_MSG(hr, "%*hs", error->GetBufferSize(), error->GetBufferPointer());
            }
            else
            {
                LOG_HR(hr);
            }
            if (p.warningCallback)
            {
                p.warningCallback(D2DERR_SHADER_COMPILE_FAILED);
            }
        }
    }
    else if (p.s->misc->useRetroTerminalEffect)
    {
        THROW_IF_FAILED(_device->CreatePixelShader(&custom_shader_ps[0], sizeof(custom_shader_ps), nullptr, _customPixelShader.put()));
        // We know the built-in retro shader doesn't require continuous redraw.
        _requiresContinuousRedraw = false;
    }

    if (_customPixelShader)
    {
        THROW_IF_FAILED(_device->CreateVertexShader(&custom_shader_vs[0], sizeof(custom_shader_vs), nullptr, _customVertexShader.put()));

        {
            D3D11_BUFFER_DESC desc{};
            desc.ByteWidth = sizeof(CustomConstBuffer);
            desc.Usage = D3D11_USAGE_DYNAMIC;
            desc.BindFlags = D3D11_BIND_CONSTANT_BUFFER;
            desc.CPUAccessFlags = D3D11_CPU_ACCESS_WRITE;
            THROW_IF_FAILED(_device->CreateBuffer(&desc, nullptr, _customShaderConstantBuffer.put()));
        }

        {
            D3D11_SAMPLER_DESC desc{};
            desc.Filter = D3D11_FILTER_MIN_MAG_MIP_LINEAR;
            desc.AddressU = D3D11_TEXTURE_ADDRESS_BORDER;
            desc.AddressV = D3D11_TEXTURE_ADDRESS_BORDER;
            desc.AddressW = D3D11_TEXTURE_ADDRESS_BORDER;
            desc.MaxAnisotropy = 1;
            desc.ComparisonFunc = D3D11_COMPARISON_ALWAYS;
            desc.MaxLOD = D3D11_FLOAT32_MAX;
            THROW_IF_FAILED(_device->CreateSamplerState(&desc, _customShaderSamplerState.put()));
        }

        {
            D3D11_TEXTURE2D_DESC desc{};
            desc.Width = p.s->targetSize.x;
            desc.Height = p.s->targetSize.y;
            desc.MipLevels = 1;
            desc.ArraySize = 1;
            desc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
            desc.SampleDesc = { 1, 0 };
            desc.BindFlags = D3D11_BIND_SHADER_RESOURCE | D3D11_BIND_RENDER_TARGET;
            THROW_IF_FAILED(_device->CreateTexture2D(&desc, nullptr, _customOffscreenTexture.put()));
            THROW_IF_FAILED(_device->CreateShaderResourceView(_customOffscreenTexture.get(), nullptr, _customOffscreenTextureView.put()));
            THROW_IF_FAILED(_device->CreateRenderTargetView(_customOffscreenTexture.get(), nullptr, _customOffscreenTextureTargetView.put()));
        }

        _customShaderStartTime = std::chrono::steady_clock::now();
    }
}

void SwapChainManager::_createSwapChain(const RenderingPayload& p, IUnknown* device)
{
    _swapChain.reset();
    _frameLatencyWaitableObject.reset();

    DXGI_SWAP_CHAIN_DESC1 desc{};
    desc.Width = p.s->targetSize.x;
    desc.Height = p.s->targetSize.y;
    desc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
    desc.SampleDesc.Count = 1;
    desc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
    // Sometimes up to 2 buffers are locked, for instance during screen capture or when moving the window.
    // 3 buffers seems to guarantee a stable framerate at display frequency at all times.
    desc.BufferCount = 3;
    desc.Scaling = DXGI_SCALING_NONE;
    // DXGI_SWAP_EFFECT_FLIP_DISCARD is a mode that was created at a time were display drivers
    // lacked support for Multiplane Overlays (MPO) and were copying buffers was expensive.
    // This allowed DWM to quickly draw overlays (like gamebars) on top of rendered content.
    // With faster GPU memory in general and with support for MPO in particular this isn't
    // really an advantage anymore. Instead DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL allows for a
    // more "intelligent" composition and display updates to occur like Panel Self Refresh
    // (PSR) which requires dirty rectangles (Present1 API) to work correctly.
    desc.SwapEffect = DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL;
    // If our background is opaque we can enable "independent" flips by setting DXGI_ALPHA_MODE_IGNORE.
    // As our swap chain won't have to compose with DWM anymore it reduces the display latency dramatically.
    desc.AlphaMode = p.s->target->enableTransparentBackground ? DXGI_ALPHA_MODE_PREMULTIPLIED : DXGI_ALPHA_MODE_IGNORE;
    desc.Flags = DXGI_SWAP_CHAIN_FLAG_FRAME_LATENCY_WAITABLE_OBJECT;

    wil::com_ptr<IDXGISwapChain1> swapChain0;

    if (p.s->target->hwnd)
    {
        THROW_IF_FAILED(p.dxgiFactory->CreateSwapChainForHwnd(device, p.s->target->hwnd, &desc, nullptr, nullptr, swapChain0.addressof()));
    }
    else
    {
        // As per: https://docs.microsoft.com/en-us/windows/win32/api/dcomp/nf-dcomp-dcompositioncreatesurfacehandle
        static constexpr DWORD COMPOSITIONSURFACE_ALL_ACCESS = 0x0003L;
        THROW_IF_FAILED(DCompositionCreateSurfaceHandle(COMPOSITIONSURFACE_ALL_ACCESS, nullptr, _swapChainHandle.addressof()));
        THROW_IF_FAILED(p.dxgiFactory.query<IDXGIFactoryMedia>()->CreateSwapChainForCompositionSurfaceHandle(device, _swapChainHandle.get(), &desc, nullptr, swapChain0.addressof()));
    }

    _swapChain = swapChain0.query<IDXGISwapChain2>();
    _targetGeneration = p.s->target.generation();
    _targetSize = p.s->targetSize;
    _waitForPresentation = true;

    WaitUntilCanRender();

    if (p.swapChainChangedCallback)
    {
        try
        {
            p.swapChainChangedCallback(_swapChainHandle.get());
        }
        CATCH_LOG()
    }
}

void AtlasEngine::_recreateSizeDependentResources()
{
    const auto totalCellCount = static_cast<size_t>(_api.s->cellCount.x) * static_cast<size_t>(_api.s->cellCount.y);
    // Let's guess that every cell consists of a surrogate pair.
    const auto projectedTextSize = static_cast<size_t>(_api.s->cellCount.x) * 2;
    // IDWriteTextAnalyzer::GetGlyphs says:
    //   The recommended estimate for the per-glyph output buffers is (3 * textLength / 2 + 16).
    const auto projectedGlyphSize = 3 * projectedTextSize / 2 + 16;

    // This buffer is a bit larger than the others (multiple MB).
    // Prevent a memory usage spike, by first deallocating and then allocating.
    _p.cells = {};
    _p.cellGlyphMapping = {};
    // Our render loop heavily relies on memcpy() which is between 1.5x
    // and 40x faster for allocations with an alignment of 32 or greater.
    // (40x on AMD Zen1-3, which have a rep movsb performance issue. MSFT:33358259.)
    _p.cells = Buffer<Cell, 32>{ totalCellCount };
    _p.cellGlyphMapping = Buffer<TileHashMap::iterator>{ totalCellCount };

    // .clear() doesn't free the memory of these buffers.
    // This code allows them to shrink again.
    _api.bufferLine = {};
    _api.bufferLine.reserve(projectedTextSize);
    _api.bufferLineColumn.reserve(projectedTextSize + 1);
    _api.bufferLineMetadata = Buffer<BufferLineMetadata>{ _api.s->cellCount.x };
    _api.analysisResults = {};

    _api.clusterMap = Buffer<u16>{ projectedTextSize };
    _api.textProps = Buffer<DWRITE_SHAPING_TEXT_PROPERTIES>{ projectedTextSize };
    _api.glyphIndices = Buffer<u16>{ projectedGlyphSize };
    _api.glyphProps = Buffer<DWRITE_SHAPING_GLYPH_PROPERTIES>{ projectedGlyphSize };
    _api.glyphAdvances = Buffer<f32>{ projectedGlyphSize };
    _api.glyphOffsets = Buffer<DWRITE_GLYPH_OFFSET>{ projectedGlyphSize };

    // Initialize cellGlyphMapping with valid data (whitespace), so that it can be
    // safely used by the TileHashMap refresh logic via makeNewest() in StartPaint().
    {
        u16x2* coords;
        AtlasKey key{ { .cellCount = 1 }, 1, L" " };
        AtlasValue value{ CellFlags::None, 1, &coords };

        coords[0] = _p.tileAllocator.allocate(_p.glyphs);

        const auto it = _p.glyphs.insert(std::move(key), std::move(value));
        _p.glyphQueue.emplace_back(it);

        std::ranges::fill(_p.cellGlyphMapping, it);
    }
}

void AtlasEngine::_recreateFontDependentResources()
{
    {
        const auto scaling = GetScaling();

        _p.d.font.cellSizeDIP.x = static_cast<float>(_p.s->font->cellSize.x) / scaling;
        _p.d.font.cellSizeDIP.y = static_cast<float>(_p.s->font->cellSize.y) / scaling;
        _p.d.font.dipPerPixel = static_cast<float>(USER_DEFAULT_SCREEN_DPI) / static_cast<float>(_p.s->font->dpi);
        _p.d.font.pixelPerDIP = static_cast<float>(_p.s->font->dpi) / static_cast<float>(USER_DEFAULT_SCREEN_DPI);
        _p.tileAllocator = TileAllocator{ _p.s->font->cellSize };
        _p.glyphs = {};
        _p.glyphQueue = {};
        _p.glyphQueue.reserve(64);
    }

    // D2D
    {
        // See AtlasEngine::UpdateFont.
        // It hardcodes indices 0/1/2 in fontAxisValues to the weight/italic/slant axes.
        // If they're -1.0f they haven't been set by the user and must be filled by us.
        // When we call SetFontAxisValues() we basically override (disable) DirectWrite's internal font axes,
        // and if either of the 3 aren't set we'd make it impossible for the user to see bold/italic text.
        const auto& standardAxes = _p.s->font->fontAxisValues;
        auto fontAxisValues = _p.s->font->fontAxisValues;

        for (auto italic = 0; italic < 2; ++italic)
        {
            for (auto bold = 0; bold < 2; ++bold)
            {
                const auto fontWeight = bold ? DWRITE_FONT_WEIGHT_BOLD : static_cast<DWRITE_FONT_WEIGHT>(_p.s->font->fontWeight);
                const auto fontStyle = italic ? DWRITE_FONT_STYLE_ITALIC : DWRITE_FONT_STYLE_NORMAL;
                auto& textFormat = _p.d.font.textFormats[italic][bold];

                THROW_IF_FAILED(_p.dwriteFactory->CreateTextFormat(_p.s->font->fontName.c_str(), _p.s->font->fontCollection.get(), fontWeight, fontStyle, DWRITE_FONT_STRETCH_NORMAL, _p.s->font->fontSizeInDIP, L"", textFormat.put()));
                THROW_IF_FAILED(textFormat->SetTextAlignment(DWRITE_TEXT_ALIGNMENT_CENTER));
                THROW_IF_FAILED(textFormat->SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP));

                // DWRITE_LINE_SPACING_METHOD_UNIFORM:
                // > Lines are explicitly set to uniform spacing, regardless of contained font sizes.
                // > This can be useful to avoid the uneven appearance that can occur from font fallback.
                // We want that. Otherwise fallback fonts might be rendered with an incorrect baseline and get cut off vertically.
                THROW_IF_FAILED(textFormat->SetLineSpacing(DWRITE_LINE_SPACING_METHOD_UNIFORM, _p.d.font.cellSizeDIP.y, _p.s->font->baselineInDIP));

                if (const auto textFormat3 = textFormat.try_query<IDWriteTextFormat3>())
                {
                    THROW_IF_FAILED(textFormat3->SetAutomaticFontAxes(DWRITE_AUTOMATIC_FONT_AXES_OPTICAL_SIZE));

                    if (!fontAxisValues.empty())
                    {
                        // The wght axis defaults to the font weight.
                        fontAxisValues[0].value = bold || standardAxes[0].value == -1.0f ? static_cast<float>(fontWeight) : standardAxes[0].value;
                        // The ital axis defaults to 1 if this is italic and 0 otherwise.
                        fontAxisValues[1].value = italic ? 1.0f : (standardAxes[1].value == -1.0f ? 0.0f : standardAxes[1].value);
                        // The slnt axis defaults to -12 if this is italic and 0 otherwise.
                        fontAxisValues[2].value = italic ? -12.0f : (standardAxes[2].value == -1.0f ? 0.0f : standardAxes[2].value);

                        THROW_IF_FAILED(textFormat3->SetFontAxisValues(fontAxisValues.data(), gsl::narrow_cast<u32>(fontAxisValues.size())));
                        _p.d.font.textFormatAxes[italic][bold] = { fontAxisValues.data(), fontAxisValues.size() };
                    }
                }
            }
        }
    }
    {
        _p.d.font.typography.reset();

        if (!_p.s->font->fontFeatures.empty())
        {
            _p.dwriteFactory->CreateTypography(_p.d.font.typography.addressof());
            for (const auto& v : _p.s->font->fontFeatures)
            {
                THROW_IF_FAILED(_p.d.font.typography->AddFontFeature(v));
            }
        }
    }
}

IDWriteTextFormat* AtlasEngine::_getTextFormat(bool bold, bool italic) const noexcept
{
    return _p.d.font.textFormats[italic][bold].get();
}

const Buffer<DWRITE_FONT_AXIS_VALUE>& AtlasEngine::_getTextFormatAxis(bool bold, bool italic) const noexcept
{
    return _p.d.font.textFormatAxes[italic][bold];
}

Cell* AtlasEngine::_getCell(u16 x, u16 y) noexcept
{
    assert(x < _p.s->cellCount.x);
    assert(y < _p.s->cellCount.y);
    return _p.cells.data() + static_cast<size_t>(_p.s->cellCount.x) * y + x;
}

TileHashMap::iterator* AtlasEngine::_getCellGlyphMapping(u16 x, u16 y) noexcept
{
    assert(x < _p.s->cellCount.x);
    assert(y < _p.s->cellCount.y);
    return _p.cellGlyphMapping.data() + static_cast<size_t>(_p.s->cellCount.x) * y + x;
}

void AtlasEngine::_setCellFlags(u16r coords, CellFlags mask, CellFlags bits) noexcept
{
    assert(coords.left <= coords.right);
    assert(coords.top <= coords.bottom);
    assert(coords.right <= _p.s->cellCount.x);
    assert(coords.bottom <= _p.s->cellCount.y);

    const auto filter = ~mask;
    const auto width = static_cast<size_t>(coords.right) - coords.left;
    const auto height = static_cast<size_t>(coords.bottom) - coords.top;
    const auto stride = static_cast<size_t>(_p.s->cellCount.x);
    auto row = _p.cells.data() + static_cast<size_t>(_p.s->cellCount.x) * coords.top + coords.left;
    const auto end = row + height * stride;

    for (; row != end; row += stride)
    {
        const auto dataEnd = row + width;
        for (auto data = row; data != dataEnd; ++data)
        {
            const auto current = data->flags;
            data->flags = (current & filter) | bits;
        }
    }
}

void AtlasEngine::_flushBufferLine()
{
    if (_api.bufferLine.empty())
    {
        return;
    }

    const auto cleanup = wil::scope_exit([this]() noexcept {
        _api.bufferLine.clear();
        _api.bufferLineColumn.clear();
    });

    // This would seriously blow us up otherwise.
    Expects(_api.bufferLineColumn.size() == _api.bufferLine.size() + 1);

    // GH#13962: With the lack of proper LineRendition support, just fill
    // the remaining columns with whitespace to prevent any weird artifacts.
    for (auto lastColumn = _api.bufferLineColumn.back(); lastColumn < _api.s->cellCount.x;)
    {
        ++lastColumn;
        _api.bufferLine.emplace_back(L' ');
        _api.bufferLineColumn.emplace_back(lastColumn);
    }

    // NOTE:
    // This entire function is one huge hack to see if it works.

    // UH OH UNICODE MADNESS AHEAD
    //
    // # What do we want?
    //
    // Segment a line of text (_api.bufferLine) into unicode "clusters".
    // Each cluster is one "whole" glyph with diacritics, ligatures, zero width joiners
    // and whatever else, that should be cached as a whole in our texture atlas.
    //
    // # How do we get that?
    //
    // ## The unfortunate preface
    //
    // DirectWrite can be "reluctant" to segment text into clusters and I found no API which offers simply that.
    // What it offers are a large number of low level APIs that can sort of be used in combination to do this.
    // The resulting text parsing is very slow unfortunately, consuming up to 95% of rendering time in extreme cases.
    //
    // ## The actual approach
    //
    // DirectWrite has 2 APIs which can segment text properly (including ligatures and zero width joiners):
    // * IDWriteTextAnalyzer1::GetTextComplexity
    // * IDWriteTextAnalyzer::GetGlyphs
    //
    // Both APIs require us to attain an IDWriteFontFace as the functions themselves don't handle font fallback.
    // This forces us to call IDWriteFontFallback::MapCharacters first.
    //
    // Additionally IDWriteTextAnalyzer::GetGlyphs requires an instance of DWRITE_SCRIPT_ANALYSIS,
    // which can only be attained by running IDWriteTextAnalyzer::AnalyzeScript first.
    //
    // Font fallback with IDWriteFontFallback::MapCharacters is very slow.

    const auto& textFormatAxis = _getTextFormatAxis(_api.attributes.bold, _api.attributes.italic);

    TextAnalysisSource analysisSource{ _api.bufferLine.data(), gsl::narrow<UINT32>(_api.bufferLine.size()) };
    TextAnalysisSink analysisSink{ _api.analysisResults };

    wil::com_ptr<IDWriteFontFace> mappedFontFace;

#pragma warning(suppress : 26494) // Variable 'mappedEnd' is uninitialized. Always initialize an object (type.5).
    for (u32 idx = 0, mappedEnd; idx < _api.bufferLine.size(); idx = mappedEnd)
    {
        auto scale = 1.0f;
        u32 mappedLength = 0;

        if (textFormatAxis)
        {
            wil::com_ptr<IDWriteFontFace5> fontFace5;
            THROW_IF_FAILED(_p.systemFontFallback.query<IDWriteFontFallback1>()->MapCharacters(
                /* analysisSource */ &analysisSource,
                /* textPosition */ idx,
                /* textLength */ gsl::narrow_cast<u32>(_api.bufferLine.size()) - idx,
                /* baseFontCollection */ _p.s->font->fontCollection.get(),
                /* baseFamilyName */ _p.s->font->fontName.c_str(),
                /* fontAxisValues */ textFormatAxis.data(),
                /* fontAxisValueCount */ gsl::narrow_cast<u32>(textFormatAxis.size()),
                /* mappedLength */ &mappedLength,
                /* scale */ &scale,
                /* mappedFontFace */ fontFace5.put()));
            mappedFontFace = std::move(fontFace5);
        }
        else
        {
            const auto baseWeight = _api.attributes.bold ? DWRITE_FONT_WEIGHT_BOLD : static_cast<DWRITE_FONT_WEIGHT>(_p.s->font->fontWeight);
            const auto baseStyle = _api.attributes.italic ? DWRITE_FONT_STYLE_ITALIC : DWRITE_FONT_STYLE_NORMAL;
            wil::com_ptr<IDWriteFont> font;

            THROW_IF_FAILED(_p.systemFontFallback->MapCharacters(
                /* analysisSource     */ &analysisSource,
                /* textPosition       */ idx,
                /* textLength         */ gsl::narrow_cast<u32>(_api.bufferLine.size()) - idx,
                /* baseFontCollection */ _p.s->font->fontCollection.get(),
                /* baseFamilyName     */ _p.s->font->fontName.c_str(),
                /* baseWeight         */ baseWeight,
                /* baseStyle          */ baseStyle,
                /* baseStretch        */ DWRITE_FONT_STRETCH_NORMAL,
                /* mappedLength       */ &mappedLength,
                /* mappedFont         */ font.addressof(),
                /* scale              */ &scale));

            mappedFontFace.reset();
            if (font)
            {
                THROW_IF_FAILED(font->CreateFontFace(mappedFontFace.addressof()));
            }
        }

        mappedEnd = idx + mappedLength;

        if (!mappedFontFace)
        {
            // Task: Replace all characters in this range with unicode replacement characters.
            // Input (where "n" is a narrow and "ww" is a wide character):
            //    _api.bufferLine       = "nwwnnw"
            //    _api.bufferLineColumn = {0, 1, 1, 3, 4, 5, 5, 6}
            //                             n  w  w  n  n  w  w
            // Solution:
            //   Iterate through bufferLineColumn until the value changes, because this indicates we passed over a
            //   complete (narrow or wide) cell. To do so we'll use col1 (previous column) and col2 (next column).
            //   Then we emit a replacement character by telling _emplaceGlyph that this range has no font face.
            auto pos1 = idx;
            auto col1 = _api.bufferLineColumn[pos1];
            for (auto pos2 = idx + 1; pos2 <= mappedEnd; ++pos2)
            {
                if (const auto col2 = _api.bufferLineColumn[pos2]; col1 != col2)
                {
                    _emplaceGlyph(nullptr, pos1, pos2);
                    pos1 = pos2;
                    col1 = col2;
                }
            }

            continue;
        }

        // We can reuse idx here, as it'll be reset to "idx = mappedEnd" in the outer loop anyways.
        for (u32 complexityLength = 0; idx < mappedEnd; idx += complexityLength)
        {
            BOOL isTextSimple;
            THROW_IF_FAILED(_p.textAnalyzer->GetTextComplexity(_api.bufferLine.data() + idx, mappedEnd - idx, mappedFontFace.get(), &isTextSimple, &complexityLength, _api.glyphIndices.data()));

            if (isTextSimple)
            {
                size_t beg = 0;
                for (size_t i = 0; i < complexityLength; ++i)
                {
                    if (_emplaceGlyph(mappedFontFace.get(), idx + beg, idx + i + 1))
                    {
                        beg = i + 1;
                    }
                }
            }
            else
            {
                _api.analysisResults.clear();
                THROW_IF_FAILED(_p.textAnalyzer->AnalyzeScript(&analysisSource, idx, complexityLength, &analysisSink));
                //_p.textAnalyzer->AnalyzeBidi(&atlasAnalyzer, idx, complexityLength, &atlasAnalyzer);

                for (const auto& a : _api.analysisResults)
                {
                    DWRITE_SCRIPT_ANALYSIS scriptAnalysis{ a.script, static_cast<DWRITE_SCRIPT_SHAPES>(a.shapes) };
                    u32 actualGlyphCount = 0;

#pragma warning(push)
#pragma warning(disable : 26494) // Variable '...' is uninitialized. Always initialize an object (type.5).
                    // None of these variables need to be initialized.
                    // features/featureRangeLengths are marked _In_reads_opt_(featureRanges).
                    // featureRanges is only > 0 when we also initialize all these variables.
                    DWRITE_TYPOGRAPHIC_FEATURES feature;
                    const DWRITE_TYPOGRAPHIC_FEATURES* features;
                    u32 featureRangeLengths;
#pragma warning(pop)
                    u32 featureRanges = 0;

                    if (!_p.s->font->fontFeatures.empty())
                    {
                        feature.features = const_cast<DWRITE_FONT_FEATURE*>(_p.s->font->fontFeatures.data());
                        feature.featureCount = gsl::narrow_cast<u32>(_p.s->font->fontFeatures.size());
                        features = &feature;
                        featureRangeLengths = a.textLength;
                        featureRanges = 1;
                    }

                    if (_api.clusterMap.size() < a.textLength)
                    {
                        _api.clusterMap = Buffer<u16>{ a.textLength };
                        _api.textProps = Buffer<DWRITE_SHAPING_TEXT_PROPERTIES>{ a.textLength };
                    }

                    for (auto retry = 0;;)
                    {
                        const auto hr = _p.textAnalyzer->GetGlyphs(
                            /* textString          */ _api.bufferLine.data() + a.textPosition,
                            /* textLength          */ a.textLength,
                            /* fontFace            */ mappedFontFace.get(),
                            /* isSideways          */ false,
                            /* isRightToLeft       */ a.bidiLevel & 1,
                            /* scriptAnalysis      */ &scriptAnalysis,
                            /* localeName          */ nullptr,
                            /* numberSubstitution  */ nullptr,
                            /* features            */ &features,
                            /* featureRangeLengths */ &featureRangeLengths,
                            /* featureRanges       */ featureRanges,
                            /* maxGlyphCount       */ gsl::narrow_cast<u32>(_api.glyphProps.size()),
                            /* clusterMap          */ _api.clusterMap.data(),
                            /* textProps           */ _api.textProps.data(),
                            /* glyphIndices        */ _api.glyphIndices.data(),
                            /* glyphProps          */ _api.glyphProps.data(),
                            /* actualGlyphCount    */ &actualGlyphCount);

                        if (hr == HRESULT_FROM_WIN32(ERROR_INSUFFICIENT_BUFFER) && ++retry < 8)
                        {
                            // Grow factor 1.5x.
                            auto size = _api.glyphProps.size();
                            size = size + (size >> 1);
                            // Overflow check.
                            Expects(size > _api.glyphProps.size());
                            _api.glyphIndices = Buffer<u16>{ size };
                            _api.glyphProps = Buffer<DWRITE_SHAPING_GLYPH_PROPERTIES>(size);
                            continue;
                        }

                        THROW_IF_FAILED(hr);
                        break;
                    }

                    if (_api.glyphAdvances.size() < actualGlyphCount)
                    {
                        // Grow the buffer by at least 1.5x and at least of `actualGlyphCount` items.
                        // The 1.5x growth ensures we don't reallocate every time we need 1 more slot.
                        auto size = _api.glyphAdvances.size();
                        size = size + (size >> 1);
                        size = std::max<size_t>(size, actualGlyphCount);
                        _api.glyphAdvances = Buffer<f32>{ size };
                        _api.glyphOffsets = Buffer<DWRITE_GLYPH_OFFSET>{ size };
                    }

                    THROW_IF_FAILED(_p.textAnalyzer->GetGlyphPlacements(
                        /* textString          */ _api.bufferLine.data() + a.textPosition,
                        /* clusterMap          */ _api.clusterMap.data(),
                        /* textProps           */ _api.textProps.data(),
                        /* textLength          */ a.textLength,
                        /* glyphIndices        */ _api.glyphIndices.data(),
                        /* glyphProps          */ _api.glyphProps.data(),
                        /* glyphCount          */ actualGlyphCount,
                        /* fontFace            */ mappedFontFace.get(),
                        /* fontEmSize          */ _p.s->font->fontSizeInDIP,
                        /* isSideways          */ false,
                        /* isRightToLeft       */ a.bidiLevel & 1,
                        /* scriptAnalysis      */ &scriptAnalysis,
                        /* localeName          */ nullptr,
                        /* features            */ &features,
                        /* featureRangeLengths */ &featureRangeLengths,
                        /* featureRanges       */ featureRanges,
                        /* glyphAdvances       */ _api.glyphAdvances.data(),
                        /* glyphOffsets        */ _api.glyphOffsets.data()));

                    _api.textProps[a.textLength - 1].canBreakShapingAfter = 1;

                    size_t beg = 0;
                    for (size_t i = 0; i < a.textLength; ++i)
                    {
                        if (_api.textProps[i].canBreakShapingAfter)
                        {
                            if (_emplaceGlyph(mappedFontFace.get(), a.textPosition + beg, a.textPosition + i + 1))
                            {
                                beg = i + 1;
                            }
                        }
                    }
                }
            }
        }
    }
}
// ^^^ Look at that amazing 8-fold nesting level. Lovely. <3

bool AtlasEngine::_emplaceGlyph(IDWriteFontFace* fontFace, size_t bufferPos1, size_t bufferPos2)
{
    static constexpr auto replacement = L'\uFFFD';

    // This would seriously blow us up otherwise.
    Expects(bufferPos1 < bufferPos2 && bufferPos2 <= _api.bufferLine.size());

    // _flushBufferLine() ensures that bufferLineColumn.size() > bufferLine.size().
    const auto x1 = _api.bufferLineColumn[bufferPos1];
    const auto x2 = _api.bufferLineColumn[bufferPos2];

    // x1 == x2, if our TextBuffer and DirectWrite disagree where glyph boundaries are. Example:
    // Our line of text contains a wide glyph consisting of 2 surrogate pairs "xx" and "yy".
    // If DirectWrite considers the first "xx" to be separate from the second "yy", we'll get:
    //   _api.bufferLine       = "...xxyy..."
    //   _api.bufferLineColumn = {01233335678}
    //                               ^ ^
    //                              /   \
    //                      bufferPos1  bufferPos2
    //   x1: _api.bufferLineColumn[bufferPos1] == 3
    //   x1: _api.bufferLineColumn[bufferPos2] == 3
    // --> cellCount (which is x2 - x1) is now 0 (invalid).
    //
    // Assuming that the TextBuffer implementation doesn't have any bugs...
    // I'm not entirely certain why this occurs, but to me, a layperson, it appears as if
    // IDWriteFontFallback::MapCharacters() doesn't respect extended grapheme clusters.
    // It could also possibly be due to a difference in the supported Unicode version.
    if (x1 >= x2 || x2 > _api.s->cellCount.x)
    {
        return false;
    }

    const auto chars = fontFace ? &_api.bufferLine[bufferPos1] : &replacement;
    const auto charCount = fontFace ? bufferPos2 - bufferPos1 : 1;
    const u16 cellCount = x2 - x1;

    auto attributes = _api.attributes;
    attributes.cellCount = cellCount;

    AtlasKey key{ attributes, gsl::narrow<u16>(charCount), chars };
    auto it = _p.glyphs.find(key);

    if (it == _p.glyphs.end())
    {
        // Do fonts exist *in practice* which contain both colored and uncolored glyphs? I'm pretty sure...
        // However doing it properly means using either of:
        // * IDWriteFactory2::TranslateColorGlyphRun
        // * IDWriteFactory4::TranslateColorGlyphRun
        // * IDWriteFontFace4::GetGlyphImageData
        //
        // For the first two I wonder how one is supposed to restitch the 27 required parameters from a
        // partial (!) text range returned by GetGlyphs(). Our caller breaks the GetGlyphs() result up
        // into runs of characters up until the first canBreakShapingAfter == true after all.
        // There's no documentation for it and I'm sure I'd mess it up.
        // For very obvious reasons I didn't want to write this code.
        //
        // IDWriteFontFace4::GetGlyphImageData seems like the best approach and DirectWrite uses the
        // same code that GetGlyphImageData uses to implement TranslateColorGlyphRun (well partially).
        // However, it's a heavy operation and parses the font file on disk on every call (TranslateColorGlyphRun doesn't).
        // For less obvious reasons I didn't want to write this code either.
        //
        // So this is a job for future me/someone.
        // Bonus points for doing it without impacting performance.
        auto flags = CellFlags::None;
        if (fontFace)
        {
            const auto fontFace2 = wil::try_com_query<IDWriteFontFace2>(fontFace);
            WI_SetFlagIf(flags, CellFlags::ColoredGlyph, fontFace2 && fontFace2->IsColorFont());
        }

        // The AtlasValue constructor fills the `coords` variable with a pointer to an array
        // of at least `cellCount` elements. I did this so that I don't have to type out
        // `value.data()->coords` again, despite the constructor having all the data necessary.
        u16x2* coords;
        AtlasValue value{ flags, cellCount, &coords };

        for (u16 i = 0; i < cellCount; ++i)
        {
            coords[i] = _p.tileAllocator.allocate(_p.glyphs);
        }

        it = _p.glyphs.insert(std::move(key), std::move(value));
        _p.glyphQueue.emplace_back(it);
    }

    const auto valueData = it->second.data();
    const auto coords = &valueData->coords[0];
    const auto cells = _getCell(x1, _api.lastPaintBufferLineCoord.y);
    const auto cellGlyphMappings = _getCellGlyphMapping(x1, _api.lastPaintBufferLineCoord.y);

    for (u32 i = 0; i < cellCount; ++i)
    {
        cells[i].tileIndex = coords[i];
        // We should apply the column color and flags from each column (instead
        // of copying them from the x1) so that ligatures can appear in multiple
        // colors with different line styles.
        cells[i].flags = valueData->flags | _api.bufferLineMetadata[static_cast<size_t>(x1) + i].flags;
        cells[i].color = _api.bufferLineMetadata[static_cast<size_t>(x1) + i].colors;
    }

    std::fill_n(cellGlyphMappings, cellCount, it);
    return true;
}
