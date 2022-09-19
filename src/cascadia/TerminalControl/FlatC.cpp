#include "pch.h"
#include "FlatC.h"
#include "winrt/Microsoft.Terminal.Control.h"
#include "winrt/Microsoft.Terminal.Core.h"
#include "../TerminalCore/ControlKeyStates.hpp"
#include "../types/inc/colorTable.hpp"
#include "../inc/DefaultSettings.h"
#include <windowsx.h>

#pragma warning(disable : 4100)

using namespace winrt::Microsoft::Terminal::Control;
using namespace winrt::Microsoft::Terminal::Core;
using CKS = ::Microsoft::Terminal::Core::ControlKeyStates;

static CKS getControlKeyState() noexcept
{
    struct KeyModifier
    {
        int vkey;
        CKS flags;
    };

    constexpr std::array<KeyModifier, 5> modifiers{ {
        { VK_RMENU, CKS::RightAltPressed },
        { VK_LMENU, CKS::LeftAltPressed },
        { VK_RCONTROL, CKS::RightCtrlPressed },
        { VK_LCONTROL, CKS::LeftCtrlPressed },
        { VK_SHIFT, CKS::ShiftPressed },
    } };

    CKS flags;

    for (const auto& mod : modifiers)
    {
        const auto state = GetKeyState(mod.vkey);
        const auto isDown = state < 0;

        if (isDown)
        {
            flags |= mod.flags;
        }
    }

    return flags;
}

static LPCWSTR term_window_class = L"HwndTerminalClass";

// This magic flag is "documented" at https://msdn.microsoft.com/en-us/library/windows/desktop/ms646301(v=vs.85).aspx
// "If the high-order bit is 1, the key is down; otherwise, it is up."
static constexpr short KeyPressed{ gsl::narrow_cast<short>(0x8000) };

static constexpr bool _IsMouseMessage(UINT uMsg)
{
    return uMsg == WM_LBUTTONDOWN || uMsg == WM_LBUTTONUP || uMsg == WM_LBUTTONDBLCLK ||
           uMsg == WM_MBUTTONDOWN || uMsg == WM_MBUTTONUP || uMsg == WM_MBUTTONDBLCLK ||
           uMsg == WM_RBUTTONDOWN || uMsg == WM_RBUTTONUP || uMsg == WM_RBUTTONDBLCLK ||
           uMsg == WM_MOUSEMOVE || uMsg == WM_MOUSEWHEEL || uMsg == WM_MOUSEHWHEEL;
}

#include "../inc/cppwinrt_utils.h"

struct NullConnection : public winrt::implements<NullConnection, winrt::Microsoft::Terminal::TerminalConnection::ITerminalConnection>
{
    void Initialize(IInspectable x) {}
    void Start() {}
    void WriteInput(winrt::hstring d)
    {
        if (_pfnWriteCallback)
        {
            _pfnWriteCallback(d.data());
        }
    }
    void Resize(uint32_t r, uint32_t c) {}
    void Close() {}
    winrt::Microsoft::Terminal::TerminalConnection::ConnectionState State() const noexcept { return winrt::Microsoft::Terminal::TerminalConnection::ConnectionState::Closed; }
    WINRT_CALLBACK(TerminalOutput, winrt::Microsoft::Terminal::TerminalConnection::TerminalOutputHandler);

    TYPED_EVENT(StateChanged, winrt::Microsoft::Terminal::TerminalConnection::ITerminalConnection, winrt::Windows::Foundation::IInspectable);

public:
    std::function<void(const wchar_t*)> _pfnWriteCallback{ nullptr };
    void WireOutput(const wchar_t* data)
    {
        _TerminalOutputHandlers(winrt::to_hstring(data));
    }
};
struct TerminalSettings : winrt::implements<TerminalSettings, IControlSettings, ICoreSettings, IControlAppearance, ICoreAppearance>
{
    using IFontAxesMap = winrt::Windows::Foundation::Collections::IMap<winrt::hstring, float>;
    using IFontFeatureMap = winrt::Windows::Foundation::Collections::IMap<winrt::hstring, uint32_t>;
    TerminalSettings()
    {
        const auto campbellSpan = Microsoft::Console::Utils::CampbellColorTable();
        std::transform(campbellSpan.begin(), campbellSpan.end(), _ColorTable.begin(), [](auto&& color) {
            return static_cast<winrt::Microsoft::Terminal::Core::Color>(til::color{ color });
        });
    }
    ~TerminalSettings() = default;

    winrt::Microsoft::Terminal::Core::Color GetColorTableEntry(int32_t index) noexcept { return _ColorTable.at(index); }

    WINRT_PROPERTY(til::color, DefaultForeground, DEFAULT_FOREGROUND);
    WINRT_PROPERTY(til::color, DefaultBackground, DEFAULT_BACKGROUND);
    WINRT_PROPERTY(til::color, SelectionBackground, DEFAULT_FOREGROUND);
    WINRT_PROPERTY(int32_t, HistorySize, DEFAULT_HISTORY_SIZE);
    WINRT_PROPERTY(int32_t, InitialRows, 30);
    WINRT_PROPERTY(int32_t, InitialCols, 80);
    WINRT_PROPERTY(bool, SnapOnInput, true);
    WINRT_PROPERTY(bool, AltGrAliasing, true);
    WINRT_PROPERTY(til::color, CursorColor, DEFAULT_CURSOR_COLOR);
    WINRT_PROPERTY(winrt::Microsoft::Terminal::Core::CursorStyle, CursorShape, winrt::Microsoft::Terminal::Core::CursorStyle::Vintage);
    WINRT_PROPERTY(uint32_t, CursorHeight, DEFAULT_CURSOR_HEIGHT);
    WINRT_PROPERTY(winrt::hstring, WordDelimiters, DEFAULT_WORD_DELIMITERS);
    WINRT_PROPERTY(bool, CopyOnSelect, false);
    WINRT_PROPERTY(bool, InputServiceWarning, true);
    WINRT_PROPERTY(bool, FocusFollowMouse, false);
    WINRT_PROPERTY(bool, TrimBlockSelection, false);
    WINRT_PROPERTY(bool, DetectURLs, true);
    WINRT_PROPERTY(winrt::Windows::Foundation::IReference<winrt::Microsoft::Terminal::Core::Color>, TabColor, nullptr);
    WINRT_PROPERTY(winrt::Windows::Foundation::IReference<winrt::Microsoft::Terminal::Core::Color>, StartingTabColor, nullptr);
    WINRT_PROPERTY(winrt::hstring, ProfileName);
    WINRT_PROPERTY(bool, UseAcrylic, false);
    WINRT_PROPERTY(double, Opacity, 1.0);
    WINRT_PROPERTY(winrt::hstring, Padding, DEFAULT_PADDING);
    WINRT_PROPERTY(winrt::hstring, FontFace, L"Cascadia Code");
    WINRT_PROPERTY(float, FontSize, DEFAULT_FONT_SIZE);
    WINRT_PROPERTY(winrt::Windows::UI::Text::FontWeight, FontWeight, winrt::Windows::UI::Text::FontWeight{ 400 });
    WINRT_PROPERTY(IFontAxesMap, FontAxes);
    WINRT_PROPERTY(IFontFeatureMap, FontFeatures);
    WINRT_PROPERTY(winrt::hstring, BackgroundImage);
    WINRT_PROPERTY(double, BackgroundImageOpacity, 1.0);
    WINRT_PROPERTY(winrt::Windows::UI::Xaml::Media::Stretch, BackgroundImageStretchMode, winrt::Windows::UI::Xaml::Media::Stretch::UniformToFill);
    WINRT_PROPERTY(winrt::Windows::UI::Xaml::HorizontalAlignment, BackgroundImageHorizontalAlignment, winrt::Windows::UI::Xaml::HorizontalAlignment::Center);
    WINRT_PROPERTY(winrt::Windows::UI::Xaml::VerticalAlignment, BackgroundImageVerticalAlignment, winrt::Windows::UI::Xaml::VerticalAlignment::Center);
    WINRT_PROPERTY(winrt::Microsoft::Terminal::Control::IKeyBindings, KeyBindings, nullptr);
    WINRT_PROPERTY(winrt::hstring, Commandline);
    WINRT_PROPERTY(winrt::hstring, StartingDirectory);
    WINRT_PROPERTY(winrt::hstring, StartingTitle);
    WINRT_PROPERTY(bool, SuppressApplicationTitle);
    WINRT_PROPERTY(winrt::hstring, EnvironmentVariables);
    WINRT_PROPERTY(winrt::Microsoft::Terminal::Control::ScrollbarState, ScrollState, winrt::Microsoft::Terminal::Control::ScrollbarState::Visible);
    WINRT_PROPERTY(winrt::Microsoft::Terminal::Control::TextAntialiasingMode, AntialiasingMode, winrt::Microsoft::Terminal::Control::TextAntialiasingMode::Grayscale);
    WINRT_PROPERTY(bool, RetroTerminalEffect, false);
    WINRT_PROPERTY(bool, ForceFullRepaintRendering, false);
    WINRT_PROPERTY(bool, SoftwareRendering, false);
    WINRT_PROPERTY(bool, ForceVTInput, false);
    WINRT_PROPERTY(winrt::hstring, PixelShaderPath);
    WINRT_PROPERTY(bool, IntenseIsBright);
    WINRT_PROPERTY(bool, IntenseIsBold);
    WINRT_PROPERTY(bool, ShowMarks);
    WINRT_PROPERTY(bool, UseBackgroundImageForWindow);
    WINRT_PROPERTY(bool, AutoMarkPrompts);
    WINRT_PROPERTY(bool, VtPassthrough);
    WINRT_PROPERTY(winrt::hstring, ProfileSource, L"this property was a mistake");
    WINRT_PROPERTY(bool, UseAtlasEngine, true);
    WINRT_PROPERTY(AdjustTextMode, AdjustIndistinguishableColors, AdjustTextMode::Never);

private:
    std::array<winrt::Microsoft::Terminal::Core::Color, 16> _ColorTable;
};

struct HwndTerminal
{
    static LRESULT CALLBACK HwndTerminalWndProc(
        HWND hwnd,
        UINT uMsg,
        WPARAM wParam,
        LPARAM lParam) noexcept
    try
    {
#pragma warning(suppress : 26490) // Win32 APIs can only store void*, have to use reinterpret_cast
        HwndTerminal* terminal = reinterpret_cast<HwndTerminal*>(GetWindowLongPtr(hwnd, GWLP_USERDATA));
        if (terminal)
        {
            return terminal->WindowProc(hwnd, uMsg, wParam, lParam);
        }
        return DefWindowProc(hwnd, uMsg, wParam, lParam);
    }
    catch (...)
    {
        LOG_CAUGHT_EXCEPTION();
        return 0;
    }

    static bool RegisterTermClass(HINSTANCE hInstance) noexcept
    {
        WNDCLASSW wc;
        if (GetClassInfoW(hInstance, term_window_class, &wc))
        {
            return true;
        }

        wc.style = 0;
        wc.lpfnWndProc = HwndTerminal::HwndTerminalWndProc;
        wc.cbClsExtra = 0;
        wc.cbWndExtra = 0;
        wc.hInstance = hInstance;
        wc.hIcon = nullptr;
        wc.hCursor = LoadCursor(nullptr, IDC_ARROW);
        wc.hbrBackground = nullptr;
        wc.lpszMenuName = nullptr;
        wc.lpszClassName = term_window_class;

        return RegisterClassW(&wc) != 0;
    }

    static MouseButtonState MouseButtonStateFromWParam(WPARAM wParam)
    {
        MouseButtonState state{};
        WI_UpdateFlag(state, MouseButtonState::IsLeftButtonDown, WI_IsFlagSet(wParam, MK_LBUTTON));
        WI_UpdateFlag(state, MouseButtonState::IsMiddleButtonDown, WI_IsFlagSet(wParam, MK_MBUTTON));
        WI_UpdateFlag(state, MouseButtonState::IsRightButtonDown, WI_IsFlagSet(wParam, MK_RBUTTON));
        return state;
    }

    static Point PointFromLParam(LPARAM lParam)
    {
        return { GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam) };
    }

    LRESULT WindowProc(
        HWND hwnd,
        UINT uMsg,
        WPARAM wParam,
        LPARAM lParam) noexcept
    {
        switch (uMsg)
        {
        case WM_LBUTTONDOWN:
        case WM_MBUTTONDOWN:
        case WM_RBUTTONDOWN:
            SetCapture(_hwnd.get());
            _interactivity.PointerPressed(
                MouseButtonStateFromWParam(wParam),
                uMsg,
                std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::system_clock::now().time_since_epoch()).count(),
                getControlKeyState(),
                PointFromLParam(lParam));
            return 0;
        case WM_MOUSEMOVE:
            _interactivity.PointerMoved(
                MouseButtonStateFromWParam(wParam),
                WM_MOUSEMOVE,
                getControlKeyState(),
                true,
                PointFromLParam(lParam),
                true);
            return 0;
        case WM_LBUTTONUP:
        case WM_MBUTTONUP:
        case WM_RBUTTONUP:
            _interactivity.PointerReleased(
                MouseButtonStateFromWParam(wParam),
                uMsg,
                getControlKeyState(),
                PointFromLParam(lParam));
            ReleaseCapture();
            return 0;
        case WM_MOUSEWHEEL:
            if (_interactivity.MouseWheel(getControlKeyState(), GET_WHEEL_DELTA_WPARAM(wParam), PointFromLParam(lParam), MouseButtonStateFromWParam(wParam)))
            {
                return 0;
            }
            break;
        case WM_SETFOCUS:
            _interactivity.GotFocus();
            break;
        case WM_KILLFOCUS:
            _interactivity.LostFocus();
            break;
        }

        return DefWindowProcW(hwnd, uMsg, wParam, lParam);
    }

    wil::unique_hwnd _hwnd;

    HwndTerminal(HWND parentHwnd)
    {
        HINSTANCE hInstance = wil::GetModuleInstanceHandle();

        if (RegisterTermClass(hInstance))
        {
            _hwnd.reset(CreateWindowExW(
                0,
                term_window_class,
                nullptr,
                WS_CHILD | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | WS_VISIBLE,
                0,
                0,
                0,
                0,
                parentHwnd,
                nullptr,
                hInstance,
                nullptr));

#pragma warning(suppress : 26490) // Win32 APIs can only store void*, have to use reinterpret_cast
            SetWindowLongPtr(_hwnd.get(), GWLP_USERDATA, reinterpret_cast<LONG_PTR>(this));
        }

        _connection = winrt::make_self<NullConnection>();
        _interactivity = ControlInteractivity{ winrt::make<TerminalSettings>(), nullptr, *_connection };
        _core = _interactivity.Core();
    }

    winrt::com_ptr<NullConnection> _connection;
    ControlInteractivity _interactivity{ nullptr };
    ControlCore _core{ nullptr };

    HRESULT SendOutput(LPCWSTR data)
    try
    {
        _connection->WireOutput(data);
        return S_OK;
    }
    CATCH_RETURN()
    bool _initialized{ false };

    HRESULT RegisterScrollCallback(PSCROLLCB callback) { return S_OK; }
    HRESULT TriggerResize(_In_ til::CoordType width, _In_ til::CoordType height, _Out_ til::size* dimensions)
    {
        if (!_initialized)
            return S_FALSE;
        SetWindowPos(_hwnd.get(), nullptr, 0, 0, width, height, 0);
        _core.SizeChanged(width, height);
        return S_OK;
    }
    HRESULT TriggerResizeWithDimension(_In_ til::size dimensions, _Out_ SIZE* dimensionsInPixels) { return S_OK; }
    HRESULT CalculateResize(_In_ til::CoordType width, _In_ til::CoordType height, _Out_ til::size* dimensions) { return S_OK; }
    HRESULT DpiChanged(int newDpi)
    {
        _core.ScaleChanged((double)newDpi / 96.0);
        return S_OK;
    }
    HRESULT UserScroll(int viewTop) { return S_OK; }
    HRESULT ClearSelection() { return S_OK; }
    HRESULT GetSelection(const wchar_t** out) { return S_OK; }
    HRESULT IsSelectionActive(bool* out) { return _core.HasSelection(); }
    HRESULT SetTheme(TerminalTheme theme, LPCWSTR fontFamily, til::CoordType fontSize, int newDpi) { return S_OK; }
    HRESULT RegisterWriteCallback(PWRITECB callback)
    {
        _connection->_pfnWriteCallback = callback;
        return S_OK;
    }
    HRESULT SendKeyEvent(WORD vkey, WORD scanCode, WORD flags, bool keyDown)
    {
        _core.TrySendKeyEvent(vkey, scanCode, getControlKeyState(), keyDown);
        return S_OK;
    }
    HRESULT SendCharEvent(wchar_t ch, WORD flags, WORD scanCode)
    {
        _core.SendCharEvent(ch, scanCode, getControlKeyState());
        return S_OK;
    }
    HRESULT BlinkCursor()
    {
        _core.CursorOn(!_core.CursorOn());
        return S_OK;
    }
    HRESULT SetCursorVisible(const bool visible)
    {
        _core.CursorOn(visible);
        return S_OK;
    }

    void Initialize()
    {
        RECT windowRect;
        GetWindowRect(_hwnd.get(), &windowRect);
        // BODGY: the +/-1 is because ControlCore will ignore an Initialize with zero size (oops)
        // becuase in the old days, TermControl would accidentally try to resize the Swap Chain to 0x0 (oops)
        // and therefore resize the connection to 0x0 (oops)
        _core.InitializeWithHwnd(windowRect.right - windowRect.left + 1, windowRect.bottom - windowRect.top + 1, 1.0, reinterpret_cast<uint64_t>(_hwnd.get()));
        _interactivity.Initialize();
        _core.EnablePainting();
        _initialized = true;
    }

private:
};

__declspec(dllexport) HRESULT _stdcall CreateTerminal(HWND parentHwnd, _Out_ void** hwnd, _Out_ PTERM* terminal)
{
    auto inner = new HwndTerminal{ parentHwnd };
    *terminal = inner;
    *hwnd = inner->_hwnd.get();
    inner->Initialize();
    return S_OK;
}

__declspec(dllexport) void _stdcall DestroyTerminal(PTERM terminal)
{
    delete (HwndTerminal*)terminal;
}

// Generate all of the C->C++ bridge functions.
#define API_NAME(name) Terminal##name
#define GENERATOR_0(name)                                                 \
    __declspec(dllexport) HRESULT _stdcall API_NAME(name)(PTERM terminal) \
    {                                                                     \
        return ((HwndTerminal*)(terminal))->name();                       \
    }
#define GENERATOR_1(name, t1, a1)                                                \
    __declspec(dllexport) HRESULT _stdcall API_NAME(name)(PTERM terminal, t1 a1) \
    {                                                                            \
        return ((HwndTerminal*)(terminal))->name(a1);                            \
    }
#define GENERATOR_2(name, t1, a1, t2, a2)                                               \
    __declspec(dllexport) HRESULT _stdcall API_NAME(name)(PTERM terminal, t1 a1, t2 a2) \
    {                                                                                   \
        return ((HwndTerminal*)(terminal))->name(a1, a2);                               \
    }
#define GENERATOR_3(name, t1, a1, t2, a2, t3, a3)                                              \
    __declspec(dllexport) HRESULT _stdcall API_NAME(name)(PTERM terminal, t1 a1, t2 a2, t3 a3) \
    {                                                                                          \
        return ((HwndTerminal*)(terminal))->name(a1, a2, a3);                                  \
    }
#define GENERATOR_4(name, t1, a1, t2, a2, t3, a3, t4, a4)                                             \
    __declspec(dllexport) HRESULT _stdcall API_NAME(name)(PTERM terminal, t1 a1, t2 a2, t3 a3, t4 a4) \
    {                                                                                                 \
        return ((HwndTerminal*)(terminal))->name(a1, a2, a3, a4);                                     \
    }
#define GENERATOR_N(name, t1, a1, t2, a2, t3, a3, t4, a4, MACRO, ...) MACRO
#define GENERATOR(...)                                                                                                                            \
    GENERATOR_N(__VA_ARGS__, GENERATOR_4, GENERATOR_4, GENERATOR_3, GENERATOR_3, GENERATOR_2, GENERATOR_2, GENERATOR_1, GENERATOR_1, GENERATOR_0) \
    (__VA_ARGS__)
TERMINAL_API_TABLE(GENERATOR)
#undef GENERATOR_0
#undef GENERATOR_1
#undef GENERATOR_2
#undef GENERATOR_3
#undef GENERATOR_4
#undef GENERATOR_N
#undef GENERATOR

#undef API_NAME
