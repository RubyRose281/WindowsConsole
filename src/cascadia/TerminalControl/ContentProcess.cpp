// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

#include "pch.h"
#include "ContentProcess.h"
#include "ContentProcess.g.cpp"
#include "ControlCore.h"

namespace winrt::Microsoft::Terminal::Control::implementation
{
    ContentProcess::ContentProcess(winrt::guid g) :
        _ourPID{ GetCurrentProcessId() },
        _guid{ g } {}

    bool ContentProcess::Initialize(Control::IControlSettings settings,
                                    Control::IControlAppearance unfocusedAppearance,
                                    TerminalConnection::ConnectionInformation connectionInfo)
    {
        auto conn{ TerminalConnection::ConnectionInformation::CreateConnection(connectionInfo) };
        if (conn == nullptr)
        {
            return false;
        }
        _interactivity = winrt::make<implementation::ControlInteractivity>(settings, unfocusedAppearance, conn);
        return true;
    }

    ContentProcess::~ContentProcess()
    {
    }

    // See https://docs.microsoft.com/en-us/windows/uwp/cpp-and-winrt-apis/details-about-destructors#deferred-destruction
    winrt::fire_and_forget ContentProcess::final_release(std::unique_ptr<ContentProcess> ptr) noexcept
    {
        winrt::com_ptr<ControlCore> coreImpl;
        coreImpl.copy_from(winrt::get_self<ControlCore>(ptr->_interactivity.Core()));
        if (coreImpl)
        {
            // Close() requires that it is called on the "main" thread. So we
            // need to switch over to the DIspatcher thread, before calling
            // Close.
            co_await wil::resume_foreground(coreImpl->Dispatcher(), winrt::Windows::System::DispatcherQueuePriority::Normal);

            // Typically, Close() runs async, closing the connection on a BG
            // thread, so that the UI doesn't hang while waiting for the client
            // process to exit. When we're running as a content process, that's
            // not relevant. In that case, we need to close the connection NOW,
            // because we're about to exit the whole process. If we close the
            // process asynchronously (on a bg thread), then we might
            // accidentally leak it as we exit() before the thread gets a time
            // slice.
            coreImpl->Close(false);
        }

        // DANGER - We're straight up going to EXIT THE ENTIRE PROCESS when we
        // get destructed. This eliminates the need to do any sort of
        // ref-counting weirdness. This entire process exists to host one
        // singular ContentProcess instance. When we're destructed, it's because
        // every other window process was done with us. We can die now, knowing
        // that our job is complete.

        std::exit(0);
    }

    Control::ControlInteractivity ContentProcess::GetInteractivity()
    {
        return _interactivity;
    }

    uint64_t ContentProcess::GetPID()
    {
        return _ourPID;
    }

    // Method Description:
    // - Duplicate the swap chain handle to the provided process.
    // - If the provided PID is our pid, then great - we don't need to do anything.
    // Arguments:
    // - callersPid: the PID of the process calling this method.
    // Return Value:
    // - The value of the swapchain handle in the callers process
    // Notes:
    // - This is BODGY! We're basically asking to marshal a HANDLE here. WinRT
    //   has no good mechanism for doing this, so we're doing it by casting the
    //   value to a uint64_t. In all reality, we _should_ be using a COM
    //   interface for this, because it can set up the security on these handles
    //   more appropriately. Fortunately, all we're dealing with is swapchains,
    //   so the security doesn't matter all that much.
    uint64_t ContentProcess::RequestSwapChainHandle(const uint64_t callersPid)
    {
        auto ourPid = GetCurrentProcessId();
        HANDLE ourHandle = reinterpret_cast<HANDLE>(_interactivity.Core().SwapChainHandle());
        if (callersPid == ourPid)
        {
            return reinterpret_cast<uint64_t>(ourHandle);
        }

        wil::unique_handle hWindowProcess{ OpenProcess(PROCESS_ALL_ACCESS,
                                                       FALSE,
                                                       static_cast<DWORD>(callersPid)) };
        if (hWindowProcess.get() == nullptr)
        {
            TraceLoggingWrite(g_hTerminalControlProvider,
                              "ContentProcess::RequestSwapChainHandle_OpenOtherProcessFailed",
                              TraceLoggingLevel(WINEVENT_LEVEL_VERBOSE),
                              TraceLoggingKeyword(TIL_KEYWORD_TRACE));

            LOG_LAST_ERROR();
            return 0;
        }

        HANDLE theirHandle{ nullptr };
        BOOL success = DuplicateHandle(GetCurrentProcess(),
                                       ourHandle,
                                       hWindowProcess.get(),
                                       &theirHandle,
                                       0,
                                       FALSE,
                                       DUPLICATE_SAME_ACCESS);
        if (!success)
        {
            TraceLoggingWrite(g_hTerminalControlProvider,
                              "ContentProcess::RequestSwapChainHandle_DuplicateHandleFailed",
                              TraceLoggingLevel(WINEVENT_LEVEL_VERBOSE),
                              TraceLoggingKeyword(TIL_KEYWORD_TRACE));

            LOG_LAST_ERROR();
            return 0;
        }

        // At this point, the handle is now in their process space, with value
        // theirHandle
        return reinterpret_cast<uint64_t>(theirHandle);
    }

    winrt::guid ContentProcess::Guid()
    {
        return _guid;
    }
}