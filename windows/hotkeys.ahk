#SingleInstance force
#include %A_ScriptDir%\desktop_switcher.ahk

#F12::Edit

;; Launch Firefox
#c::run firefox

;; Launch Emacs
#\::Run C:\emacs\bin\runemacs.exe

;; Minimize current window
$Insert::WinMinimize, A

;; Pause autohotkey
$ScrollLock::Suspend
