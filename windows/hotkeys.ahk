#SingleInstance force
#include %A_ScriptDir%\desktop_switcher.ahk

#F12::Edit

;; Launch Firefox
#c::run firefox

;; Launch Emacs
#\::Run C:\emacs\bin\runemacs.exe, C:\

;; Minimize current window
$Insert::WinMinimize, A

;; Pause autohotkey
$ScrollLock::Suspend
