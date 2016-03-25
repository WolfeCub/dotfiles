-- Makes window movement instant i.e. remove animations
hs.window.animationDuration = 0

-- Moves window to left 50% of screen
hs.hotkey.bind({"cmd", "shift"}, "H", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)

  max_iterm()
end)

-- Moves window to right 50% of screen
hs.hotkey.bind({"cmd", "shift"}, "L", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w / 2)
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)

  max_iterm()
end)

-- Moves window to fill the screen
hs.hotkey.bind({"cmd", "shift"}, "J", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w
  f.h = max.h
  win:setFrame(f)

  max_iterm()
end)

-- Moves the window to the centre and sizes it to be 2/3 of screen
hs.hotkey.bind({"cmd", "shift"}, "K", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w / 3)/2
  f.y = max.y
  f.w = max.w * 2/3
  f.h = max.h
  win:setFrame(f)

  max_iterm()
end)

-- Moves the window to the left 33.3% of the screen
hs.hotkey.bind({"cmd", "shift"}, "N", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w / 3
  f.h = max.h
  win:setFrame(f)

  max_iterm()
end)

-- Moves the window to the right 33.3% of the screen
hs.hotkey.bind({"cmd", "shift"}, ".", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w / 3)*2
  f.y = max.y
  f.w = max.w / 3
  f.h = max.h
  win:setFrame(f)

  max_iterm()
end)

-- Moves the window to the left 66.6% of the screen
hs.hotkey.bind({"cmd", "shift"}, "M", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = (max.w / 3)*2
  f.h = max.h
  win:setFrame(f)

  max_iterm()
end)

-- Moves the window to the right 66.6% of the screen
hs.hotkey.bind({"cmd", "shift"}, ",", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w / 3)
  f.y = max.y
  f.w = 2*(max.w / 3)
  f.h = max.h
  win:setFrame(f)

  max_iterm()
end)

-- Moves the window to the left 40% of the screen
hs.hotkey.bind({"cmd", "shift"}, "U", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w * (4 / 10)
  f.h = max.h
  win:setFrame(f)

  max_iterm()
end)

-- Moves the window to the right 40% of the screen
hs.hotkey.bind({"cmd", "shift"}, "P", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w  * (6/10))
  f.y = max.y
  f.w = max.w * (4 / 10)
  f.h = max.h
  win:setFrame(f)

  max_iterm()
end)

-- Moves the window to the left 60% of the screen
hs.hotkey.bind({"cmd", "shift"}, "I", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w * (6 / 10)
  f.h = max.h
  win:setFrame(f)

  max_iterm()
end)

-- Moves the window to the right 60% of the screen
hs.hotkey.bind({"cmd", "shift"}, "O", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w * (4/10))
  f.y = max.y
  f.w = max.w * (6 / 10)
  f.h = max.h
  win:setFrame(f)

  max_iterm()
end)

-- Launches or focuses iTerm
hs.hotkey.bind({"cmd"}, "\\", function()
    hs.application.launchOrFocus("iTerm2")
end)

-- Launches new Incognito Window IF chrome is active window
function chrome_new_incognito()
    local win = hs.window.focusedWindow()
    if win:application():name() == "Google Chrome" then
        hs.application.launchOrFocus("Google Chrome")
        local chrome = hs.appfinder.appFromName("Google Chrome")
        local str_default = {"File", "New Incognito Window"} 
        local default = chrome:findMenuItem(str_default)

        chrome:selectMenuItem(str_default)
    end
end
hs.hotkey.bind({"cmd", "alt", "shift"}, 'N', chrome_new_incognito)

function max_iterm()
    local win = hs.window.focusedWindow()
    if win:application():name() == "iTerm2" then
        hs.application.launchOrFocus("iTerm2")
        local term = hs.appfinder.appFromName("iTerm2")
        local str_default = {"Window", "Zoom"}
        local default = term:findMenuItem(str_default)

        term:selectMenuItem(str_default)
    end
end
