#!/usr/bin/env python3

import i3ipc

counts = {}
i3 = i3ipc.Connection()

def fibonacci(con, num):
    print(num)
    if num % 2 == 0:
        i3.command('split h')
    else:
        i3.command('split v')

def on_window_open(i3, e):
    con = i3.get_tree().find_focused()
    ws  = con.workspace().name

    if ws in counts.keys():
        counts[ws] += 1
    else:
        counts[ws] = 0

    fibonacci(con, counts[ws])

def on_window_close(i3, e):
    con = i3.get_tree().find_focused()
    ws  = con.workspace().name

    counts[ws] -= 1

def on_window_move(i3, e):
    print('halo?')

i3.on('window::new', on_window_open)
i3.on('window::move', on_window_move)
i3.on('window::close', on_window_close)

i3.main()
