local M = {}

function M.tcp_port_open(host, port, timeout_ms)
    local addr = vim.uv.getaddrinfo(host, nil, { socktype = 'stream' })
    if not addr or not addr[1] then
        return false
    end

    local sock = vim.uv.new_tcp()
    local done, ok = false, false
    sock:connect(addr[1].addr, port, function(err)
        ok = err == nil
        done = true
        sock:close()
    end)
    vim.wait(timeout_ms, function() return done end, 5)
    return ok
end

function M.qf_move(delta)
    local qf = vim.fn.getqflist({ idx = 0, size = 0 })
    local idx, size = qf.idx, qf.size

    if size == 0 then
        return vim.notify('Quickfix list is empty', vim.log.levels.INFO)
    end

    local new = idx + delta
    if new < 1 or new > size then
        return vim.notify('No more quickfix items', vim.log.levels.INFO)
    end

    vim.cmd(delta > 0 and 'cnext' or 'cprev')
end

return M
