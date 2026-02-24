local M = {}

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
