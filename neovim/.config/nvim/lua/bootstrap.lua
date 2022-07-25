local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end
vim.api.nvim_command 'packadd packer.nvim'

require('packer').startup(function()
    require('plugins')

    if packer_bootstrap then
        require('packer').sync()
    end
end)
