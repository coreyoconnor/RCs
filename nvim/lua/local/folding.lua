local F = require('local.functions')
local map = F.map
local shiftk = F.shiftk
local api = vim.api

return {
  config = function ()
    vim.opt.foldtext = ""
    vim.opt.foldlevel = 99
    vim.opt.foldlevelstart = 4
    vim.opt.foldenable = true

    vim.keymap.set('n', 'zR', require('ufo').openAllFolds)
    vim.keymap.set('n', 'zM', require('ufo').closeAllFolds)

    require('ufo').setup({})
     --   provider_selector = function(bufnr, filetype, buftype)
--            return {'treesitter', 'indent'}
--        end
--    })
  end
}

