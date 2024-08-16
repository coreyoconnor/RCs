local F = require('local.functions')
local lspconfig = require('local.lspconfig')
local map = F.map
local shiftk = F.shiftk
local api = vim.api

local config = function ()
  require("telescope").setup({
    defaults = {
      layout_strategy = 'center',
      layout_config = {
        center = { width = 130 }
      },
      wrap_results = true,
      mappings = {
        i = {
          ["<esc>"] = require("telescope.actions").close,
        }
      }
    }
  })
  local telescope_builtin = require('telescope.builtin')
  local telescope_utils = require('telescope.utils')

  local find_project_files = function(opts)
    local project_dir = vim.fn.systemlist("git rev-parse --show-toplevel")[1]

    print(project_dir)

    if vim.v.shell_error ~= 0 then
      -- if not git then active lsp client root
      -- will get the configured root directory of the first attached lsp. You will have problems if you are using multiple lsps
      -- opts.cwd = vim.lsp.get_active_clients()[1].config.root_dir
      telescope_builtin.find_files(opts)
    end

    opts = opts or {}
    opts.cwd = project_dir
    telescope_builtin.find_files(opts)
  end

  local live_grep_from_here = function (opts)
    opts = opts or {}
    opts.cwd = telescope_utils.buffer_dir()
    telescope_builtin.live_grep(opts)
  end

  vim.keymap.set('n', '<leader>fb', telescope_builtin.buffers, {})
  vim.keymap.set('n', '<leader>ff', find_project_files, {})
  vim.keymap.set('n', '<leader>fg', telescope_builtin.live_grep, {})
  vim.keymap.set('n', '<leader>fG', live_grep_from_here, {})
  vim.keymap.set('n', '<leader>fh', telescope_builtin.help_tags, {})
  vim.keymap.set('n', '<leader>fk', telescope_builtin.keymaps, {})
  vim.keymap.set('n', '<leader>ft', telescope_builtin.treesitter, {})
end

return {
  config = config
}
