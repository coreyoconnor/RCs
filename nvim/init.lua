local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

local api = vim.api
local cmd = vim.cmd
local map = vim.keymap.set

----------------------------------
-- PLUGINS -----------------------
----------------------------------
cmd([[packadd packer.nvim]])
require("packer").startup(function(use)
  use 'wbthomason/packer.nvim'
  
  use {
    'nvim-telescope/telescope.nvim', tag = '0.1.2',
     requires = { {'nvim-lua/plenary.nvim'} }
  }

  use({
    "hrsh7th/nvim-cmp",
    requires = {
      { "hrsh7th/cmp-nvim-lsp" },
      { "hrsh7th/cmp-vsnip" },
      { "hrsh7th/vim-vsnip" },
    },
  })
  use({
    "scalameta/nvim-metals",
    requires = {
      "nvim-lua/plenary.nvim",
      "mfussenegger/nvim-dap",
    },
  })

  use {
    'neovim/nvim-lspconfig',
    requires = {
      'folke/neodev.nvim'
    }
  }

  use({ 'monsonjeremy/onedark.nvim', branch = 'treesitter' })

  use {
    'nvim-treesitter/nvim-treesitter',
    run = function()
      local ts_update = require('nvim-treesitter.install').update({ with_sync = true })
      ts_update()
    end,
  }

  use {
    'nvim-tree/nvim-tree.lua',
    requires = {
      'nvim-tree/nvim-web-devicons',
    },
  }

  use {
    'NeogitOrg/neogit',
    requires = {
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope.nvim',
      'sindrets/diffview.nvim'
    }
  }

  if packer_bootstrap then
    require('packer').sync()
  end
end)

----------------------------------
-- OPTIONS -----------------------
----------------------------------
-- global
local options_settings = {
  autoindent     = true,
  backspace      = { "indent", "eol", "start" },
  clipboard      = "unnamed",
  completeopt    = { "menuone", "noinsert", "noselect" },
  cursorline     = true,
  cursorlineopt  = "number",
  expandtab      = true,
  hlsearch       = true,
  ignorecase     = true,
  incsearch      = true,
  laststatus     = 2,
  linebreak      = true,
  number         = true,
  relativenumber = true,
  scrolloff      = 5,
  shiftwidth     = 2,
  showmatch      = true,
  smartcase      = true,
  smartindent    = true,
  smarttab       = true,
  softtabstop    = 2,
  tabstop        = 2,
  textwidth      = 110,
  timeoutlen     = 1000,
  undofile       = true,
  updatetime     = 750,
  wrap           = false,
}
for name, setting in pairs(options_settings) do
  vim.opt[name] = setting
end

vim.cmd([[ syntax on ]])
vim.cmd([[ filetype on ]])
vim.cmd([[ filetype indent off ]])

-- define custom symbols for non-printing chars
vim.cmd([[ set listchars=eol:¬,tab:>·,trail:~,extends:>,precedes:<,space:␣ ]])
-- don't fold until the first request for a fold toggle
vim.cmd([[ set nofoldenable ]])
-- highlight column at max width
vim.cmd([[ set colorcolumn=+1 ]])

-- disable netrw at the very start of your init.lua
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- set termguicolors to enable highlight groups
vim.opt.termguicolors = true
vim.opt_global.completeopt = { "menuone", "noinsert", "noselect" }

-- all workspace diagnostics
map("n", "<leader>aa", vim.diagnostic.setqflist)

-- all workspace errors
map("n", "<leader>ae", function()
  vim.diagnostic.setqflist({ severity = "E" })
end)

-- all workspace warnings
map("n", "<leader>aw", function()
  vim.diagnostic.setqflist({ severity = "W" })
end)

-- buffer diagnostics only
map("n", "<leader>d", vim.diagnostic.setloclist)

map("n", "[c", function()
  vim.diagnostic.goto_prev({ wrap = false })
end)

map("n", "]c", function()
  vim.diagnostic.goto_next({ wrap = false })
end)


require('onedark').setup()
vim.cmd[[colorscheme onedark]]

require'nvim-treesitter.configs'.setup {
  ensure_installed = { "bash", "lua", "python", "scala", "hocon", "yaml", "sql", "dockerfile", "vim" },
  auto_install = true,
  highlight = {
    enable = true
  },
  indent = {
    enable = true
  },
}

local function tree_on_attach(bufnr)
  local api = require "nvim-tree.api"

  local function opts(desc)
    return { desc = "nvim-tree: " .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
  end

  -- default mappings
  api.config.mappings.default_on_attach(bufnr)
  vim.keymap.set("n", "T",  vim.tree.toggle, {})
end

require("nvim-tree").setup()

local tree_api = require "nvim-tree.api"
vim.keymap.set("n", "T",  tree_api.tree.toggle, {})

vim.api.nvim_create_autocmd(
  "FileType", {
  pattern={"qf"},
  command=[[nnoremap <buffer> <CR> <CR>:cclose<CR>]]}
)

require('local.lsp').setup()

-- completion related settings
-- This is similiar to what I use
local cmp = require("cmp")
cmp.setup {
  sources = {
    { name = "nvim_lsp" },
    { name = "vsnip" },
  },
  snippet = {
    expand = function(args)
      -- Comes from vsnip
      vim.fn["vsnip#anonymous"](args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert {
    -- None of this made sense to me when first looking into this since there
    -- is no vim docs, but you can't have select = true here _unless_ you are
    -- also using the snippet stuff. So keep in mind that if you remove
    -- snippets you need to remove this select
    ["<CR>"] = cmp.mapping.confirm({ select = true }),
    -- I use tabs... some say you should stick to ins-completion but this is just here as an example
    ["<Tab>"] = function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      else
        fallback()
      end
    end,
    ["<S-Tab>"] = function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      else
        fallback()
      end
    end,
  }
}

local telescope_actions = require("telescope.actions")
local telescope_builtin = require('telescope.builtin')

require("telescope").setup({
  defaults = {
    mappings = {
      i = {
        ["<esc>"] = telescope_actions.close,
      }
    }
  }
})

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

vim.keymap.set('n', '<leader>ff', find_project_files, {})
vim.keymap.set('n', '<leader>fg', telescope_builtin.live_grep, {})
vim.keymap.set('n', '<leader>fb', telescope_builtin.buffers, {})
vim.keymap.set('n', '<leader>fh', telescope_builtin.help_tags, {})

local neogit = require('neogit')
neogit.setup {}

vim.cmd [[autocmd! CursorHold,CursorHoldI * lua vim.diagnostic.open_float(nil, {focus=false})]]

