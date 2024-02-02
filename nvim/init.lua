local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end

vim.opt.rtp:prepend(lazypath)


local api = vim.api
local cmd = vim.cmd
local map = vim.keymap.set

local lspconfig = require('local.lspconfig')
local metals = require('local.metals')

----------------------------------
-- PLUGINS -----------------------
----------------------------------
--
require("lazy").setup({
  'wbthomason/packer.nvim',
  {
    'nvim-telescope/telescope.nvim',
     dependencies = {
      'nvim-lua/plenary.nvim'
    }
  },

  {
    'hrsh7th/vim-vsnip',
    dependencies = {
      'hrsh7th/vim-vsnip-integ'
    }
  },

  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      { "hrsh7th/cmp-nvim-lsp" },
      { "hrsh7th/cmp-vsnip" },
      { "hrsh7th/vim-vsnip" },
    },
    config = function ()
      local cmp = require('cmp')
      cmp.setup {
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "vsnip" },
        }, {
            { name = "buffer" }
        }),
        snippet = {
          expand = function(args)
            -- Comes from vsnip
            vim.fn["vsnip#anonymous"](args.body)
          end,
        },
        sorting = {
          comparators = {
            -- cmp.config.compare.offset,
            cmp.config.compare.exact,
            cmp.config.compare.score,
            cmp.config.compare.kind,
            -- cmp.config.compare.sort_text,
            cmp.config.compare.length,
            cmp.config.compare.order,
          }
        },
        mapping = cmp.mapping.preset.insert {
          -- None of this made sense to me when first looking into this since there
          -- is no vim docs, but you can't have select = true here _unless_ you are
          -- also using the snippet stuff. So keep in mind that if you remove
          -- snippets you need to remove this select
          ["<CR>"] = cmp.mapping.confirm({ select = true }),
          ['<C-Space>'] = cmp.mapping.complete(),
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
    end
  },

  {
    "scalameta/nvim-metals",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "mfussenegger/nvim-dap",
    },
    ft = metals.ft,
    opts = metals.opts,
    config = metals.config
  },

  {
    'neovim/nvim-lspconfig',
    dependencies = {
      'folke/neodev.nvim'
    },
    config = lspconfig.config
  },

  {
    'monsonjeremy/onedark.nvim',
    branch = 'treesitter'
  },

  {
    'nvim-treesitter/nvim-treesitter',
    build = ":TSUpdate",
    config = function ()
      local configs = require("nvim-treesitter.configs")

      configs.setup({
        -- ensure_installed = { "lua", "vim", "vimdoc", "bash", "scala", "javascript", "html", "dockerfile", "sql", "python", "hocon", "yaml" },
        ensure_installed = { "scala", "javascript", "html", "dockerfile", "sql", "hocon" },
        auto_install = false,
        sync_install = false,
        highlight = { enable = true },
        indent = { enable = true },
      })
    end

  },

  {
    'nvim-treesitter/nvim-treesitter-context',
    dependencies = {
      'nvim-treesitter/nvim-treesitter'
    },
    config = function()
      require('treesitter-context').setup {
        max_lines = 4,
        min_window_height = 30
      }
    end
  },

  {
    'nvim-tree/nvim-tree.lua',
    dependencies = {
      'nvim-tree/nvim-web-devicons',
    },
  },

  {
    'NeogitOrg/neogit',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope.nvim',
      'sindrets/diffview.nvim'
    }
  },

  {
    'ntpeters/vim-better-whitespace',
  },

  {
    'HiPhish/rainbow-delimiters.nvim'
  },

  {
    'HiPhish/jinja.vim',
  },

  {
    'ahmedkhalf/project.nvim',
    config = function()
      require('project_nvim').setup {
        -- https://github.com/ahmedkhalf/project.nvim
      }
    end
  },

  {
    'TamaMcGlinn/quickfixdd',
  },

  {
    'rose-pine/neovim',
    name = 'rose-pine'
  },

  {
    "jackMort/ChatGPT.nvim",
    config = function()
      require("chatgpt").setup({
        api_key_cmd = "secret-tool lookup openai api-key",
        openai_params = {
          model = "gpt-4",
          max_tokens = 8000
        },
        openai_edit_params = {
          model = "gpt-4"
        }
      })
    end,
    dependencies = {
      "MunifTanjim/nui.nvim",
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim"
    }
  },

  {
    'nvimdev/hlsearch.nvim',
    event = 'BufRead',
    config = function()
      require('hlsearch').setup()
    end
  },

  {
    'bignimbus/pop-punk.vim',
    name = 'pop-punk'
  }
})

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
vim.cmd([[ set spell ]])

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


-- require('onedark').setup()
-- vim.cmd[[colorscheme rose-pine]]
vim.cmd[[colorscheme pop-punk]]

local function tree_on_attach(bufnr)
  local api = require "nvim-tree.api"

  local function opts(desc)
    return { desc = "nvim-tree: " .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
  end

  -- default mappings
  api.config.mappings.default_on_attach(bufnr)
  vim.keymap.set("n", "T",  vim.tree.toggle, {})
end

require("nvim-tree").setup({
  sync_root_with_cwd = true,
  respect_buf_cwd = true,
  update_focused_file = {
    enable = true,
    update_root = true
  },
  view = {
    width = 50,
  },
})

local tree_api = require "nvim-tree.api"
vim.keymap.set("n", "T",  tree_api.tree.toggle, {})

vim.api.nvim_create_autocmd(
  "FileType", {
  pattern={"qf"},
  command=[[nnoremap <buffer> <CR> <CR>:cclose<CR>]]}
)

local telescope_actions = require("telescope.actions")
local telescope_builtin = require('telescope.builtin')

require("telescope").setup({
  defaults = {
    layout_strategy = 'center',
    layout_config = {
      center = { width = 130 }
    },
    wrap_results = true,
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

vim.keymap.set('n', ',e', ':e <C-R>=expand("%:p:h") . "/" <CR>', {})
vim.keymap.set('n', ',t', ':tabe <C-R>=expand("%:p:h") . "/" <CR>', {})
vim.keymap.set('n', ',s', ':split <C-R>=expand("%:p:h") . "/" <CR>', {})
vim.keymap.set('n', ',v', ':vsplit <C-R>=expand("%:p:h") . "/" <CR>', {})

-- This module contains a number of default definitions
local rainbow_delimiters = require 'rainbow-delimiters'

vim.g.rainbow_delimiters = {
    strategy = {
        [''] = rainbow_delimiters.strategy['global'],
        vim = rainbow_delimiters.strategy['local'],
    },
    query = {
        [''] = 'rainbow-delimiters',
        lua = 'rainbow-blocks',
    },
    highlight = {
        'RainbowDelimiterRed',
        'RainbowDelimiterYellow',
        'RainbowDelimiterBlue',
        'RainbowDelimiterOrange',
        'RainbowDelimiterGreen',
        'RainbowDelimiterViolet',
        'RainbowDelimiterCyan',
    },
}

-- strip whitespace on save
vim.api.nvim_create_autocmd( "FileType", {
  pattern = { 'text', 'markdown', 'html', 'xhtml', 'javascript', 'typescript', 'scala', 'c', 'java', 'go', 'rust', 'c++', 'lua' },
  command = 'EnableStripWhitespaceOnSave'
})

vim.g.strip_whitespace_confirm = 0

