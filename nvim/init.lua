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

local map = vim.keymap.set

local folding = require('local.folding')
local lspconfig = require('local.lspconfig')
local metals = require('local.metals')
local telescope = require('local.telescope')
local to_install = {
  {
    'nvim-telescope/telescope.nvim',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = telescope.config,
    lazy = false
  },
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
      { 'hrsh7th/cmp-nvim-lsp-signature-help' },
    },
    config = function ()
      local cmp = require('cmp')
      cmp.setup {
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "vsnip" },
          { name = "cmp-nvim-lsp-signature-help" },
        }, {
            { name = "buffer" }
        }),
        performance = {
          fetching_timeout = 2000
        },
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
          ["<C-l>"] = cmp.mapping.confirm({ select = true }),
          ['<C-Space>'] = cmp.mapping.complete(),
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
        },
        formatting = {
          format = function(entry, vim_item)
            vim_item.menu = entry.source.name
            return vim_item
          end
        },
        enabled = function()
          local disabled = false
          disabled = disabled or (vim.api.nvim_buf_get_option(0, "buftype") == "prompt")
          disabled = disabled or (vim.fn.reg_recording() ~= "")
          disabled = disabled or (vim.fn.reg_executing() ~= "")
          disabled = disabled or require("cmp.config.context").in_treesitter_capture("comment")
          disabled = disabled or require"cmp.config.context".in_syntax_group("Comment")
          return not disabled
        end
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
    "j-hui/fidget.nvim",
    opts = {
      ignore_done_already = true,
      ignore_empty_message = true
    },
  },
  {
    'sainnhe/sonokai',
    lazy = false,
    priority = 1000,
    config = function()
      vim.g.sonokai_enable_italic = true
      vim.g.sonokai_style = 'shusia'
      vim.cmd.colorscheme('sonokai')
    end
  },
  {
    'nvim-treesitter/nvim-treesitter',
    build = ":TSUpdate",
    config = function ()
      local configs = require("nvim-treesitter.configs")

      configs.setup({
        ensure_installed = {
          "lua",
          "vim",
          "vimdoc",
          "bash",
          "scala",
          "javascript",
          "html",
          "properties",
          "sql",
          "python",
          "hocon",
          "yaml"
        },
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
    'TamaMcGlinn/quickfixdd',
  },

  {
    'rose-pine/neovim',
    name = 'rose-pine'
  },

  {
    "jackMort/ChatGPT.nvim",
    config = function()
      local secret_tool_exists = os.execute("type secret-tool >/dev/null 2>&1")
      local have_key = os.execute("type secret-tool lookup openai api-key >/dev/null 2>&1")

      if secret_tool_exists == 0 and have_key == 0 then
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
      end
    end,
    dependencies = {
      "MunifTanjim/nui.nvim",
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "folke/trouble.nvim"
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
  },
  {
    'kevinhwang91/nvim-ufo',
    dependencies = {
      'kevinhwang91/promise-async'
    },
    config = folding.config
  },
  {
    'gen740/SmoothCursor.nvim',
    config = function()
      require('smoothcursor').setup({})
    end
  },
  {
    'vala-lang/vala.vim',
  },
  {
    'MeanderingProgrammer/render-markdown.nvim',
    dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.nvim' },
    ft = { "markdown", "codecompanion" },
    config = function ()
      require('render-markdown').setup({
        code = {
          border = 'thick',
        },
      })
    end
  },
  {
    "echasnovski/mini.diff",
    config = function()
      local diff = require("mini.diff")
      diff.setup({
        -- Disabled by default
        source = diff.gen_source.none(),
      })
    end,
  },
}

if vim.loop.os_uname().sysname == "Darwin" then
  table.insert(to_install, {
    'github/copilot.vim',
    config = function()
      -- stop copilot completion. Ain't no vibe strong enough for me to accept this yet.
      vim.g.copilot_filetypes = {
        ['*'] = false
      }
    end
  })
  table.insert(to_install, {
    "CopilotC-Nvim/CopilotChat.nvim",
    dependencies = {
      { "github/copilot.vim" },
      { "nvim-lua/plenary.nvim" },
    },
    build = "make tiktoken",
    opts = {
    },
  })
  table.insert(to_install, {
    "olimorris/codecompanion.nvim",
    opts = {},
    dependencies = {
      "github/copilot.vim",
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
      "ravitemer/mcphub.nvim"
    },
    config = function()
      require("codecompanion").setup()
      vim.keymap.set({ "n", "v" }, "<leader>cc", "<cmd>CodeCompanionChat Toggle<cr>", { noremap = true, silent = true })
    end
  })
end

----------------------------------
-- PLUGINS -----------------------
----------------------------------
--
require("lazy").setup(to_install)

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
-- highlight column at max width
vim.cmd([[ set colorcolumn=+1 ]])
vim.cmd([[ set spell ]])

-- disable netrw at the very start of your init.lua
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- set termguicolors to enable highlight groups
-- vim.opt.termguicolors = true
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

