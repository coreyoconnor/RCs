local F = require('local.functions')
local map = F.map
local shiftk = F.shiftk
local api = vim.api

local lsp_group = api.nvim_create_augroup("lsp", { clear = true })

local function is_executable(executable)
  local handle = io.popen("command -v " .. executable)
  if handle == nil then
    return false
  end

  local result = handle:read("*a")
  handle:close()

  return result ~= ""
end

local attach_func = function(client, bufnr)
  local telescope_builtin = require("telescope.builtin")
  -- LSP mappings
  map("n", "gd", telescope_builtin.lsp_definitions, { desc = "LSP go to definition of the symbol under the cursor." })
  map("n", "K", shiftk,
    { desc = "First press, perform lsp hover action if available; second press enter the hover window; third press exit the hover window." })
  map("n", "gi", telescope_builtin.lsp_implementations,
    { desc = "LSP go to implementation of the symbol under the cursor." })
  -- map("n", "gr", vim.lsp.buf.references, { desc = "LSP go to references of the sumbol under the cursor." })
  map("n", "gr", telescope_builtin.lsp_references, { desc = "LSP go to references of the sumbol under the cursor." })
  -- map("n", "gds", vim.lsp.buf.document_symbol, { desc = "LSP list symbols in the current document." })
  map("n", "gds", telescope_builtin.lsp_document_symbols, { desc = "LSP list symbols in the current document." })
  -- map("n", "gws", vim.lsp.buf.workspace_symbol, { desc = "LSP search for symbol in workspace by name." })
  map("n", "gws", telescope_builtin.lsp_dynamic_workspace_symbols,
    { desc = "LSP search for symbol in workspace by name." })
  map("n", "<leader>cl", vim.lsp.codelens.run, { desc = "LSP code lens." })
  map({ "n", "v" }, "<leader>ca", function()
    -- local curr_row = vim.fn.line('.')
    -- local max_col = vim.fn.col('$') - 1
    -- vim.lsp.buf.code_action({ ["range"] = { ["start"] = { curr_row, 0 }, ["end"] = { curr_row, max_col } } })
    vim.lsp.buf.code_action()
  end, { desc = "LSP code action." })
  map("n", "<leader>sh", vim.lsp.buf.signature_help, { desc = "LSP function signature help." })
  map("i", "<C-p>", vim.lsp.buf.signature_help, { desc = "LSP function signature help." })
  map("n", "<leader>rn", vim.lsp.buf.rename, { desc = "LSP rename the symbol under the cursor." })
  -- map("n", "<leader>f", function() vim.lsp.buf.format { async = true } end, { desc = "LSP format open file." })
  -- map("n", "<leader>aa", vim.diagnostic.setqflist, { desc = "Open all LSP diagnostics in quickfix list." })
  map("n", "<leader>aa", telescope_builtin.diagnostics, { desc = "LSP all workspace diagnostics." })
  -- map("n", "<leader>ae", function() vim.diagnostic.setqflist({ severity = "E" }) end,
  --   { desc = "Open 'Error' diagnostics in quickfix list." }) -- all workspace errors
  map("n", "<leader>ae", function() telescope_builtin.diagnostics({ severity = "Error" }) end,
    { desc = "LSP 'Error' diagnostics." })
  -- map("n", "<leader>aw", function() vim.diagnostic.setqflist({ severity = "W" }) end,
  --   { desc = "Open 'Warning' diagnostics in quickfix list." }) -- all workspace warnings
  map("n", "<leader>aw", function() telescope_builtin.diagnostics({ severity = "Warning" }) end,
    { desc = "LSP 'Warning' diagnostics." })
  -- map("n", "<leader>d", vim.diagnostic.setloclist,
  --   { desc = "Open all LSP diagnostics for current buffer only in local list." }) -- buffer diagnostics only
  map("n", "<leader>ab", function() telescope_builtin.diagnostics({ bufnr = 0 }) end,
    { desc = "LSP diagnostics for current buffer." })
  map("n", "[d", function() vim.diagnostic.goto_prev({ float = { border = 'double' } }) end,
    { desc = "Previous diagnostic" })
  map("n", "]d", function() vim.diagnostic.goto_next({ float = { border = 'double' } }) end,
    { desc = "Next diagnostic" })

  -- Example mappings for usage with nvim-dap. If you don't use that, you can skip these
  map("n", "<leader>dc", require("dap").continue, { desc = "DAP continue execution." })
  map("n", "<leader>dr", require("dap").repl.toggle, { desc = "DAP toggle the repl." })
  -- map("n", "<leader>dr", require("dapui").toggle, { desc = "DAP toggle the repl." })
  map("n", "<leader>dK", require("dap.ui.widgets").hover, { desc = "DAP-specific hover function." })
  map("n", "<leader>dt", require("dap").toggle_breakpoint, { desc = "DAP toggle breakpoint." })
  map("n", "<leader>dso", require("dap").step_over, { desc = "DAP step over." })
  map("n", "<leader>dsi", require("dap").step_into, { desc = "DAP step into." })
  map("n", "<leader>dl", require("dap").run_last, { desc = "DAP re-run the previously executed command." })

  api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

  -- add border to hover box
  local hover_config = {
    border = 'double',
  }

  vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, hover_config)
  vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, hover_config)

  if client.server_capabilities.documentHighlightProvider then
    api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
      buffer = bufnr,
      callback = vim.lsp.buf.document_highlight,
      group = lsp_group,
    })

    api.nvim_create_autocmd("CursorMoved", {
      buffer = bufnr,
      callback = vim.lsp.buf.clear_references,
      group = lsp_group,
    })
  end

  if client.server_capabilities.codeLensProvider then
    api.nvim_create_autocmd({ "BufEnter", "BufWritePost" }, {
      buffer = bufnr,
      callback = vim.lsp.codelens.refresh,
      group = lsp_group
    })
  end

  api.nvim_create_autocmd("FileType", {
    pattern = { "dap-repl" },
    callback = function()
      require("dap.ext.autocompl").attach()
    end,
    group = lsp_group,
  })

  api.nvim_buf_create_user_command(bufnr, "Format", function() vim.lsp.buf.format() end,
    { desc = "Format current buffer with LSP" })
end

local config = function()
  -- LSP logging to debug
  vim.lsp.set_log_level("debug")
  -- vim.lsp.set_log_level("trace")

  -- some diagnostic settings
  vim.diagnostic.config({
    severity_sort = true,
    virtual_text = false,
  })

  local lspconfig = require("lspconfig")
  --
  -- lua lsp runtime path adjustments
  local runtime_path = vim.split(package.path, ";")
  table.insert(runtime_path, "lua/?.lua")
  table.insert(runtime_path, "lua/?/init.lua")

  -- extra Neovim Lua stuff
  require("neodev").setup()

  -- lua lsp
  lspconfig.lua_ls.setup({
    on_attach = attach_func,
    settings = {
      Lua = {
        runtime = {
          version = "LuaJIT",
          path = runtime_path,
        },
        diagnostics = {
          globals = { "vim", "require" }
        },
        workspace = {
          library = api.nvim_get_runtime_file("", true),
          checkThirdParty = false,
        },
        completion = {
          callSnippet = "Replace"
        },
        telemetry = { enable = false },
      },
    },
  })

  -- python lsp
  lspconfig.pylsp.setup {
    on_attach = attach_func,
    settings = {
      pylsp = {
        configurationSources = { "pylint", "pydocstyle" },
        plugins = {
          autopep8 = {
            enabled = false
          },
          pydocstyle = {
            enabled = true
          },
          pylint = {
            enabled = true
          },
        }
      }
    }
  }

  -- bash lsp
  lspconfig.bashls.setup({
    on_attach = attach_func,
    filetypes = { "bash", "sh", "zsh" }
  })

  -- html lsp
  lspconfig.html.setup({
    on_attach = attach_func,
    filetypes = { "html", "htmldjango" }
  })

  -- rust-analyzer
  lspconfig.rust_analyzer.setup({
    on_attach = attach_func,
  })

  -- yaml lsp
  -- lspconfig.yamlls.setup({
  --   on_attach = attach_func,
  --   filetypes = { "yaml", "yaml.docker-compose", "yml" }
  -- })

  -- sql lsp
  lspconfig.sqlls.setup({
    on_attach = attach_func,
  })

  -- vala lsp
  --if is_executable("vala-lanuage-server") then
  lspconfig.vala_ls.setup({})
  --end
end


return {
  group = lsp_group,
  attach_func = attach_func,
  config = config
}
