local F = require('local.functions')
local lspconfig = require('local.lspconfig')
local map = F.map
local shiftk = F.shiftk
local api = vim.api

local nvim_metals_group = api.nvim_create_augroup("nvim-metals", { clear = true })

local ft = { "scala", "sbt", "java" }

local opts = function()
  local metals_config = require("metals").bare_config()
  metals_config.init_options.statusBarProvider = false
  metals_config.settings = {
    showImplicitArguments = true,
    defaultBspToBuildTool = true,
    -- serverVersion = "latest.snapshot"
  }
  metals_config.tvp["icons"] = { enabled = true }

  local capabilities = require("cmp_nvim_lsp").default_capabilities()
  capabilities.textDocument.foldingRange = {
    dynamicRegistration = false,
    lineFoldingOnly = true
  }
  metals_config.capabilities = capabilities

  metals_config.on_attach = function(client, bufnr)

    lspconfig.attach_func(client, bufnr)

    -- mappings specific to Metals
    map("n", "<leader>t", require("metals.tvp").toggle_tree_view, { desc = "Open Metals tree view." })
    -- map("n", "T", require("metals.tvp").toggle_tree_view, { desc = "Open Metals tree view." })
    map("n", "<leader>tr", require("metals.tvp").reveal_in_tree,
      { desc = "Open Metals tree view at currently highlighted symbol." })
    map("n", "<leader>ws", require "metals".hover_worksheet, { desc = "Metals hover worksheet." })
    map("n", "<leader>mc", require("telescope").extensions.metals.commands,
      { desc = "Metals commands Telescope picker." })

    require("metals").setup_dap()

    client.server_capabilities.semanticTokensProvider = nil

  end

  metals_config.handlers = {
    ["metals/status"] = function()
    end
  }

  return metals_config
end

local config = function(self, metals_config)
  -- Debug settings if you're using nvim-dap
  local dap = require("dap")
  -- local dapui = require("dapui")
  dap.configurations.scala = {
    {
      type = "scala",
      request = "launch",
      name = "Run Or Test Current File",
      metals = {
        runType = "runOrTestFile",
      },
    },
    {
      type = "scala",
      request = "launch",
      name = "Test Entire Build Target",
      metals = {
        runType = "testTarget",
      },
    },
  }
  -- auto open/close dapui with dap
  dap.listeners.after.event_initialized["nvim-metals"] = function() dap.repl.open() end
  -- dap.listeners.after.event_initialized["dapui_config"] = function() dapui.open() end
  -- dap.listeners.before.event_terminated["dapui_config"] = function() dapui.close() end
  -- dap.listeners.before.event_exited["dapui_config"] = function() dapui.close() end


  -- Autocmd that starts up Metals
  api.nvim_create_autocmd("FileType", {
    pattern = self.ft,
    callback = function()
      require("metals").initialize_or_attach(metals_config)
    end,
    group = nvim_metals_group,
  })

  -- propagate all filetype failure messages
  vim.opt_global.shortmess:remove("F")
end

return {
  ft = ft,
  opts = opts,
  config = config,
  group = nvim_metals_group
}
