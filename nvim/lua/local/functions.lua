local api = vim.api

-- shortcut for key remaps
local function map(mode, lhs, rhs, opts)
  local options = { noremap = true }
  if opts then
    options = vim.tbl_extend("force", options, opts)
  end
  vim.keymap.set(mode, lhs, rhs, options)
end

-- wrap vim.cmd calls in another function to enable usage in keybinds
local function cmd_map(command)
  return function()
    vim.cmd(command)
  end
end

-- Expand the functionality of shift-k to do the LSP hover action
-- or close the hover window if we are inside the hover
local function shiftk()
  local win = api.nvim_get_current_win()
  local win_config = api.nvim_win_get_config(win)
  if win_config.relative ~= "" then
    vim.cmd('close')
  else
    vim.lsp.buf.hover()
  end
end

local function toggleqf()
  local filetypes = {}
  for _, data in ipairs(vim.api.nvim_list_wins()) do
    table.insert(filetypes, vim.api.nvim_buf_get_option(vim.api.nvim_win_get_buf(data), "filetype"))
  end
  local function has_value(table, comp)
    for _, value in ipairs(table) do
      if value == comp then
        return true
      end
    end
    return false
  end

  if has_value(filetypes, "qf") then
    vim.cmd("cclose")
  else
    vim.cmd("copen")
  end
end

-- Use tab in completion menus
local function check_back_space()
  local col = vim.fn.col('.') - 1
  return col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') ~= nil
end

--- Use (s-)tab to:
--- move to prev/next item in completion menu
--- jump to prev/next snippet's placeholder
local function tab_complete()
  if vim.fn.pumvisible() == 1 then
    return "<C-n>"
    -- elseif vim.fn['vsnip#available'](1) == 1 then
    --   return t "<Plug>(vsnip-expand-or-jump)"
  elseif check_back_space() then
    return "<Tab>"
  else
    return "<C-x><C-o>"
  end
end
--- Use (s-)tab to:
--- move to prev/next item in completion menu
--- jump to prev/next snippet's placeholder
local function s_tab_complete()
  if vim.fn.pumvisible() == 1 then
    return "<C-p>"
    -- elseif vim.fn['vsnip#jumpable'](-1) == 1 then
    --   return t "<Plug>(vsnip-jump-prev)"
  else
    -- If <S-Tab> is not working in your terminal, change it to <C-h>
    return "<S-Tab>"
  end
end

-- Enter selects completion
local function carriage_return()
  if vim.fn.pumvisible() == 1 then
    return "<C-y>"
  else
    return "<Plug>delimitMateCR"
  end
end

return {
  carriage_return = carriage_return,
  cmd_map = cmd_map,
  map = map,
  shiftk = shiftk,
  s_tab_complete = s_tab_complete,
  tab_complete = tab_complete,
  toggleqf = toggleqf
}
