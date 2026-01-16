-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
--
-- Add any additional autocmds here
-- with `vim.api.nvim_create_autocmd`
--
-- Or remove existing autocmds by their group name (which is prefixed with `lazyvim_` for the defaults)
-- e.g. vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")
-- In autocmds.lua
vim.api.nvim_create_autocmd("CursorHold", {
  callback = function()
    vim.diagnostic.open_float(nil, { focus = false })
  end,
})
vim.api.nvim_create_autocmd({ "ColorScheme", "VimEnter", "BufEnter" }, {
  callback = function()
    vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
    vim.api.nvim_set_hl(0, "FloatBorder", { bg = "none" })
    vim.api.nvim_set_hl(0, "BlinkCmpMenu", { bg = "none" })
    vim.api.nvim_set_hl(0, "BlinkCmpMenuBorder", { bg = "none" })
    vim.api.nvim_set_hl(0, "BlinkCmpMenuSelection", { bg = "none", fg = "#89b4fa" })
    vim.api.nvim_set_hl(0, "BlinkCmpDoc", { bg = "none" })
    vim.api.nvim_set_hl(0, "BlinkCmpDocBorder", { bg = "none" })
  end,
})

vim.diagnostic.config({
  virtual_text = false,
  float = {
    border = "rounded",
    source = true,
  },
})
vim.api.nvim_create_autocmd("TermOpen", {
  group = vim.api.nvim_create_augroup("custom-term-open", { clear = true }),
  callback = function()
    vim.opt.number = false
    vim.opt.relativenumber = false
  end,
})
