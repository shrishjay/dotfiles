-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

vim.keymap.set("n", "<leader>bn", ":bnext<CR>", { noremap = true, silent = true })

-- Switch to previous buffer
vim.keymap.set("n", "<leader>bp", ":bprev<CR>", { noremap = true, silent = true })
-- Close the current buffer
vim.keymap.set("n", "<leader>bd", ":bd<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>bb", ":Telescope buffers<CR>", { noremap = true, silent = true })
-- Open terminal in current file's directory
vim.keymap.set("n", "<leader>tt", function()
  local dir = vim.fn.expand("%:p:h")
  vim.cmd("lcd " .. dir)
  vim.cmd.vnew()
  vim.cmd.term()
  vim.cmd.wincmd("J")
  vim.api.nvim_win_set_height(0, 12)
end)
