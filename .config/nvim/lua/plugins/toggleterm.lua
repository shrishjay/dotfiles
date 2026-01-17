return {
  { "akinsho/toggleterm.nvim", version = "*", config = true },
  vim.keymap.set("n", "<space>tt", ":ToggleTerm dir=%:p:h<CR>"),
}
