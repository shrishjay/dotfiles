return {
  { "akinsho/toggleterm.nvim", opts = {
    direction = "horizontal",
  } },
  vim.keymap.set("n", "<space>td", ":ToggleTerm dir=%:p:h<CR>"), -- open terminal in the directory of the file
}
