return {
  {
    "akinsho/toggleterm.nvim",
    opts = {
      -- direction = "float",
    },
  },
  vim.keymap.set("n", "<space>tt", ":ToggleTerm dir=%:p:h<CR>"), -- open terminal in the directory of the file
}
