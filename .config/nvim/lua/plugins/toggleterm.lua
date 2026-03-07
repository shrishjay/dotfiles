return {
  {
    "akinsho/toggleterm.nvim",
    cmd = { "ToggleTerm", "TermExec" },
    version = "*",
    config = function()
      require("toggleterm").setup({
        shell = vim.fn.exepath("fish"),
      })
    end,
  },
  vim.keymap.set("n", "<space>tt", ":ToggleTerm dir=%:p:h<CR>"),
}
