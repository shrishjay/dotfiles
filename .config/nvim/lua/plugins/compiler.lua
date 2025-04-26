return {
  {
    "CRAG666/code_runner.nvim",
    config = function()
      require("code_runner").setup({
        -- Path where results are shown: "toggleterm", "terminal", "quickfix", "buffer"
        mode = "toggleterm",
        -- Set keybindings
        filetype = {
          python = "python3 -u",
          javascript = "node",
          java = "javac % && java %:r",
          cpp = "g++ % -o %:r && ./%:r",
          go = "go run",
          rust = "cargo run",
        },

        startinsert = true, -- Start terminal in insert mode
        -- configuration details here
      })
    end,
    dependencies = { "nvim-lua/plenary.nvim" },
  },

  vim.api.nvim_set_keymap("n", "<leader>rc", "<cmd>RunCode<CR>", { noremap = true, silent = true }),
  vim.api.nvim_set_keymap("n", "<leader>rf", "<cmd>RunFile<CR>", { noremap = true, silent = true }),
  vim.api.nvim_set_keymap("n", "<leader>rp", "<cmd>RunProject<CR>", { noremap = true, silent = true }),
  vim.api.nvim_set_keymap("n", "<leader>cx", "<cmd>RunClose<CR>", { noremap = true, silent = true }),
}
