return {
  {
    "tpope/vim-fugitive",
  },
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim", -- required
      "sindrets/diffview.nvim", -- optional - Diff integration
      "nvim-telescope/telescope.nvim",
    },
    config = function()
      require("neogit").setup({
        -- Use the current buffer's git root, not home directory
        use_default_keymaps = true,
      })
    end,
    keys = {
      {
        "<leader>gg",
        function()
          -- Find git root from current file's directory
          local git_root = vim.fn.systemlist("git -C " .. vim.fn.expand("%:p:h") .. " rev-parse --show-toplevel")[1]
          if vim.v.shell_error == 0 then
            require("neogit").open({ cwd = git_root })
          else
            require("neogit").open({ cwd = vim.fn.expand("%:p:h") })
          end
        end,
        desc = "Open Neogit in project directory",
      },
    },
  },
}
