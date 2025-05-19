return {
  {
    "nvim-neorg/neorg",
    lazy = false, -- Disable lazy loading as some lazy.nvim distributions set lazy = true by default
    version = "*", -- Pin Neorg to the latest stable release
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("neorg").setup {
        load = {
          ["core.defaults"] = {}, -- Loads default behavior
          ["core.concealer"] = {}, -- Adds icons and better UI
          ["core.dirman"] = { -- Manages Neorg workspaces
            config = {
              workspaces = {
                notes = "~/neorg", -- Adjust this to your preferred directory
              },
              default_workspace = "notes",
            },
          },
          ["core.qol.todo_items"] = {}, -- Task management (ToDos)
          ["core.integrations.nvim-cmp"] = {}, -- Optional: Completion support
        }
      }
    end,
  },
  {
  "dhruvasagar/vim-table-mode",
  ft = { "markdown", "norg", "org", "text" }, -- Optional: load only for relevant filetypes
  keys = {
    { "<leader>tm", "<cmd>TableModeToggle<CR>", desc = "Toggle Table Mode" }
  },
}
}
