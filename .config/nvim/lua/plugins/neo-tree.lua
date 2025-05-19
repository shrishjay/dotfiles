return {
	"nvim-neo-tree/neo-tree.nvim",
	branch = "v3.x",
	dependencies = {
		"nvim-lua/plenary.nvim",
		"nvim-tree/nvim-web-devicons",
		"MunifTanjim/nui.nvim",
	},
  config = function()
    require("neo-tree").setup({
      filesystem = {
        follow_current_file = {
          enabled = true,
          leave_dirs_open = true,
        },
        autochdir = true,
        hijack_netrw_behavior = "open_current",
        filtered_items = {
          visible = true, -- Show hidden files by default
        },
      },
    })

    -- Optional: keymap to reveal the current file in Neo-tree
    vim.keymap.set("n", "<leader>e", ":Neotree reveal<CR>", { desc = "Neo-tree: Reveal file" })
  end,
}
