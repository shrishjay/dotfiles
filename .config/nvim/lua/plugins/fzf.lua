return {
  {
    "ibhagwan/fzf-lua",
    dependencies = { "nvim-tree/nvim-web-devicons" }, -- optional but nice
    opts = {
      winopts = {
        height = 0.85,
        width = 0.80,
        row = 0.35,
        col = 0.50,
        border = "rounded",
      },
      files = {
        cmd = "fd --type f --hidden --exclude .git", -- use fd, show hidden files
        previewer = false,                           -- set to true if you want previews
      },
      grep = {
        rg_opts = "--column --line-number --no-heading --color=always --smart-case --hidden",
      },
    },
    keys = {
      {
        "<leader>fz",
        function()
          require("fzf-lua").files()
        end,
        desc = "Find Files (fzf-lua, with hidden)",
      },
    },
  },
}
