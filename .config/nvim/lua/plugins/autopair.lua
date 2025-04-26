return {
  {
    "windwp/nvim-autopairs",
    config = function()
      require("nvim-autopairs").setup ({
      check_ts = true,  -- enable tree-sitter for better pair matching
      map_cr = true,    -- map the <CR> key to automatically close pairs
      fast_wrap = {}    -- You can set keybindings for fast wrapping (optional)
      })
    end,
  },
}
