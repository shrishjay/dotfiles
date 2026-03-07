return {
{
  'nvim-treesitter/nvim-treesitter',
  event = { "BufReadPost", "BufNewFile" },
  build = ':TSUpdate',
  opts = {
  -- LazyVim config for treesitter
  indent = { enable = true }, ---@type lazyvim.TSFeat
  highlight = { enable = true }, ---@type lazyvim.TSFeat
  folds = { enable = true },
}
},
  {
  "nvim-treesitter/nvim-treesitter-textobjects",
  event = { "BufReadPost", "BufNewFile" },
  branch = "main",
  init = function()
    -- Disable entire built-in ftplugin mappings to avoid conflicts.
    -- See https://github.com/neovim/neovim/tree/master/runtime/ftplugin for built-in ftplugins.
    vim.g.no_plugin_maps = true

    -- Or, disable per filetype (add as you like)
    -- vim.g.no_python_maps = true
    -- vim.g.no_ruby_maps = true
    -- vim.g.no_rust_maps = true
    -- vim.g.no_go_maps = true
  end,
  config = function()
    -- put your config here
  end,
},
{
  "windwp/nvim-ts-autotag",
  event = { "BufReadPost", "BufNewFile" },
}
}
