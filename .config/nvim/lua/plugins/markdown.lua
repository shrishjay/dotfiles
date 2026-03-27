return {
  {
  "iamcco/markdown-preview.nvim",
  cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
  build = "cd app && yarn install",
  init = function()
    vim.g.mkdp_filetypes = { "markdown" }
  end,
  ft = { "markdown" },
},
{
  'MeanderingProgrammer/render-markdown.nvim',
ft="markdown",
  dependencies = { 'nvim-treesitter/nvim-treesitter' },            -- if you use the mini.nvim suite
  opts = {},
 },
}
