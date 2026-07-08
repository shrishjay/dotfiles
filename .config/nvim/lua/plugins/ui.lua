require("lualine").setup({
    options = {
        theme = "auto",
    },
})
require("oil").setup()
-- ── nvim-bufferline.lua ───────────────────────────────────────────────
require("bufferline").setup()
local wilder = require("wilder")
wilder.setup({ modes = { ":", "/", "?" } })
wilder.set_option("renderer", wilder.popupmenu_renderer({
    border     = "rounded",
    max_height = 10,
}))
wilder.set_option("pipeline", {
    wilder.branch(
        wilder.cmdline_pipeline({
            fuzzy = 1,
        })
    ),
})
require("mini.surround").setup()
require("mini.pairs").setup()
require("mini.files").setup()
vim.keymap.set("n", "<leader>e", function()
  local mf = require("mini.files")
  if not mf.close() then
    mf.open(vim.api.nvim_buf_get_name(0))
  end
end, { desc = "Toggle Explorer" })
-- ── mini.clue ─────────────────────────────────────────────────────────
local miniclue = require("mini.clue")

miniclue.setup({
  triggers = {
    { mode = "n", keys = "<leader>" },
    { mode = "x", keys = "<leader>" },
    { mode = "n", keys = "g" },
    { mode = "n", keys = "z" },
    { mode = "n", keys = "'" },
    { mode = "n", keys = "`" },
    { mode = "n", keys = '"' },
    { mode = "i", keys = "<C-x>" },
    { mode = "i", keys = "<C-r>" },
    { mode = "c", keys = "<C-r>" },
  },
  clues = {
    miniclue.gen_clues.builtin_completion(),
    miniclue.gen_clues.g(),
    miniclue.gen_clues.marks(),
    miniclue.gen_clues.registers(),
    miniclue.gen_clues.z(),
  },
  window = {
    delay = 100,
    config = { border = "rounded" },
  },
})
