-- ── nvim-lint ─────────────────────────────────────────────────────────
local lint = require("lint")

lint.linters_by_ft = {}

vim.api.nvim_create_autocmd({ "BufWritePost", "InsertLeave" }, {
    callback = function()
    lint.try_lint()
    end,
})
