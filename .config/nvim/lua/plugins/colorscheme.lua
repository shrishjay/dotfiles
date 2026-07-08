-- ── catppuccin ────────────────────────────────────────────────────────
require("catppuccin").setup({
    transparent_background = true,
    styles = {
        sidebars = "transparent",
        floats   = "transparent",
    },
})

vim.cmd.colorscheme("catppuccin")
