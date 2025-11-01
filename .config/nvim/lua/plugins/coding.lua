return
{
  {
    "saghen/blink.cmp",
    opts = {
      completion = {
        menu = {
          border = "rounded",
        },
        documentation = {
          window = {
            border = "rounded",
          },
        },
      },
      keymap = {
        preset = "default",
        ["<Tab>"] = { "select_next", "snippet_forward", "fallback" },
        ["<S-Tab>"] = { "select_prev", "snippet_backward", "fallback" },
        ["<CR>"] = { "accept", "fallback" },
      },
    },
  },
  {
    "neovim/nvim-lspconfig",
    opts = {
      diagnostics = {
        virtual_text = false,
      },
    },
  },
  {
    "folke/noice.nvim",
    opts = function(_, opts)
      opts.lsp.signature = {
        auto_open = {
          enabled = true,
          trigger = true,
          luasnip = true,
        },
        opts = {
          border = "rounded",
          relative = "cursor",
          anchor = "SW",
          row = -1,
          size = {
            width = 120, -- wider horizontally
            height = 5,  -- small vertically
          },
        },
      }
    end,
  }

}
