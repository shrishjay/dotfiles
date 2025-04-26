return {
  -- Mason for managing LSP servers

  {
    "williamboman/mason.nvim",
    lazy = false,
    config = function()
      require("mason").setup()
    end,
  },
  -- Mason-LSPConfig to bridge Mason with LSPConfig
  {
    "williamboman/mason-lspconfig.nvim",
    lazy = false,
    opts = {
      auto_install = true,
    },
  },
  -- nvim-lspconfig to configure the LSP servers
  {
    "neovim/nvim-lspconfig",
    lazy = false,
    config = function()
      -- Setup for nvim-cmp capabilities
      local capabilities = require("cmp_nvim_lsp").default_capabilities()

      local lspconfig = require("lspconfig")
      -- LSP server configurations
      lspconfig.ts_ls.setup({
  capabilities = capabilities
})

      lspconfig.pyright.setup({
        capabilities = capabilities
      })
      lspconfig.solargraph.setup({
        capabilities = capabilities
      })

      -- Associate .html files with the 'htmldjango' filetype
      vim.filetype.add({
        pattern = { [".*%.html%"] = "htmldjango" },
      })

      -- Keybindings for LSP actions
      vim.keymap.set("n", "K", vim.lsp.buf.hover, {})
      vim.keymap.set("n", "<leader>gd", vim.lsp.buf.definition, {})
      vim.keymap.set("n", "<leader>gr", vim.lsp.buf.references, {})
      vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, {})
    end,
  },
  -- Snippets engine and configuration
  {
    "L3MON4D3/LuaSnip",
    dependencies = { "rafamadriz/friendly-snippets" },
    config = function()
      local luasnip = require("luasnip")

      -- Load all friendly-snippets
      require("luasnip.loaders.from_vscode").lazy_load()

      -- Extend htmldjango to use html snippets
      luasnip.filetype_extend("htmldjango", { "html" })

      -- Add Quarto-specific snippets
      luasnip.add_snippets("quarto", {
        luasnip.snippet("p", {
          luasnip.text_node("```{python}"),
          luasnip.text_node({ "", "" }), -- Empty line for code
          luasnip.text_node({ "", "" }), -- Empty line for code
          luasnip.text_node("```"),
        }),
      })
    end,
  },
  -- nvim-cmp for autocompletion
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "L3MON4D3/LuaSnip",         -- Snippet engine
      "saadparwaiz1/cmp_luasnip", -- LuaSnip integration with cmp
      "hrsh7th/cmp-nvim-lsp",     -- LSP source
      "hrsh7th/cmp-buffer",       -- Buffer source
      "hrsh7th/cmp-path",         -- Path source
    },
    config = function()
      local cmp = require("cmp")
      local luasnip = require("luasnip")

      cmp.setup({
        snippet = {
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end,
        },
        window = {
          completion = cmp.config.window.bordered(),
          documentation = cmp.config.window.bordered(),
        },
        mapping = cmp.mapping.preset.insert({
          ["<Tab>"] = cmp.mapping.select_next_item(),
          ["<S-Tab>"] = cmp.mapping.select_prev_item(),
          ["<C-CR>"] = cmp.mapping.confirm({ select = true }), -- Use Ctrl+Space to confirm
          -- ["<C-e>"] = cmp.mapping.abort(),                        -- Cancel autocompletion
        }),
        sources = {
          { name = "otter" },
          { name = "nvim_lsp" },
          { name = "luasnip" },
          { name = "buffer" },
          { name = "path" },
        },
      })
    end,
  },
}

