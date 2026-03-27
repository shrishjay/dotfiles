return {
  {
    "mason-org/mason.nvim",
    event="VeryLazy",
    config = function()
      require("mason").setup({
      ensure_installed={'lua_ls','pyright'}})
    end,
  },
  {
    "mason-org/mason-lspconfig.nvim",
    event = "VeryLazy",
    opts = {
      auto_install = true,
    },
  },
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPost", "BufNewFile" },
    config = function()
      -- Diagnostics
      vim.diagnostic.config({
        underline = true,
        update_in_insert = false,
        severity_sort = true,
        float = {
          border = "rounded",
        },
        virtual_text = {
          spacing = 4,
          source = "if_many",
          prefix = "●",
        },
        signs = {
          text = {
            [vim.diagnostic.severity.ERROR] = "",
            [vim.diagnostic.severity.WARN]  = "",
            [vim.diagnostic.severity.HINT]  = "󰠠",
            [vim.diagnostic.severity.INFO]  = "",
          },
        },
      })
      vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
        vim.lsp.diagnostic.on_publish_diagnostics, {
          update_in_insert = false,
          debounce_text_changes = 150,
        }
      )
      -- Keymaps
      local opts = { noremap = true, silent = true }
      vim.keymap.set("n", "<leader>cl", function() Snacks.picker.lsp_config() end, opts)
      vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
      vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
      vim.keymap.set("n", "gI", vim.lsp.buf.implementation, opts)
      vim.keymap.set("n", "gy", vim.lsp.buf.type_definition, opts)
      vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)
      vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
      vim.keymap.set("n", "<leader>cr", vim.lsp.buf.rename, opts)
      vim.keymap.set({ "n", "x" }, "<leader>ca", vim.lsp.buf.code_action, opts)
      vim.keymap.set({ "n", "x" }, "<leader>cc", vim.lsp.codelens.run, opts)
      vim.keymap.set("n", "<leader>cR", function()
        Snacks.rename.rename_file()
      end, opts)
      vim.keymap.set("n", "]]", function()
        Snacks.words.jump(vim.v.count1)
      end, opts)
      vim.keymap.set("n", "[[", function()
        Snacks.words.jump(-vim.v.count1)
      end, opts)
      -- Inlay hints auto-enable
      vim.api.nvim_create_autocmd("LspAttach", {
        callback = function(args)
          local client = vim.lsp.get_client_by_id(args.data.client_id)
          if client and client.server_capabilities.inlayHintProvider then
            vim.lsp.inlay_hint.enable(true, { bufnr = args.buf })
          end
          vim.bo[args.buf].omnifunc = ""
          vim.bo[args.buf].completefunc = ""
        end,
      })
      vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    update_in_insert = false,
    debounce_text_changes = 10,
  }
)
    end,
  },
}
