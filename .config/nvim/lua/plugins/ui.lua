return {
  {
    "nvim-lualine/lualine.nvim",
    config = function()
      local function clock()
        return os.date("%I:%M %p") -- 12-hour format with AM/PM
      end

      require("lualine").setup {
        options = {
          theme = 'dracula',
        },
        sections = {
          lualine_a = { 'mode' },
          lualine_b = { 'branch', 'diff', 'diagnostics' },
          lualine_c = { 'filename' },
          lualine_x = { 'encoding', 'fileformat', 'filetype' },
          lualine_y = { clock },
          lualine_z = { 'location' },
        },
      }
    end,
  },
  {
    "akinsho/nvim-bufferline.lua",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = true,
  },
  {
    "folke/noice.nvim",
    event = "VeryLazy",
    opts = {
      lsp = {
        override = {
          ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
          ["vim.lsp.util.stylize_markdown"] = true,
          ["cmp.entry.get_documentation"] = true,
        },
      },
      presets = {
        bottom_search = true,
        command_palette = true,
        long_message_to_split = true,
        inc_rename = false,
        lsp_doc_border = false,
      },
    },
    dependencies = {
      "MunifTanjim/nui.nvim",
      "rcarriga/nvim-notify",
    },
    config = function(_, opts)
      require("noice").setup(opts)
      vim.o.cmdheight = 1
    end,
  },
  {
    "norcalli/nvim-colorizer.lua",
    config = function()
      require("colorizer").setup({
        '*',
        css = { rgb_fn = true },
        html = { names = false },
      })
    end,
  },
  {
    "max397574/colortils.nvim",
    cmd = "Colortils",
    config = function()
      require("colortils").setup({
        register = "+",
        color_preview = "█ %s",
        default_format = "hex",
        default_color = "#000000",
        border = "rounded",
        mappings = {
          increment = "l",
          decrement = "h",
          increment_big = "L",
          decrement_big = "H",
          min_value = "0",
          max_value = "$",
          set_register_default_format = "<cr>",
          set_register_choose_format = "g<cr>",
          replace_default_format = "<m-cr>",
          replace_choose_format = "g<m-cr>",
          export = "E",
          set_value = "c",
          transparency = "T",
          choose_background = "B",
        },
      })
    end,
  },
  {
    "kevinhwang91/nvim-ufo",
    dependencies = { "kevinhwang91/promise-async" },
    config = function()
      -- Basic setup for ufo.nvim
      require("ufo").setup({
        provider_selector = function(bufnr, filetype, buftype)
          return { "treesitter", "indent" }
        end,
      })

      -- Keybindings for folding
      vim.keymap.set("n", "zR", require("ufo").openAllFolds) -- Open all folds
      vim.keymap.set("n", "zM", require("ufo").closeAllFolds) -- Close all folds
      vim.keymap.set("n", "zr", require("ufo").openFoldsExceptKinds) -- Open folds except specific kinds
      vim.keymap.set("n", "zm", require("ufo").closeFoldsWith) -- Close folds based on a condition

      -- Customize fold text
      local ft = require("ufo").foldText
      vim.opt.foldtext = ft
      vim.opt.fillchars:append({
        fold = " ",
        foldopen = "",
        foldsep = " ",
        foldclose = "",
      })

      -- Ensure these are enabled for ufo.nvim
      vim.o.foldcolumn = "1" -- Show fold column
      vim.o.foldlevel = 99   -- Ensure all folds are open by default
      vim.o.foldlevelstart = 99
      vim.o.foldenable = true
    end,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    ---@module "ibl"
    ---@type ibl.config
    opts={},
},
}
