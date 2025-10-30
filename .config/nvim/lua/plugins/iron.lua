return {
  {
    "Vigemus/iron.nvim",
    config = function()
      local iron = require("iron.core")
      local view = require("iron.view")

      iron.setup({
        config = {
          scratch_repl = true,
          repl_definition = {
            python = {
              -- Always start IPython in the current buffer's directory
              command = function()
                local cwd = vim.fn.expand("%:p:h")
                return {
                  "bash",
                  "-c",
                  string.format("cd '%s' && ipython --no-autoindent -i", cwd),
                }
              end,
              format = require("iron.fts.common").bracketed_paste,
            },
          },
          repl_open_cmd = view.right(60),
        },
        keymaps = {
          send_motion = "<leader>sc",
          visual_send = "<leader>sc",
          send_line = "<leader>sl",
          send_paragraph = "<leader>sp",
          send_until_cursor = "<leader>su",
          cr = "<leader>s<cr>",
          interrupt = "<leader>s<space>",
          exit = "<leader>sq",
          clear = "<leader>cl",
        },
        highlight = { italic = true },
      })

      -- 🔹 Optional: automatically print where REPL is starting
      vim.api.nvim_create_user_command("IronReplCwd", function()
        print("Iron REPL will start in: " .. vim.fn.expand("%:p:h"))
      end, {})
    end,

    keys = {
      {
        -- Start Iron REPL in the current file's directory
        "<leader>rs",
        function()
          local cwd = vim.fn.expand("%:p:h") -- buffer's folder
          local prev_cwd = vim.fn.getcwd() -- save old cwd
          vim.cmd("lcd " .. cwd) -- set local cwd for window
          vim.cmd("IronRepl") -- start REPL
          vim.cmd("lcd " .. prev_cwd) -- restore previous cwd
        end,
        desc = "Start Iron REPL in file's directory",
      },
      { "<leader>rr", "<cmd>IronRestart<cr>", desc = "Restart Iron REPL" },
      { "<leader>rf", "<cmd>IronFocus<cr>", desc = "Focus Iron REPL" },
      { "<leader>rh", "<cmd>IronHide<cr>", desc = "Hide Iron REPL" },
    },
  },
}
