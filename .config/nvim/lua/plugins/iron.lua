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
          repl_open_cmd = require("iron.view").bottom(15),
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

      -- Optional: show REPL starting directory
      vim.api.nvim_create_user_command("IronReplCwd", function()
        print("Iron REPL will start in: " .. vim.fn.expand("%:p:h"))
      end, {})
    end,

    keys = {
      {
        "<leader>rs",
        function()
          local cwd = vim.fn.expand("%:p:h")
          local prev_cwd = vim.fn.getcwd()
          vim.cmd("lcd " .. cwd)
          vim.cmd("IronRepl")
          vim.cmd("lcd " .. prev_cwd)
        end,
        desc = "Start Iron REPL in file's directory",
      },
      { "<leader>rr", "<cmd>IronRestart<cr>", desc = "Restart Iron REPL" },
      { "<leader>rf", "<cmd>IronFocus<cr>", desc = "Focus Iron REPL" },
      { "<leader>rh", "<cmd>IronHide<cr>", desc = "Hide Iron REPL" },
    },
  },
  {
    "jpalardy/vim-slime",
    config = function()
      -- Use tmux as the target
      vim.g.slime_target = "tmux"

      -- Default tmux target (current pane on the right)
      vim.g.slime_default_config = {
        socket_name = "default",
        target_pane = "{right-of}",
      }

      -- Don't ask for confirmation every time
      vim.g.slime_dont_ask_default = 1

      -- Preserve trailing newlines for Python
      vim.g.slime_preserve_curpos = 0

      -- Keymaps
      vim.keymap.set("n", "<leader>ss", "<Plug>SlimeLineSend", { desc = "Send line to REPL" })
      vim.keymap.set("n", "<leader>sp", "<Plug>SlimeParagraphSend", { desc = "Send paragraph to REPL" })
      vim.keymap.set("x", "<leader>sr", "<Plug>SlimeRegionSend", { desc = "Send selection to REPL" })
      vim.keymap.set("n", "<leader>sc", "<Plug>SlimeConfig", { desc = "Configure slime" })
    end,
  },
}
