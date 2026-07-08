

local map = vim.keymap.set
local opts = { noremap = true, silent = true }


-- Insert mode escape
map("i", "jk", "<Esc>", opts)

-- Clipboard
map("v", "<leader>y", '"+y', opts)
map("v", "<leader>p", '"+p', opts)

-- Buffers
map("n", "<leader>bn", "<cmd>bnext<CR>", opts)
map("n", "<leader>bp", "<cmd>bprev<CR>", opts)
map("n", "<leader>bd", "<cmd>bd<CR>", opts)
map("n", "<leader>bb", function()
  require("mini.pick").builtin.buffers()
end, { desc = "Buffers" })

-- Find
map("n", "<leader>ff", function()
  require("mini.pick").builtin.files()
end, { desc = "Find Files" })

map("n", "<leader>fr", function()
  require("mini.extra").pickers.oldfiles()
end, { desc = "Recent Files" })

map("n", "<leader>fg", function()
  require("mini.pick").builtin.grep_live()
end, { desc = "Live Grep" })
