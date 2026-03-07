-- Keybindings in Lua

local map = vim.keymap.set
local opts = { noremap = true, silent = true }

-- Explorer
map("n", "<leader>e", function()
  Snacks.explorer()
end, { desc = "Toggle Explorer" })

-- Live grep (Snacks picker)
map("n", "<leader>fg", function()
  Snacks.picker.grep()
end, { desc = "Live Grep" })

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
  Snacks.picker.buffers()
end, { desc = "Buffers" })

-- Find files
map("n", "<leader>ff", function()
  Snacks.picker.files()
end, { desc = "Find Files" })

-- Recent files
map("n", "<leader>fr", function()
  Snacks.picker.recent()
end, { desc = "Recent Files" })

-- Lazygit
map("n", "<leader>gg", "<cmd>Lazygit<CR>", opts)
