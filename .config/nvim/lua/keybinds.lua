-- Keybindings in Lua

-- Map function for convenience
local map = vim.api.nvim_set_keymap
local opts = { noremap = true, silent = true }

-- Normal Mode Keybindings
map("n", "<leader>e", ":Neotree toggle<CR>", opts) -- Toggle NvimTree
map("n", "<leader>fg", ":Telescope live_grep<CR>", opts) -- Live grep with Telescope

-- Insert Mode Keybindings
map("i", "jk", "<Esc>", opts) -- Quickly exit Insert mode

-- Visual Mode Keybindings
map("v", "<leader>y", '"+y', opts) -- Copy to system clipboard
map("v", "<leader>p", '"+p', opts) -- Paste from system clipboard
-- Require the which-key module
local wk = require("which-key")

-- Register key mappings with descriptions
vim.keymap.set("n", "<leader>bn", ":bnext<CR>", { noremap = true, silent = true })

-- Switch to previous buffer
vim.keymap.set("n", "<leader>bp", ":bprev<CR>", { noremap = true, silent = true })
-- Close the current buffer
vim.keymap.set("n", "<leader>bd", ":bd<CR>", { noremap = true, silent = true })
-- Find files
vim.keymap.set("n", "<leader>ff", ":Telescope find_files<CR>", { noremap = true, silent = true })
-- Find recent files
vim.keymap.set("n", "<leader>fr", ":Telescope oldfiles<CR>", { noremap = true, silent = true })
-- List buffers
vim.keymap.set("n", "<leader>bb", ":Telescope buffers<CR>", { noremap = true, silent = true })
vim.keymap.set("n","<leader>gn",":Neogit<CR>",{noremap=true,silent=true})
vim.keymap.set("n","<leader>si",":IronRepl<CR>",{noremap=true,silent=true})
