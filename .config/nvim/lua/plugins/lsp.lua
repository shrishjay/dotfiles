require("mason").setup()
require("mason-lspconfig").setup{
ensure_installed={"lua_ls","pyright"},
}

-- vim.o.autocomplete=true
vim.lsp.enable({
  "lua_ls",
  "pyright",
  "clangd",
})

vim.lsp.config("lua_ls", {
    settings = {
        Lua = {
            diagnostics = {
                globals = { "vim" }}}}
              })


-- vim.api.nvim_create_autocmd("LspAttach", {
--  callback = function(ev)
--    local client = assert(vim.lsp.get_client_by_id(ev.data.client_id))
--    if client:supports_method("textDocument/completion") then
--      vim.lsp.completion.enable(true, client.id, ev.buf,{autotrigger=false})
--   end
--  end,
-- })
-- vim.opt.complete:append("o")
-- vim.opt.completeopt = { "menuone", "noselect", "popup","nosort","fuzzy" }
vim.opt.pumheight = 15
vim.opt.pumborder = "rounded"
require("mini.completion").setup({
    lsp_completion = {
        auto_setup = true,
    }
})

--- mini snippets ---
local MiniSnippets = require("mini.snippets")
MiniSnippets.setup({
    snippets = {
        MiniSnippets.gen_loader.from_lang(), -- loads friendly-snippets
    },
})
MiniSnippets.start_lsp_server({ match = false })
vim.keymap.set("i", "<Tab>", function()
  if vim.fn.pumvisible() == 1 then
    return "<C-n>"
  end
  return "<Tab>"
end, { expr = true })

vim.keymap.set("i", "<S-Tab>", function()
  if vim.fn.pumvisible() == 1 then
    return "<C-p>"
  end
  return "<S-Tab>"
end, { expr = true })
vim.keymap.set("i", "<CR>", function()
  if vim.fn.pumvisible() == 1 then
    return "<C-y>"
  end
  return "<CR>"
end, { expr = true })
