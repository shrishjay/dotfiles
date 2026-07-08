vim.api.nvim_create_user_command("PackAdd", function(opts)
    vim.pack.add(opts.fargs)
end, { nargs = "+", desc = "Add plugins (:PackAdd user/repo1 user/repo2)" })

-- Pack Delete and Update cmds are built-in on Nightly 0.13
vim.api.nvim_create_user_command("PackDel", function(opts)
    vim.pack.del(opts.fargs)
end, { nargs = "+", desc = "Delete plugins (:PackDel plugin1 plugin2)" })

vim.api.nvim_create_user_command("PackUpdate", function(opts)
	-- checks if any argument is passed
    if opts.args:match("%S") then
        -- update specific plugins
        local plugins = vim.split(opts.args, "%s+", { trimempty = true })
		-- update only specified plugins
        vim.pack.update(plugins)
    else
        -- update all
        vim.pack.update()
    end
end, { nargs = "*", desc = "Update all plugins or specific ones" })
vim.api.nvim_create_autocmd("CursorHold", {
	callback = function()
		vim.diagnostic.open_float(nil, { focus = false })
	end,
})
vim.api.nvim_create_autocmd({ "ColorScheme", "VimEnter", "BufEnter" }, {
  callback = function()
    vim.api.nvim_set_hl(0, "NormalFloat",  { bg = "none" })
    vim.api.nvim_set_hl(0, "FloatBorder",  { bg = "none" })
    vim.api.nvim_set_hl(0, "Pmenu",        { bg = "none" })
    vim.api.nvim_set_hl(0, "PmenuSel",     { bg = "none", fg = "#89b4fa" })
    vim.api.nvim_set_hl(0, "PmenuThumb",   { bg = "none" })
    vim.api.nvim_set_hl(0, "PmenuSbar",    { bg = "none" })
  end,
})

vim.diagnostic.config({
	virtual_text = false,
	float = {
		border = "rounded",
		source = true,
	},
})

vim.api.nvim_create_autocmd("TermOpen", {
	callback = function()
		vim.opt_local.winbar = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(vim.fn.bufnr("#")), ":t")
	end,
})
