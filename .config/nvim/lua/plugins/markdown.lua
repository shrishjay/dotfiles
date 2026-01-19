return {
  {
    "brianhuster/live-preview.nvim",
    ft = "markdown",
    dependencies = {
      -- You can choose one of the following pickers
      "folke/snacks.nvim",
    },
  },
  {
    "OXY2DEV/markview.nvim",
    ft = "markdown",
    -- Completion for `blink.cmp`
    dependencies = { "saghen/blink.cmp" },
    -- Toggle markdown checkboxes
    vim.keymap.set("n", "<C-c>", function()
      local line = vim.api.nvim_get_current_line()
      if line:match("%- %[ %]") then
        -- Unchecked -> Checked
        local new_line = line:gsub("%- %[ %]", "- [x]", 1)
        vim.api.nvim_set_current_line(new_line)
      elseif line:match("%- %[x%]") or line:match("%- %[X%]") then
        -- Checked -> Unchecked
        local new_line = line:gsub("%- %[[xX]%]", "- [ ]", 1)
        vim.api.nvim_set_current_line(new_line)
      end
    end, { desc = "Toggle markdown checkbox" }),
  },
}
