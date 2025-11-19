return {
  {
    "snacks.nvim",
    opts = {
      dashboard = {
        preset = {
          pick = function(cmd, opts)
            return LazyVim.pick(cmd, opts)()
          end,
          header = [[
	                                                                    
	       ████ ██████           █████      ██                    
	      ███████████             █████                            
	      █████████ ███████████████████ ███   ███████████  
	     █████████  ███    █████████████ █████ ██████████████  
	    █████████ ██████████ █████████ █████ █████ ████ █████  
	  ███████████ ███    ███ █████████ █████ █████ ████ █████ 
	 ██████  █████████████████████ ████ █████ █████ ████ ██████]],
        },
      },
      explorer = {
        win = {
          position = "left",
        },
      },
      picker = {
        sources = {
          files = { hidden = true },
        },
      },
    },
    keys = {

      {
        "-",
        function()
          require("snacks").explorer({
            cwd = vim.fn.expand("%:p:h"),
          })
        end,
        desc = "Open explorer in current file directory",
      },
      {
        "<leader>e",
        function()
          require("snacks").explorer({
            cwd = vim.fn.expand("%:p:h"),
          })
        end,
        desc = "Open explorer in current file directory",
      },
    },
  },
}
