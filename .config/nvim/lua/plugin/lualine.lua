return {
	"nvim-lualine/lualine.nvim",
	lazy = false,
	init = function()
		vim.opt.cmdheight = 0
	end,
	opts = {
		options = {
			icons_enabled = false,
			theme = "auto",
			component_separators = "",
			section_separators = "",
			disabled_filetypes = {
				statusline = {},
				winbar = {},
			},
			ignore_focus = {},
			always_divide_middle = true,
			globalstatus = false,
			refresh = {
				statusline = 1000,
				tabline = 1000,
				winbar = 1000,
			}
		},
		sections = {
			lualine_a = { "mode" },
			lualine_b = { "branch" },
			lualine_c = { "filename" },
			lualine_x = {},
			lualine_y = {},
			lualine_z = {},
		},
		inactive_sections = {
			lualine_a = {},
			lualine_b = {},
			lualine_c = { "filename" },
			lualine_x = {},
			lualine_y = {},
			lualine_z = {},
		},
		tabline = {},
		winbar = {},
		inactive_winbar = {},
		extensions = {
			"oil",
		},
	},
}
