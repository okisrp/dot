return {
	"stevearc/oil.nvim",
	opts = {
		columns = {},
		view_options = {
			show_hidden = true,
		},
		delete_to_trash = true,
		float = {
			border = "single",
		},
		keymaps_help = {
			border = "single",
		},
	},
	keys = {
		{ "-", vim.cmd.Oil, mode = "n" },
	},
}
