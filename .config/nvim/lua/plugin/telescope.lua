return {
	"nvim-telescope/telescope.nvim",
	tag = "0.1.6",
	dependencies = {
		"nvim-lua/plenary.nvim",
	},
	opts = {
		defaults = {
			borderchars = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
			file_ignore_patterns = {
				".git/",
				"node_modules/",
			},
			mappings = {
				i = {
					[ "<M-k>" ] = "move_selection_previous",
					[ "<M-j>" ] = "move_selection_next",
					[ "<Esc>" ] = "close",
				},
			},
		},
		pickers = {
			find_files = {
				hidden = true,
			},
		},
	},
	keys = {
		{ "<M-f>", ":Telescope current_buffer_fuzzy_find<Cr>" },
		{ "<M-o>", ":Telescope find_files<Cr>" },
		{ "<M-l>", ":Telescope live_grep<Cr>" },
		{ "<M-b>", ":Telescope buffers<Cr>" },
		{ "<M-h>", ":Telescope help_tags<Cr>" },
	},
}
