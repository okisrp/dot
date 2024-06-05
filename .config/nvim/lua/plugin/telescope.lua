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
    { "<M-f>", ":Telescope current_buffer_fuzzy_find<Cr>", mode = "n" },
    { "<M-o>", ":Telescope find_files<Cr>", mode = "n" },
    { "<M-r>", ":Telescope oldfiles<Cr>", mode = "n" },
    { "<M-b>", ":Telescope buffers<Cr>", mode = "n" },
    { "<M-h>", ":Telescope help_tags<Cr>", mode = "n" },
  },
}
