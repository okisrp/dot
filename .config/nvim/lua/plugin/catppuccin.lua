return {
  "catppuccin/nvim",
  name = "catppuccin",
  priority = 999,
  lazy = false,
  init = function ()
    vim.opt.termguicolors = true
  end,
  config = function ()
    require("catppuccin").setup({
      flavour = "mocha",
      transparent_background = true,
      show_end_of_buffer = true,
      term_colors = true,
      dim_inactive = {
        enabled = true,
        percentage = 0.8,
      },
      styles = {
        conditionals = {},
      },
      integrations = {
        vim_sneak = true,
        treesitter = true,
        telescope = {
          enabled = true,
        },
        gitsigns = true,
      },
    })

    vim.cmd.colorscheme("catppuccin")
  end,
}
