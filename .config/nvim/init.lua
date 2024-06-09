local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not (vim.uv or vim.loop).fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable",
		lazypath,
	})
end

vim.opt.rtp:prepend(lazypath)

require("local.autocmd")

require("local.option")
require("local.keymap")

require("lazy").setup("plugin", {
	lockfile = vim.fn.getenv("HOME") .. "/.cache/nvim/lazy/lock.json",
	defaults = {
		lazy = true,
	},
	install = {
		colorscheme = { "catppuccin" },
	},
	ui = {
		border = "single",
		title = " Manage Laziness ",
		title_pos = "left",
	},
	change_detection = {
		notify = false,
	},
})

vim.keymap.set("n", "<M-S-l>", vim.cmd.Lazy)
