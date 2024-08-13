vim.opt.encoding = "utf-8"
vim.opt.fileencoding = "utf-8"

vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.swapfile = false

vim.opt.undodir = vim.fn.stdpath("data") .. "/undo"
vim.opt.undofile = true

vim.opt.clipboard:append("unnamedplus")

vim.opt.hidden = true

vim.opt.shiftwidth = 4
vim.opt.tabstop = 4
vim.opt.softtabstop = -1

vim.opt.expandtab = false
vim.opt.smarttab = true

vim.opt.autoindent = true
vim.opt.smartindent = true

vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.wrap = false
vim.opt.breakindent = true
vim.opt.linebreak = true

vim.opt.splitbelow = true
vim.opt.splitright = true

vim.wo.number = false
vim.wo.relativenumber = false

-- vim.opt.cmdheight = 0
-- vim.opt.statusline = "%<%t [buf №%n] -%{mode()}-%= %r %h -%p%%- [%l:%c]"

vim.opt.cursorline = true
vim.opt.guicursor = ""
vim.opt.mouse = "a"

vim.opt.list = true
vim.opt.listchars = {
	tab = "¬ ",
	trail = "·",
}
