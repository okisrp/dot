vim.opt.encoding = "utf-8"
vim.opt.fileencoding = "utf-8"

vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.swapfile = false

vim.opt.undodir = vim.fn.getenv("HOME") .. "/.cache/nvim/undo"
vim.opt.undofile = true

vim.opt.clipboard:append("unnamedplus")

vim.g.mapleader = " "
vim.g.maplocalleader = " m"

vim.wo.number = true
vim.wo.relativenumber = true

vim.opt.cursorline = true
vim.opt.guicursor = ""

vim.opt.mouse = "a"

vim.opt.list = true
vim.opt.listchars = { tab = "> ", trail = "·", nbsp = "+" }

vim.opt.scroll = 15
vim.opt.scrolloff = 9
vim.opt.sidescrolloff = 27

vim.opt.hidden = true

vim.opt.splitbelow = true
vim.opt.splitright = true

vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.wrap = false
vim.opt.breakindent = true

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.expandtab = false
vim.opt.smarttab = true

vim.opt.autoindent = true
vim.opt.smartindent = true

vim.opt.shiftwidth = 4
vim.opt.tabstop = 4
