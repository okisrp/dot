vim.keymap.set("n", "<M-S-s>", vim.cmd.source)

vim.keymap.set({ "n", "i" }, "<M-q>", vim.cmd.xall)

vim.keymap.set("n", "<M-s>", vim.cmd.write)

vim.keymap.set("n", "<M-e>", vim.cmd.bdelete)

vim.keymap.set("n", "<M-k>", vim.cmd.bprevious)
vim.keymap.set("n", "<M-j>", vim.cmd.bnext)

vim.keymap.set("c", "<M-k>", "<Up>")
vim.keymap.set("c", "<M-j>", "<Down>")
