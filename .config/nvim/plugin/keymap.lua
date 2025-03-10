local function map(mode, lhs, rhs, opts)
	local opts, t = opts or { noremap = true, silent = true }, {}
	mode:gsub(".", function(c) table.insert(t, c) end)
	vim.keymap.set(t, lhs, rhs, opts)
end

map("ni", "<M-q>", vim.cmd.xall)
map("ni", "<M-s>", vim.cmd.write)

map("n", "<M-e>", vim.cmd.bdelete)
map("n", "<M-k>", vim.cmd.bprevious)
map("n", "<M-j>", vim.cmd.bnext)

map("v", "<", "<gv")
map("v", ">", ">gv")

map("nv", "<Space>", "<Nop>")

map("nvic", "<C-g>", "<Esc>")

map("n", "<C-a>", "gg<S-v>G")
