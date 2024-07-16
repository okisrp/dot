local function map(mode, lhs, rhs, opts)
	local opts, t = opts or { noremap = true }, {}
	mode:gsub(".", function(c) table.insert(t, c) end)
	vim.keymap.set(t, lhs, rhs, opts)
end

map("nvi", "<C-q>", vim.cmd.xall)
map("nvi", "<C-s>", vim.cmd.write)

map("n", "<M-e>", vim.cmd.bdelete)
map("n", "<M-k>", vim.cmd.bprevious)
map("n", "<M-j>", vim.cmd.bnext)

map("c", "<M-k>", "<Up>")
map("c", "<M-j>", "<Down>")

map("v", "<", "<gv")
map("v", ">", ">gv")

map("nv", "<Space>", "<Nop>")
