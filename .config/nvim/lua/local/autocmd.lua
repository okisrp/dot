-- Save cursor position on exit.
vim.api.nvim_create_autocmd("BufReadPost", {
	pattern = "*",
	callback = function()
		vim.cmd [[ if line("'\"") > 1 && line("'\"") <= line("$") | execute "normal! g`\"" | endif ]]
	end,
})

-- Remove extra whitespaces at the end of lines.
vim.api.nvim_create_autocmd("BufWritePre", {
	pattern = "*",
	callback = function()
		local save_cursor = vim.fn.getpos(".")
		pcall(function () vim.cmd [[%s/\s\+$//e]] end)
		vim.fn.setpos(".", save_cursor)
	end,
})

-- Highlight yanked text.
vim.api.nvim_create_autocmd("TextYankPost", {
	pattern = "*",
	callback = function()
		vim.highlight.on_yank({ higroup = "IncSearch",  timeout = 150 })
	end,
})

-- Open help buffer as whole window.
vim.api.nvim_create_autocmd("FileType", {
	pattern = "help",
	group = vim.api.nvim_create_augroup("HelpBuf", { clear = true }),
	command = "only",
})
