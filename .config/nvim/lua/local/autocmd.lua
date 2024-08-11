-- Save cursor position on buffer closing.
vim.api.nvim_create_autocmd("BufReadPost", {
	pattern = "*",
	callback = function()
		if vim.fn.line("'\"") > 1 and vim.fn.line("'\"") <= vim.fn.line("$") then
			vim.cmd("normal! g`\"")
		end
	end,
})

-- Delete extra spaces at the end of lines when saving buffer.
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

-- Open git commit message buffer in insert mode at the beginning of first line.
vim.api.nvim_create_autocmd("FileType", {
	pattern = "gitcommit",
	callback = function()
		vim.cmd.normal("gg0")
		vim.cmd("startinsert")
	end,
})
