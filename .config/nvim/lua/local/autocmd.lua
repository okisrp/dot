vim.api.nvim_create_autocmd("BufReadPost", {
	pattern = "*",
	callback = function()
		if vim.fn.line("'\"") > 1 and vim.fn.line("'\"") <= vim.fn.line("$") then
			vim.cmd("normal! g`\"")
		end
	end,
})

vim.api.nvim_create_autocmd("BufWritePre", {
	pattern = "*",
	callback = function()
		local save_cursor = vim.fn.getpos(".")
		pcall(function () vim.cmd [[%s/\s\+$//e]] end)
		vim.fn.setpos(".", save_cursor)
	end,
})

vim.api.nvim_create_autocmd("TextYankPost", {
	pattern = "*",
	callback = function()
		vim.highlight.on_yank({
			higroup = "IncSearch",
			timeout = 150,
		})
	end,
})

vim.api.nvim_create_autocmd("FileType", {
	pattern = "gitcommit",
	callback = function()
		vim.cmd.normal("gg0")
		vim.cmd("startinsert")
	end,
})

vim.api.nvim_create_autocmd("FileType", {
	pattern = "help",
	callback = function()
		if vim.cmd("execute winnr()") == vim.cmd("execute winnr('$')") then
			vim.keymap.set("n", "q", vim.cmd.bdelete, { buffer = true })
			return
		end
		vim.keymap.set("n", "q", vim.cmd.helpclose, { buffer = true })
	end,
})
