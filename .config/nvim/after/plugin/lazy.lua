local status, _ = pcall(require, "lazy")

if not status then
    return
end

vim.keymap.set("n", "<M-S-l>", vim.cmd.Lazy)
