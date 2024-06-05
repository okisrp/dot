return {
    "justinmk/vim-sneak",
    event = {
        "BufReadPre",
        "BufNewFile",
    },
    init = function()
        vim.cmd("let g:sneak#use_ic_scs = 1")
    end,
}
