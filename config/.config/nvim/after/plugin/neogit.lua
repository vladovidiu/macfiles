local status_ok, neogit = pcall(require, "neogit")
if not status_ok then
    return
end

local map = vim.keymap.set
local opts = { noremap = true, silent = true }

neogit.setup({})

map("n", "<leader>gg", "<cmd>Neogit<cr>", opts)

vim.cmd([[
    highlight diffAdded guifg=#449dab
    highlight NeogitDiffAdd guifg=#449dab

    highlight diffRemoved guifg=#914c54
    highlight NeogitDiffDelete guifg=#914c54

    highlight diffOldFile guifg=#e0af68 
    highlight diffNewFile guifg=#ff9e64

    highlight NeogitDiffAddHighlight guifg=#449dab guibg=#449dab
    highlight NeogitDiffDeleteHighlight guifg=#914c54 guibg=#914c54

    highlight NeogitDiffContextHighlight guibg=none guifg=#a9b1d6
]])
