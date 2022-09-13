vim.g.tokyonight_transparent_sidebar = true
vim.g.tokyonight_transparent = true
vim.g.tokyonight_italic_functions = true

local status_ok, tokyonight = pcall(require, "tokyonight")
if not status_ok then
	return
end

tokyonight.setup({
	transparent = true,
})

vim.cmd("colorscheme tokyonight")

vim.cmd([[
    highlight NormalFloat guibg=none
    highlight MsgArea guibg=none
    highlight Float guibg=none
    highlight Normal guibg=none
    highlight TelescopeNormal guibg=none
    highlight TelescopeBorder guibg=none
    highlight FloatBorder guibg=none
    highlight DiagnosticVirtualTextError guibg=none
    highlight DiagnosticVirtualTextWarn guibg=none
    highlight DiagnosticVirtualTextInfo guibg=none
    highlight DiagnosticVirtualTextHint guibg=none
    highlight LspFloatWinNormal guibg=none
    highlight CursorLineNr guifg=#bb9af7
]])
