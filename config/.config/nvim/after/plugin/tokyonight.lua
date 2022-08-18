vim.g.tokyonight_transparent_sidebar = true
vim.g.tokyonight_transparent = true
vim.g.tokyonight_italic_functions = true

vim.cmd("colorscheme tokyonight")

vim.cmd([[
    highlight NormalFloat guibg=none
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
]])
