local opt = vim.opt

opt.backup = false
opt.writebackup = false
opt.clipboard = "unnamed"
opt.completeopt = { "menu", "menuone", "noselect" }
opt.fileencoding = "utf-8"
opt.ignorecase = true
opt.smartcase = true
opt.mouse = "a"
opt.pumheight = 20
opt.smartindent = true
opt.splitbelow = true
opt.splitright = true
opt.swapfile = false
--opt.termguicolors = false
opt.undofile = true
opt.updatetime = 300
opt.timeoutlen = 300
opt.expandtab = true
opt.undodir = "./.vim/undodir"
opt.conceallevel = 0
opt.showmode = false
opt.scrolloff = 5
opt.whichwrap = 'b,s,<,>,[,],h,l'

opt.list = true
opt.listchars:append("tab:»\\ ")
opt.listchars:append("extends:›")
opt.listchars:append("precedes:‹")
opt.listchars:append("nbsp:·")
opt.listchars:append("trail:·")
opt.listchars:append("eol:↲")

vim.wo.wrap = false
vim.wo.number = true
vim.wo.relativenumber = true
vim.wo.cursorline = true

vim.o.tabstop = 2
vim.o.softtabstop = 2
vim.o.shiftwidth = 2
vim.o.autoindent = true
vim.bo.copyindent = true
vim.o.expandtab = true

vim.g.loaded_machparen = 1
vim.g.loaded_matchit = 1

vim.g.netrw_banner = 0
vim.g.netrw_silent = 1
