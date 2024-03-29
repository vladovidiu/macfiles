local opt = vim.opt

-- File Encoding
opt.encoding = "utf-8"
opt.fileencoding = "utf-8"

-- Backups
opt.backup = false
opt.writebackup = false
opt.swapfile = false
opt.undofile = true
opt.undodir = vim.fn.stdpath("cache") .. "/undodir"

-- UI changes
opt.termguicolors = true
opt.timeoutlen = 300
opt.updatetime = 100
opt.scrolloff = 5
opt.colorcolumn = "80"
opt.cmdheight = 1
opt.guicursor = ""
opt.wrap = false
opt.cursorline = true

opt.splitbelow = true
opt.splitright = true

opt.number = true
opt.relativenumber = true
opt.laststatus = 3
opt.signcolumn = "yes"

opt.showmode = false

-- Indenting
opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4
opt.expandtab = true
opt.smartindent = true

-- Clipboard
opt.clipboard:append({ "unnamedplus" })

opt.backspace = { "start", "eol", "indent" }

-- Undercurl
vim.cmd([[let &t_Cs = "\e[4:3m"]])
vim.cmd([[let &t_Ce = "\e[4:0m"]])

vim.g.mapleader = " "

-- netrw options
vim.g.netrw_banner = 0
vim.g.netrw_silent = 1

-- Disable unneeded builtin plugins
local default_plugins = {
	"2html_plugin",
	"getscript",
	"getscriptPlugin",
	"gzip",
	"logipat",
	"machparen",
	"matchit",
	"tar",
	"tarPlugin",
	"rrhelper",
	"spellfile_plugin",
	"vimball",
	"vimballPlugin",
	"zip",
	"zipPlugin",
	"tutor",
	"rplugin",
	"synmenu",
	"optwin",
	"compiler",
	"bugreport",
	"ftplugin",
}

for _, plugin in pairs(default_plugins) do
	vim.g["loaded_" .. plugin] = 1
end
