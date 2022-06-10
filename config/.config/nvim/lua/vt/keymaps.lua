local opts = { noremap = true, silent = true }
local term_opts = { silent = true }

local map = vim.keymap.set

map("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

map("n", "<leader>e", ":Lex 30<cr>", opts)
map("n", "<leader>bs", ":w<cr>", opts)
map("n", "<leader><esc>", ":nohlsearch<cr>", opts)

map("v", "<", "<gv", opts)
map("v", ">", ">gv", opts)

map("t", "<C-h>", "<C-\\><C-N><C-w>h", term_opts)
map("t", "<C-j>", "<C-\\><C-N><C-w>j", term_opts)
map("t", "<C-k>", "<C-\\><C-N><C-w>k", term_opts)
map("t", "<C-l>", "<C-\\><C-N><C-w>l", term_opts)

-- Telescope
map("n", "<leader>pf", "<cmd>Telescope find_files<cr>", opts)
map("n", "<leader>sp", "<cmd>Telescope live_grep<CR>", opts)
map("n", "<leader>bb", "<cmd>Telescope buffers<CR>", opts)
map("n", "<leader>fh", "<cmd>Telescope help_tags<CR>", opts)

-- Neogit
map("n", "<leader>gg", "<cmd>Neogit<cr>", opts)

-- Float term
map("n", "<leader>tt", '<CMD>lua require("FTerm").toggle()<CR>')
map("t", "<leader>tt", '<C-\\><C-n><CMD>lua require("FTerm").toggle()<CR>')
