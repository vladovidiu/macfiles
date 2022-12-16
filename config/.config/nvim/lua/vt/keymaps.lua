local opts = { noremap = true, silent = true }
local term_opts = { silent = true }

local map = vim.keymap.set

map("", "<Space>", "<Nop>", opts)

map("n", "<leader>pv", vim.cmd.Ex)

map("n", "<leader><esc>", "<cmd>nohlsearch<cr>", opts)

map("v", "<", "<gv", opts)
map("v", ">", ">gv", opts)

map("t", "<C-h>", "<C-\\><C-N><C-w>h", term_opts)
map("t", "<C-j>", "<C-\\><C-N><C-w>j", term_opts)
map("t", "<C-k>", "<C-\\><C-N><C-w>k", term_opts)
map("t", "<C-l>", "<C-\\><C-N><C-w>l", term_opts)
