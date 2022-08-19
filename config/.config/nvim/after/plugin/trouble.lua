local status_ok, trouble = pcall(require, "trouble")

if not status_ok then
	return
end

local map = vim.keymap.set
local opts = { noremap = true, silent = true }

trouble.setup({})
map("n", "<leader>xx", "<cmd>Trouble<cr>", opts)
map("n", "<leader>xw", "<cmd>Trouble workspace_diagnostics<cr>", opts)
map("n", "<leader>xd", "<cmd>Trouble document_diagnostics<cr>", opts)
map("n", "<leader>xl", "<cmd>Trouble loclist<cr>", opts)
map("n", "<leader>xq", "<cmd>Trouble quickfix<cr>", opts)
map("n", "gR", "<cmd>Trouble lsp_references<cr>", opts)
