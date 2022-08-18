local telescope_status_ok, telescope = pcall(require, "telescope")
if not telescope_status_ok then
	return
end

local map = vim.keymap.set

telescope.setup({
	extensions = {
		fzf = {
			fuzzy = true,
			override_generic_sorter = true,
			override_file_sorter = true,
			case_mode = "smart_case",
		},
	},
	defaults = {
		file_ignore_patterns = { "node_modules", "public/build" },
		vimgrep_arguments = {
			"rg",
			"--color=never",
			"--no-heading",
			"--with-filename",
			"--line-number",
			"--column",
			"--smart-case",
		},
	},
})

telescope.load_extension("fzf")
telescope.load_extension("packer")

local opts = { noremap = true, silent = true }

map("n", "<leader>pf", "<cmd>Telescope find_files<cr>", opts)
map("n", "<leader>sp", "<cmd>Telescope live_grep<CR>", opts)
map("n", "<leader>bb", "<cmd>Telescope buffers<CR>", opts)
map("n", "<leader>fh", "<cmd>Telescope help_tags<CR>", opts)
