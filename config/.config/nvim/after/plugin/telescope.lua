local telescope_status_ok, telescope = pcall(require, "telescope")
local builtin_status_ok, builtin = pcall(require, "telescope.builtin")
if not telescope_status_ok or not builtin_status_ok then
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

map("n", "<leader>pf", builtin.find_files, opts)
map("n", "<leader>gf", builtin.git_files, opts)
map("n", "<leader>sp", builtin.live_grep, opts)
map("n", "<leader>bb", builtin.buffers, opts)
map("n", "<leader>fh", builtin.help_tags, opts)
map("n", "<leader>gp", function()
	builtin.grep_string({ search = vim.fn.input("Grep > ") })
end, opts)
