local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
	PACKER_BOOTSTRAP = fn.system({
		"git",
		"clone",
		"--depth",
		"1",
		"https://github.com/wbthomason/packer.nvim",
		install_path,
	})

	print("Installing packer...")
	vim.cmd([[packadd packer.nvim]])
end

local status_ok, packer = pcall(require, "packer")
if not status_ok then
	return
end

packer.init({
	display = {
		open_fn = function()
			return require("packer.util").float({ border = "rounded" })
		end,
	},
})

return packer.startup(function(use)
	use("wbthomason/packer.nvim") -- have packer manage itself

	-- Utils
	use("nvim-lua/plenary.nvim") -- useful lua functions
	use("nvim-lua/popup.nvim") -- popup api implementation
	use("lewis6991/impatient.nvim") -- speeds up loading lua modules
	use("max397574/better-escape.nvim") -- fixes the delay for quick escape jk
	use("windwp/nvim-autopairs") -- completes pairs
	use("numToStr/Comment.nvim")

	-- Themes
	use("folke/tokyonight.nvim") -- main theme for now

	-- Status line - Feline
	use("feline-nvim/feline.nvim")

	-- UI improvements
	use("kyazdani42/nvim-web-devicons") -- adds icons to files
	use("rcarriga/nvim-notify") -- beautiful notifiers
	use("norcalli/nvim-colorizer.lua") -- adds colour to colour codes

	-- Fuzzy Finder
	use("nvim-telescope/telescope.nvim")
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
	use("nvim-telescope/telescope-packer.nvim")

	-- LSP
	use("jose-elias-alvarez/null-ls.nvim")
	use("glepnir/lspsaga.nvim")
	use("folke/trouble.nvim")

	use({
		"VonHeikemen/lsp-zero.nvim",
		requires = {
			-- LSP Support
			{ "neovim/nvim-lspconfig" },
			{ "williamboman/mason.nvim" },
			{ "williamboman/mason-lspconfig.nvim" },

			-- Autocompletion
			{ "hrsh7th/nvim-cmp" },
			{ "hrsh7th/cmp-buffer" },
			{ "hrsh7th/cmp-path" },
			{ "saadparwaiz1/cmp_luasnip" },
			{ "hrsh7th/cmp-nvim-lsp" },
			{ "hrsh7th/cmp-nvim-lua" },
			{ "hrsh7th/cmp-cmdline" },
			{ "hrsh7th/cmp-nvim-lsp-signature-help" },

			-- Snippets
			{ "L3MON4D3/LuaSnip" },
			{ "rafamadriz/friendly-snippets" },
		},
	})

	-- Syntax
	use({
		"nvim-treesitter/nvim-treesitter",
		run = ":TSUpdate",
	})
	use("p00f/nvim-ts-rainbow")
	use("nvim-treesitter/playground")
	use("nvim-treesitter/nvim-treesitter-textobjects")
	use("JoosepAlviste/nvim-ts-context-commentstring")

	-- Git
	use("TimUntersberger/neogit")
	use("lewis6991/gitsigns.nvim")

	-- Programming Languages
	use("crispgm/nvim-go")
	use("simrat39/rust-tools.nvim")
end)
