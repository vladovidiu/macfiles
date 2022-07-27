return {
	tools = {
		on_initialized = function()
			vim.api.nvim_create_autocmd({ "BufWritePost", "BufEnter", "CursorHold", "InsertLeave" }, {
				pattern = { "*.rs" },
				callback = function()
					vim.lsp.codelens.refresh()
				end,
			})
		end,
		inlay_hints = {
			show_variable_name = true,
			show_parameter_hints = true,
			parameter_hints_prefix = " ",
			other_hints_prefix = " ",
		},
		hover_actions = {
			auto_focus = false,
			border = "rounded",
			width = 60,
			-- height = 30,
		},
		autoSetHints = true,
		hover_with_actions = true,
	},
	server = {
		cmd = { os.getenv("HOME") .. "/.local/bin/rust-analyzer" },
		on_attach = require("vt.lsp.handlers").on_attach,
		capabilities = require("vt.lsp.handlers").capabilities,

		settings = {
			["rust-analyzer"] = {
				lens = {
					enable = true,
				},
				checkOnSave = {
					command = "clippy",
				},
				assist = {
					importEnforceGranularity = true,
					importPrefix = "crate",
				},
				cargo = {
					allFeatures = true,
				},
				inlayHints = {
					lifetimeElisionHints = {
						enable = true,
						useParameterNames = true,
					},
				},
			},
		},
	},
}
