local status_ok, null_ls = pcall(require, "null-ls")
if not status_ok then
	return
end

-- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/formatting
local formatting = null_ls.builtins.formatting
-- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/diagnostics
local diagnostics = null_ls.builtins.diagnostics

local augroup_format = vim.api.nvim_create_augroup("Format", { clear = true })

null_ls.setup({
	debug = false,
	sources = {
		diagnostics.eslint_d,
		diagnostics.golangci_lint,
		diagnostics.rubocop,

		formatting.prettierd,
		formatting.stylua,
		formatting.gofmt,
		formatting.rustfmt,
		-- formatting.rufo,
	},
	on_attach = function(client)
		if client.server_capabilities.documentFormattingProvider then
			vim.api.nvim_clear_autocmds({ buffer = 0, group = augroup_format })
			vim.api.nvim_create_autocmd("BufWritePre", {
				group = augroup_format,
				buffer = 0,
				callback = function()
					vim.lsp.buf.format()
				end,
			})
		end
	end,
})
