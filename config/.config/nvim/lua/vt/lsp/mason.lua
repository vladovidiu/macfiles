local mason_status_ok, mason = pcall(require, "mason")
if not mason_status_ok then
	return
end

local mason_lspconfig_status_ok, mason_lspconfig = pcall(require, "mason-lspconfig")
if not mason_lspconfig_status_ok then
	return
end

mason.setup({
	ui = {
		icons = {
			package_installed = "✓",
			package_pending = "➜",
			package_uninstalled = "✗",
		},
		border = "rounded",
	},
})

local lspconfig = require("lspconfig")

local servers = {
	"jsonls",
	"gopls",
	"sumneko_lua",
	"tsserver",
	"cssls",
	"prismals",
	"tailwindcss",
	"rust_analyzer",
	"taplo",
}

mason_lspconfig.setup({
	ensure_installed = servers,
})

-- Register a handler that will be called for all installed servers.
-- Alternatively, you may also register handlers on specific server instances instead (see example below).
for _, server in pairs(servers) do
	local opts = {
		on_attach = require("vt.lsp.handlers").on_attach,
		capabilities = require("vt.lsp.handlers").capabilities,
	}
	local has_custom_opts, server_custom_opts = pcall(require, "vt.lsp.settings." .. server)

	if has_custom_opts then
		opts = vim.tbl_deep_extend("force", server_custom_opts, opts)
	end

	if server == "rust_analyzer" then
		local rust_tools_status_ok, rust_tools = pcall(require, "rust-tools")
		if not rust_tools_status_ok then
			return
		end

		local rust_opts = require("vt.lsp.settings.rust")

		rust_tools.setup(rust_opts)

		goto continue
	end

	lspconfig[server].setup(opts)
	::continue::
end
