local status_ok, lsp_installer = pcall(require, "nvim-lsp-installer")
if not status_ok then
	return
end

local go_status_ok, go = pcall(require, "go")
if not go_status_ok then
	return
end

local rust_status_ok, rust_tools = pcall(require, "rust-tools")
if not rust_status_ok then
	return
end

go.setup({
	test_runner = "richgo",
	run_in_floaterm = true,
})

rust_tools.setup({})

local lspconfig = require("lspconfig")

local servers = { "jsonls", "gopls", "sumneko_lua", "tsserver" }

lsp_installer.setup({
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
	lspconfig[server].setup(opts)
end
