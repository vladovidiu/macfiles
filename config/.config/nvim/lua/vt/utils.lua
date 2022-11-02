local async = require("plenary.async")
local map = vim.keymap.set

local keymap_opts = { noremap = true, silent = true }

local packer_sync = function()
	async.run(function()
		vim.notify.async("Syncing packer.", "info", {
			title = "Packer",
		})
	end)
	local snapshot_time = os.date("!%Y-%m-%dT%TZ")
	vim.cmd("PackerSnapshot " .. snapshot_time)
	vim.cmd("PackerSync")
end

map("n", "<leader>ps", "", {
	callback = packer_sync,
})

-- Diagnostics
local diag_float_grp = vim.api.nvim_create_augroup("DiagnosticFloat", { clear = true })
vim.api.nvim_create_autocmd("CursorHold", {
	callback = function()
		vim.diagnostic.open_float(nil, { focusable = false })
	end,
	group = diag_float_grp,
})

map("n", "g[", vim.diagnostic.goto_prev, keymap_opts)
map("n", "g]", vim.diagnostic.goto_next, keymap_opts)
