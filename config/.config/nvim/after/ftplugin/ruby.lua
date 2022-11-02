vim.notify = function(msg, level, opts) end

vim.keymap.set("n", "<leader>ft", "", {
	callback = function()
		vim.cmd([[
            !rubocop -A %
        ]])
	end,
})
