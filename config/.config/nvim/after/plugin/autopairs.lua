local status_ok, npairs = pcall(require, "nvim-autopairs")
if not status_ok then
	return
end

npairs.setup({
	check_ts = true,
	ts_config = {
		lua = { "string" },
		javascript = { "template_string" },
		java = false,
	},
})
