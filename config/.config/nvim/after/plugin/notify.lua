local status_ok, notify = pcall(require, "notify")
if not status_ok then
	return
end

notify.setup({
	enable = false,
	background_colour = "#000000",
	fps = 60,
})

vim.notify = notify
