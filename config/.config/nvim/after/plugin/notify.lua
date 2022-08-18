local status_ok, notify = pcall(require, "notify")
if not status_ok then
	return
end

notify.setup({
	background_colour = "#24283b",
})

vim.notify = notify
