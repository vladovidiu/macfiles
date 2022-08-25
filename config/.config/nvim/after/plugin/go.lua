local status_ok, go = pcall(require, "go")

if not status_ok then
	return
end

go.setup({
	notify = false,
	linter = "revive",
	lint_prompt_style = "vt",
})
