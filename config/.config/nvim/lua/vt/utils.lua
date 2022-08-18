local map = vim.keymap.set

local packer_sync = function ()
    local snapshot_time = os.date("!%Y-%m-%dT%TZ")
    vim.cmd("PackerSnapshot " .. snapshot_time)
    vim.cmd("PackerSync")
end

map("n", "<leader>ps", "", {
    callback = packer_sync,
})
