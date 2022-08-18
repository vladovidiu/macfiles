local status_ok, feline = pcall(require, "feline")
if not status_ok then return end

local vi_mode_utils = require("feline.providers.vi_mode")
local lsp = require("feline.providers.lsp")
local icons = require("nvim-web-devicons")
local tokyonight_colors = require('tokyonight.colors').setup { style = 'storm' }

local vi_mode_text = {
	NORMAL = " NORMAL ",
	OP = " OP ",
	INSERT = " INSERT ",
	VISUAL = " VISUAL ",
	LINES = " LINES ",
	BLOCK = " BLOCK ",
	REPLACE = " REPLACE ",
	["V-REPLACE"] = " V-REPLACE ",
	ENTER = " ENTER ",
	MORE = " MORE ",
	SELECT = " SELECT ",
	COMMAND = " COMMAND ",
	SHELL = " SHELL ",
	TERM = " TERM ",
	NONE = " NONE ",
	CONFIRM = " CONFIRM ",
}

local force_inactive = {
	filetypes = {},
	buftypes = {},
	bufnames = {},
}

force_inactive.filetypes = {
	"packer",
	"Telescope",
}

force_inactive.buftypes = {
	"terminal",
}

local components = {
	active = { {}, {}, {} },
	inactive = { {}, {}, {} },
}

local colors = {
    bg = tokyonight_colors.bg_statusline,
    fg = tokyonight_colors.fg,
    yellow = tokyonight_colors.yellow,
    cyan = tokyonight_colors.cyan,
    darkblue = tokyonight_colors.blue0,
    green = tokyonight_colors.green,
    orange = tokyonight_colors.orange,
    violet = tokyonight_colors.purple,
    magenta = tokyonight_colors.magenta,
    blue = tokyonight_colors.blue,
    red = tokyonight_colors.red,
    light_bg = tokyonight_colors.bg_highlight,
    primary_blue = tokyonight_colors.blue5,
}

local vi_mode_colors = {
    NORMAL = colors.blue,
    OP = colors.primary_blue,
    INSERT = colors.yellow,
    VISUAL = colors.magenta,
    LINES = colors.magenta,
    BLOCK = colors.magenta,
    REPLACE = colors.red,
    ['V-REPLACE'] = colors.red,
    ENTER = colors.cyan,
    MORE = colors.cyan,
    SELECT = colors.orange,
    COMMAND = colors.primary_blue,
    SHELL = colors.green,
    TERM = colors.green,
    NONE = colors.green,
}

-- LEFT
-- vi-mode
components.active[1][1] = {
	hl = function()
		local val = {}

		val.bg = vi_mode_utils.get_mode_color()
		val.fg = "black"
		val.style = "bold"

		return val
	end,
	right_sep = " ",
}

-- vi-symbol
components.active[1][2] = {
	provider = function()
		return vi_mode_text[vi_mode_utils.get_vim_mode()]
	end,
	hl = function()
		local val = {}

		val.fg = "bg"
		val.bg = vi_mode_utils.get_mode_color()
		val.style = "bold"

		return val
	end,
	right_set = " ",
}

-- filename
components.active[1][3] = {
	provider = {
		name = "file_info",
		opts = {
			type = "relative-short",
			file_readonly_icon = "  ",
			file_modified_icon = "  ",
		},
	},
	hl = {
		fg = "white",
		bg = "bg",
		style = "bold",
	},
	right_sep = {
		str = "  ",
		hl = {
			fg = "white",
			bg = "bg",
			style = "bold",
		},
	},
	left_sep = " ",
}

-- git branch
components.active[1][4] = {
	provider = "git_branch",
	hl = {
		fg = "yellow",
		bg = "bg",
		style = "bold",
	},
}

-- diffAdd
components.active[1][5] = {
	provider = "git_diff_added",
	hl = {
		fg = "green",
		bg = "bg",
		style = "bold",
	},
}
-- diffModfified
components.active[1][6] = {
	provider = "git_diff_changed",
	hl = {
		fg = "orange",
		bg = "bg",
		style = "bold",
	},
}

-- diffRemove
components.active[1][7] = {
	provider = "git_diff_removed",
	hl = {
		fg = "red",
		bg = "bg",
		style = "bold",
	},
}

-- MIDDLE

-- diagnosticErrors
components.active[2][5] = {
	provider = "diagnostic_errors",
	enabled = function()
		return lsp.diagnostics_exist(vim.diagnostic.severity.ERROR)
	end,
	hl = {
		fg = "red",
		style = "bold",
	},
}

-- diagnosticWarn
components.active[2][6] = {
	provider = "diagnostic_warnings",
	enabled = function()
		return lsp.diagnostics_exist(vim.diagnostic.severity.WARN)
	end,
	hl = {
		fg = "yellow",
		style = "bold",
	},
}

-- diagnosticHint
components.active[2][7] = {
	provider = "diagnostic_hints",
	enabled = function()
		return lsp.diagnostics_exist(vim.diagnostic.severity.HINT)
	end,
	hl = {
		fg = "cyan",
		style = "bold",
	},
}

-- diagnosticInfo
components.active[2][8] = {
	provider = "diagnostic_info",
	enabled = function()
		return lsp.diagnostics_exist(vim.diagnostic.severity.INFO)
	end,
	hl = {
		fg = "skyblue",
		style = "bold",
	},
}

-- RIGHT

-- lsp name
components.active[3][1] = {
	provider = "lsp_client_names",
	enabled = false,
	hl = {
		fg = "yellow",
		bg = "bg",
		style = "bold",
	},
	right_sep = " ",
}

-- fileIcon
components.active[3][2] = {
	provider = function()
		local filename = vim.fn.expand("%:t")
		local extension = vim.fn.expand("%:e")
		local icon = icons.get_icon(filename, extension)

		if icon == nil then
			icon = " "
		end

		return icon
	end,
	enabled = false,
	hl = function()
		local val = {}
		local filename = vim.fn.expand("%:t")
		local extension = vim.fn.expand("%:e")
		local icon, name = icons.get_icon(filename, extension)

		if icon ~= nil then
			val.fg = vim.fn.synIDattr(vim.fn.hlID(name), "fg")
		else
			val.fg = "white"
		end
		val.bg = "bg"
		val.style = "bold"

		return val
	end,
	right_sep = " ",
}

-- fileType
components.active[3][3] = {
	provider = "file_type",
	enabled = false,
	hl = function()
		local val = {}
		local filename = vim.fn.expand("%:t")
		local extension = vim.fn.expand("%:e")
		local icon, name = icons.get_icon(filename, extension)

		if icon ~= nil then
			val.fg = vim.fn.synIDattr(vim.fn.hlID(name), "fg")
		else
			val.fg = "white"
		end
		val.bg = "bg"
		val.style = "bold"

		return val
	end,
	right_sep = " ",
}

-- fileSize
components.active[3][4] = {
	provider = "file_size",
	enabled = false,
	hl = {
		fg = "skyblue",
		bg = "bg",
		style = "bold",
	},
	right_sep = " ",
}

-- fileformat
components.active[3][5] = {
	provider = function()
		return "" .. vim.bo.fileformat .. ""
	end,
	enabled = false,
	hl = {
		fg = "white",
		bg = "bg",
		style = "bold",
	},
	right_sep = " ",
}

-- lineInfo
components.active[3][6] = {
	provider = "position",
	hl = {
		fg = "white",
		bg = "bg",
		style = "bold",
	},
	right_sep = " ",
}

-- linePercent
components.active[3][7] = {
	provider = "line_percentage",
	hl = {
		fg = "white",
		bg = "bg",
		style = "bold",
	},
	right_sep = " ",
}

-- scrollBar
components.active[3][8] = {
	provider = "scroll_bar",
	hl = {
		fg = "yellow",
		bg = "bg",
	},
}

-- INACTIVE

-- fileType
components.inactive[1][1] = {
	provider = "file_type",
	hl = {
		fg = "black",
		bg = "cyan",
		style = "bold",
	},
	left_sep = {
		str = " ",
		hl = {
			fg = "NONE",
			bg = "cyan",
		},
	},
	right_sep = {
		{
			str = " ",
			hl = {
				fg = "NONE",
				bg = "cyan",
			},
		},
		" ",
	},
}

feline.setup({
	theme = colors,
	components = components,
    vi_mode_colors = vi_mode_colors,
	force_inactive = force_inactive,
})
