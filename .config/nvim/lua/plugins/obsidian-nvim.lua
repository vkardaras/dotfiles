return {
    -- https://github.com/epwalsh/obsidian.nvim
    "epwalsh/obsidian.nvim",
    version = "*",
    lazy = true,
    ft = "markdown",
    dependencies = {
        "nvim-lua/plenary.nvim",
    },
    opts = {
        workspaces = {
            {
                name = "personal",
                path = "~/Documents/MyVault",
            },
        },
        ui = { enable = false },
        templates = {
            folder = "_templates",
            date_format = "%Y-%m-%d-%a",
            time_format = "%H:%M",
        },
        note_id_func = function(title)
            return title
        end,
    },
}
