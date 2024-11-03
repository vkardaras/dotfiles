return {
    -- https://github.com/stevearc/oil.nvim
    'stevearc/oil.nvim',
    opts = {},
    dependencies = { "nvim-tree/nvim-web-devicons" }, -- use if prefer nvim-web-devicons
    config = function()
        require("oil").setup({
            default_file_explorer = true,
            delete_to_trash = true,
            skip_confirm_for_simple_edits = false,
            view_options = {
                show_hidden = true,
            }
        })
    end
}
