return {
    -- TODO: add better comments
    -- WARN: this is a warning
    -- BUG: this is a bug
    "folke/todo-comments.nvim",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
        require('todo-comments').setup()
    end
}
