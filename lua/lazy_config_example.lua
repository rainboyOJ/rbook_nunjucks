-- Lazy.nvim 配置示例
-- 使用 lazy.nvim 管理 rbook 插件

return {
  "your-username/rbook.nvim",  -- 替换为实际的插件仓库
  dependencies = {
    "folke/snacks.nvim",        -- 依赖 snacks.nvim
  },
  config = function()
    local rbook = require("rbook")
    
    -- 基础配置
    rbook.setup({
      -- 用户代码片段路径（可选，默认为 stdpath('data')/rbook/code/）
      codePath = vim.fn.expand("~/.local/share/nvim/rbook/code/"),
      
      -- 用户片段路径（可选，默认为 stdpath('data')/rbook/oiSnippets/）
      snippetPath = vim.fn.expand("~/.local/share/nvim/rbook/oiSnippets/"),
      
      -- 插件内置代码片段路径（自动检测）
      builtinCodePath = false,  -- false = 自动检测，true = 使用，或指定路径
      builtinSnippetPath = false, -- false = 自动检测，true = 使用，或指定路径
    })
    
    -- 自定义快捷键（可选，会覆盖默认快捷键）
    vim.keymap.set('n', '<leader>ac', ":OICodeSnip<CR>", { 
      desc = "插入代码片段" 
    })
    vim.keymap.set('n', '<leader>as', ":OISnipChoose<CR>", { 
      desc = "选择片段" 
    })
    vim.keymap.set('n', '<leader>af', ":OISearchSnip<CR>", { 
      desc = "搜索片段" 
    })
    vim.keymap.set('n', '<leader>ab', ":OIBrowseSnip<CR>", { 
      desc = "浏览分类" 
    })
    
    -- 自定义命令（可选）
    vim.api.nvim_create_user_command("CodeSnip", function()
      vim.cmd("OICodeSnip")
    end, { desc = "插入代码片段" })
    
    vim.api.nvim_create_user_command("SnipSearch", function()
      vim.cmd("OISearchSnip")
    end, { desc = "搜索代码片段" })
    
  end,
  
  -- 可选：延迟加载
  event = "VeryLazy",
  -- 或者根据需要触发加载
  -- keys = { "<leader>ac", "<leader>as", "<leader>af", "<leader>ab" },
  
  -- 可选：插件标签
  -- tag = "v1.0.0",
  
  -- 可选：开发模式配置
  -- dev = true,
  -- dir = "~/path/to/rbook_nunjucks",
}