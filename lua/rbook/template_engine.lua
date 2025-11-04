-- 简单的Lua模板引擎
-- 用于处理template.cpp等模板文件

local M = {}

-- 模板变量定义
local template_vars = {
    author = "Rainboy",
    github = "https://github.com/rainboylvx",
    date = os.date("%Y-%m-%d %H:%M:%S"),
}

function M.setup(config)
  if config.vars then
    M.set_vars(config.vars)
  end

  if config.builtinCodePath then
    M.builtinCodePath = config.builtinCodePath
  end

  M.create_commands()

end

-- 设置模板变量
function M.set_var(key, value)
    template_vars[key] = value
end

-- 批量设置模板变量
function M.set_vars(vars)
    for k, v in pairs(vars) do
        template_vars[k] = v
    end
end

-- 获取当前所有模板变量
function M.get_vars()
    return template_vars
end

-- 包含文件内容
local function include_file(file_path)
    -- 支持相对路径和绝对路径
    local full_path
    
    if file_path:match("^/") then
        -- 绝对路径
        full_path = file_path
    elseif file_path:match("^%./") or file_path:match("^../") then
        -- 相对路径，相对于当前工作目录
        full_path = vim.fn.getcwd() .. "/" .. file_path:sub(3)
    else
        -- 相对于项目根目录
        local base_path = _G.RbookRoot
        full_path = base_path .. file_path
    end
    
    local file = io.open(full_path, "r")
    if not file then
        return "// 包含文件未找到: " .. file_path .. " (查找路径: " .. full_path .. ")"
    end
    
    local content = file:read("*all")
    file:close()
    
    -- 移除可能的include语句，避免重复
    content = content:gsub("#include%s*<[^>]+>", "")
    :gsub("#include%s*\"[^\"]+\"", "")
    
    return "\n//oisnip_begin " .. file_path .. " 内容开始\n" .. content .. "\n//oisnip_end " .. file_path .. " 内容结束\n"
end

-- 解析模板字符串
function M.parse_template(template_str)
    local result = template_str
    
    -- 处理文件包含 {{include "path/to/file"}}
    result = result:gsub("{{%s*include%s*\"(.-)\"%s*}}", function(file_path)
        return include_file(file_path)
    end)
    
    -- 替换 {{variable}} 格式的变量
    result = result:gsub("{{(.-)}}", function(var_name)
        var_name = var_name:gsub("^%s+", ""):gsub("%s+$", "") -- 去除空白
        
        -- 跳过include语法
        if var_name:match("^include%s+") then
            return "{{" .. var_name .. "}}"
        end
        
        return template_vars[var_name] or ("{{" .. var_name .. "}}")
    end)
    
    -- 处理条件块 {{#if condition}} ... {{/if}}
    result = result:gsub("{{#if (.-)}}(.-){{/if}}", function(condition, content)
        local var_name = condition:gsub("^%s+", ""):gsub("%s+$", "")
        if template_vars[var_name] and template_vars[var_name] ~= "" then
            return content
        else
            return ""
        end
    end)
    
    return result
end

-- 解析模板文件
function M.parse_template_file(file_path)
    local file = io.open(file_path, "r")
    if not file then
        return nil, "无法打开文件: " .. file_path
    end
    
    local content = file:read("*all")
    file:close()
    
    return M.parse_template(content)
end

-- 应用模板并插入到当前buffer
function M.apply_template_to_buffer(file_path, line_start, line_end)
    local parsed_content, err = M.parse_template_file(file_path)
    if not parsed_content then
        vim.notify("模板解析错误: " .. err, vim.log.levels.ERROR)
        return false
    end
    
    local lines = {}
    -- 按行分割，保留空行
    for line in (parsed_content .. "\n"):gmatch("(.-)\r?\n") do
        table.insert(lines, line)
    end
    
    -- 获取当前buffer
    local buf = vim.api.nvim_get_current_buf()
    
    -- 如果没有指定行范围，替换整个buffer
    if not line_start then
        vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    else
        line_end = line_end or line_start
        vim.api.nvim_buf_set_lines(buf, line_start - 1, line_end, false, lines)
    end

    -- 把光标放到 CURRENT_LINE 这一行, 如果有的话
    -- 获取当前buffer的所有行内容
    local buf_lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
    
    for i, line in ipairs(buf_lines) do
        if line:match("CURRENT_LINE") then
            -- 使用 vim.schedule 确保异步执行
            vim.schedule(function()
                -- 移动光标到这一行
                vim.api.nvim_win_set_cursor(0, {i, 0})
                
                -- 居中显示
                vim.cmd("normal! zz")
                
                -- 获取当前行内容
                local current_line = vim.api.nvim_get_current_line()
                -- 搜索 CURRENT_LINE
                local pos = current_line:find("CURRENT_LINE")
                vim.api.nvim_win_set_cursor(0, { i, pos - 1 })
                vim.cmd("normal! d$")
                vim.cmd("startinsert!")
            end)
            break
        end
    end
    
    return true
end

-- 创建neovim命令
function M.create_commands()
    -- 设置模板变量命令
    -- 这个命令没有用 by rainboy
    -- vim.api.nvim_create_user_command('TemplateSetVar', function(opts)
    --     local key, value = opts.args:match("^(%S+)%s+(.+)$")
    --     if key and value then
    --         M.set_var(key, value)
    --         vim.notify("模板变量已设置: " .. key .. " = " .. value)
    --     else
    --         vim.notify("用法: TemplateSetVar <key> <value>", vim.log.levels.ERROR)
    --     end
    -- end, {nargs = "+"})
    
    -- 应用模板命令
    vim.api.nvim_create_user_command('TemplateApply', function(opts)
        local file_path = opts.args
        if file_path == "" then
            -- 如果没有指定文件，尝试使用当前文件
            file_path = vim.api.nvim_buf_get_name(0)
        end
        
        if file_path == "" then
            vim.notify("请指定模板文件路径", vim.log.levels.ERROR)
            return
        end
        
        -- 更新日期变量
        template_vars.date = os.date("%Y-%m-%d %H:%M:%S")
        
        local success = M.apply_template_to_buffer(file_path)
        if success then
            vim.notify("模板应用成功: " .. file_path)
        end
    end, {nargs = "?"})
    
    -- 显示当前模板变量
    vim.api.nvim_create_user_command('TemplateVars', function()
        local vars_text = "当前模板变量:\n"
        for k, v in pairs(template_vars) do
            vars_text = vars_text .. "  " .. k .. " = " .. tostring(v) .. "\n"
        end
        vim.notify(vars_text)
    end, {})
end

return M