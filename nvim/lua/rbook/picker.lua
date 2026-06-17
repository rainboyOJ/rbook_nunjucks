local actions = require("rbook.actions")
local catalog = require("rbook.catalog")
local deps = require("rbook.deps")
local paths = require("rbook.paths")

local M = {}

local function text_for_template(item)
  -- text 是 snacks 的模糊搜索源；把标题、描述、tag、导航状态都放进去。
  local tags = table.concat(item.tags or {}, " ")
  local category = table.concat(item.categories or {}, "/")
  local nav = item.in_nav and "" or " 未加入导航"
  return string.format(
    "[%s] %s %s %s%s",
    category ~= "" and category or "模板",
    item.title or "",
    item.desc or "",
    tags,
    nav
  )
end

local function format_item(item)
  -- format 只负责显示，不参与搜索。这里保持信息密度高，适合写题时快速扫列表。
  local ret = {}
  local category = table.concat(item.categories or {}, "/")
  ret[#ret + 1] = { "[" .. (category ~= "" and category or item.source) .. "] ", "Comment" }
  ret[#ret + 1] = { item.title or "未命名", "Normal" }
  if item.desc and item.desc ~= "" then
    ret[#ret + 1] = { "  " .. item.desc, "Comment" }
  end
  if item.in_nav == false then
    ret[#ret + 1] = { "  未加入导航", "WarningMsg" }
  end
  return ret
end

local function pick(items, title)
  -- RbookCode 和 RbookCodeFiles 共用 picker 行为：
  -- Enter 插入，C-o 打开代码，C-a 打开文章，C-y 复制，C-f 带折叠标记插入。
  local snacks = deps.snacks()
  if not snacks then
    return
  end

  snacks.picker.pick({
    title = title,
    items = items,
    format = format_item,
    preview = "file",
    confirm = function(picker, item)
      picker:close()
      if item and item.is_markdown then
        actions.open_code(item)
      else
        actions.insert_code(item)
      end
    end,
    win = {
      input = {
        keys = {
          ["<C-y>"] = { "copy_code", mode = { "i", "n" } },
          ["<C-o>"] = { "open_code", mode = { "i", "n" } },
          ["<C-a>"] = { "open_article", mode = { "i", "n" } },
          ["<C-f>"] = { "insert_with_fold", mode = { "i", "n" } },
          ["<C-r>"] = { "refresh_rbook", mode = { "i", "n" } },
        },
      },
    },
    actions = {
      copy_code = function(picker, item)
        picker:close()
        actions.copy_code(item)
      end,
      open_code = function(picker, item)
        picker:close()
        actions.open_code(item)
      end,
      open_article = function(picker, item)
        picker:close()
        actions.open_article(item)
      end,
      insert_with_fold = function(picker, item)
        picker:close()
        actions.insert_code(item, { fold_markers = true })
      end,
      refresh_rbook = function(picker)
        catalog.refresh()
        picker:close()
        vim.schedule(function()
          M.code()
        end)
      end,
    },
  })
end

function M.code()
  -- 主入口：只展示文章声明过的正式 code_template。
  local data = catalog.get()
  if not data then
    return
  end

  local items = {}
  for _, item in ipairs(data.templates) do
    local copy = vim.deepcopy(item)
    copy.text = text_for_template(item)
    copy.file = item.code_path
    items[#items + 1] = copy
  end
  pick(items, "Rbook Code Templates")
end

function M.code_files()
  -- 兜底入口：直接浏览 book/code 全部允许类型文件。
  local data = catalog.get()
  if not data then
    return
  end

  local items = {}
  for _, item in ipairs(data.code_files) do
    local copy = vim.deepcopy(item)
    copy.text = item.title .. " " .. item.desc
    copy.file = item.code_path
    items[#items + 1] = copy
  end
  pick(items, "Rbook Code Files")
end

function M.articles()
  -- 阅读/维护文章时使用，不插入代码，只打开源 Markdown。
  local data = catalog.get()
  if not data then
    return
  end

  local snacks = deps.snacks()
  if not snacks then
    return
  end

  local items = {}
  for _, article in ipairs(data.articles) do
    local copy = vim.deepcopy(article)
    copy.text = (article.title or "") .. " " .. (article.route or "")
    copy.file = article.path
    items[#items + 1] = copy
  end

  snacks.picker.pick({
    title = "Rbook Articles",
    items = items,
    preview = "file",
    confirm = function(picker, item)
      picker:close()
      actions.open_article(item)
    end,
  })
end

return M
