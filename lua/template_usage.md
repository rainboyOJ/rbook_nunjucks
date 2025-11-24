# Lua模板引擎使用指南

## 快速开始

### 1. 在neovim中加载模板系统

在你的neovim配置中添加：

```lua
-- 加载模板系统
local TemplateIntegration = require("rbook.template_integration")
TemplateIntegration.setup()
TemplateIntegration.setup_keymaps()
```

### 2. 基本使用方法

#### 命令方式
```vim
" 设置模板变量
:TemplateSetVar author "你的名字"
:TemplateSetVar github "https://github.com/yourusername"

" 应用模板文件
:TemplateApply /path/to/template.cpp

" 查看当前模板变量
:TemplateVars
```

#### 快捷键方式
```vim
<leader>tt    " 应用C++模板
<leader>tq    " 应用快速IO模板  
<leader>ts    " 设置模板变量
<leader>ta    " 应用模板文件
<leader>tv    " 显示模板变量
```

### 3. 模板语法

在模板文件中使用以下语法：

```cpp
// 变量替换
//Author by {{author}}({{github}})
//date: {{date}}

// 条件块
{{#if debug}}
#define LOG(args...) cout << args << endl
{{/if}}

// 未定义的变量会保持原样
//Version: {{version}}  // 如果version未定义，保持为{{version}}
```

### 4. 模板示例

#### template.cpp 修改
```cpp
//Author by {{author}}({{github}})
//date: {{date}}

#include <iostream>
using namespace std;

int main() {
    {{#if debug}}
    cout << "Debug mode" << endl;
    {{/if}}
    return 0;
}
```

## 高级用法

### 1. 动态设置变量
```lua
-- 在lua中设置变量
local TemplateEngine = require("rbook.template_engine")
TemplateEngine.set_var("project_name", "myproject")
TemplateEngine.set_var("version", "1.0.0")
```

### 2. 批量设置变量
```lua
TemplateEngine.set_vars({
    author = "Your Name",
    email = "your.email@example.com", 
    company = "Your Company"
})
```

### 3. 自定义模板
创建自己的模板文件，使用`{{variable}}`语法标记需要替换的部分。

## 集成到现有工作流

### 1. 与rbook代码片段系统集成
可以在现有的代码片段选择器中添加模板选项，让用户可以选择是插入代码片段还是应用模板。

### 2. 自动化工作流
```vim
" 创建新的C++文件时自动应用模板
autocmd BufNewFile *.cpp :TemplateApply ~/.config/nvim/templates/cpp_template.cpp
```

## 注意事项

1. **变量命名**: 使用有意义的变量名，避免冲突
2. **文件路径**: 模板文件路径建议使用绝对路径
3. **性能**: 模板解析很快，适合频繁使用
4. **扩展性**: 可以轻松添加新的模板语法和功能

这个简单的模板系统让你能够在neovim中快速定制和管理代码模板，提高开发效率！