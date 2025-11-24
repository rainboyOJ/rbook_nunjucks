# 通用文件包含语法指南

## 新的include语法

现在支持通用的文件包含语法，无需为每个文件创建特定的命令！

### 语法格式

```cpp
{{include "path/to/file.cpp"}}   // 双引号
{{include 'path/to/file.cpp'}}   // 单引号
```

### 路径支持

#### 1. 项目相对路径
```cpp
{{include "code/utils/quick_io.cpp"}}
{{include "code/template/template.cpp"}}
{{include "lua/rbook/init.lua"}}
```

#### 2. 绝对路径
```cpp
{{include "/home/user/myproject/header.h"}}
{{include "/Users/rainboy/mycode/rbook_nunjucks/code/utils/quick_io.cpp"}}
```

#### 3. 相对路径（从当前工作目录）
```cpp
{{include "./utils/helper.cpp"}}
{{include "../common/header.h"}}
```

## 使用示例

### 示例1: 包含快速IO
```cpp
//Author by {{author}}({{github}})
//date: {{date}}

#include <cstdio>
using namespace std;

{{include "code/utils/quick_io.cpp"}}

int main() {
    int n;
    read(n);  // 使用包含进来的快速IO函数
    write(n);
    return 0;
}
```

### 示例2: 包含多个文件
```cpp
{{include "code/utils/quick_io.cpp"}}
{{include "code/math/myceil.cpp"}}
{{include "code/graph/linklist_mini.cpp"}}
```

### 示例3: 包含头文件
```cpp
{{include "include/myheader.h"}}
{{include "template/tmpl.hpp"}}
```

## 功能特性

### 自动处理
- ✅ 自动读取文件内容
- ✅ 添加注释标记（开始/结束）
- ✅ 移除重复的#include语句
- ✅ 支持各种文件类型

### 错误处理
- 如果文件不存在，会插入错误注释
- 显示查找路径，方便调试
- 不会中断模板处理过程

### 输出格式
```cpp
// code/utils/quick_io.cpp 内容开始
[文件内容]
// code/utils/quick_io.cpp 内容结束
```

## 实际应用

### 在模板中使用
```cpp
//oisnip_begin 头文件
#include <iostream>
#include <cstdio>

{{include "code/utils/quick_io.cpp"}}
//oisnip_end
```

### 动态包含
```lua
-- 可以在lua中动态设置要包含的文件
TemplateEngine.set_var("target_file", "code/utils/quick_io.cpp")
```

```cpp
{{include target_file}}  // 动态包含
```

## 优势

1. **通用性**: 一个语法支持所有文件
2. **灵活性**: 支持多种路径格式
3. **可读性**: 语法清晰易懂
4. **可维护性**: 无需为每个文件写特定代码
5. **扩展性**: 易于添加新功能

现在你可以用统一的语法包含任何文件了！