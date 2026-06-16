---
title: "LLDB 个性化配置：.lldbinit 文件编写指南"
date: 2025-10-16 23:21
toc: true
tags: ["LLDB", "调试", "开发工具", "C++"]
categories: ["开发工具"]
---

[[TOC]]

## 什么是 `.lldbinit` 文件？

`.lldbinit` 是 LLDB 的初始化配置文件。每次启动 LLDB 会话时，它都会自动执行该文件中的命令。通过配置 `.lldbinit`，您可以实现命令别名、自动化调试流程、自定义输出格式等，从而极大地提升调试效率。

LLDB 会在以下位置按顺序查找 `.lldbinit` 文件：

1.  **项目级**：当前工作目录。
2.  **用户级**：用户的主目录 (`~/.lldbinit`)。

通常，建议将通用配置放在用户主目录的 `.lldbinit` 文件中。

## 常用配置示例

### 1. 创建命令别名

为冗长的命令创建简短的别名是 `.lldbinit` 最常见的用途。使用 `command alias` 命令可以实现。

**示例：**
```
# .lldbinit

# ===================================
#  命令别名 (Command Aliases)
# ===================================

# 程序执行
command alias c continue
command alias n next
command alias s step
command alias f finish
command alias tu thread until

# 断点管理
command alias b breakpoint
command alias bl breakpoint list
command alias bd breakpoint delete

# 堆栈信息
command alias bt thread backtrace

# 打印变量
command alias p expression
command alias po expression -O --
```

配置完成后，您就可以在 LLDB 中使用 `c` 代替 `continue`，`p my_var` 代替 `expression my_var`。

### 2. 设置自动执行命令 (Stop Hooks)

如果您希望在程序每次暂停时都自动显示某些信息（类似 GDB 的 `display` 功能），可以使用 `target stop-hook`。

**示例：**

```
# .lldbinit

# ===================================
#  自动显示 (Stop Hooks)
# ===================================

# 每次暂停时，自动显示当前行的汇编指令、CPU 寄存器和当前栈帧信息
# target stop-hook add -o "disassemble -p"
# target stop-hook add -o "register read"
# target stop-hook add -o "frame info"

# 如果需要模拟 GDB 的 `display/i $pc`，只显示当前指令
target stop-hook add -o "disassemble --start-address \$pc --count 1"
```

### 3. 自定义调试器行为

通过 `settings set` 命令，您可以调整 LLDB 的默认行为，使其更符合个人习惯。

**示例：**

```
# .lldbinit

# ===================================
#  自定义设置 (Settings)
# ===================================

# 设置在进入 main 函数前停止
settings set target.run-args --stop-at-entry true

# 美化输出，特别是针对 C++ STL 容器
settings set target.prefer-dynamic-value no-dynamic-values
settings set type-summary.cpp.string.show-size true

# 设置反汇编风格为 Intel（默认为 AT&T）
settings set target.x86-disassembly-flavor intel

# 调整源码显示的行数
settings set source.lines-before-cursor 5
settings set source.lines-after-cursor 5
```

### 4. 加载自定义 Python 脚本

LLDB 提供了强大的 Python API，允许您编写复杂的自定义命令和功能。通过 `command script import`，您可以在 LLDB 启动时加载这些脚本。

**示例：**

假设您有一个名为 `my_lldb_utils.py` 的脚本，存放于 `~/.lldb/` 目录下。

```
# .lldbinit

# ===================================
#  加载 Python 脚本
# ===================================

command script import ~/.lldb/my_lldb_utils.py
```

## 完整的 `.lldbinit` 模板

这是一个可以直接使用的 `.lldbinit` 文件模板，整合了上述所有功能。您可以根据自己的需求进行修改。

```
# ~/.lldbinit
# LLDB 初始化配置文件模板

# ===================================
#  命令别名 (Command Aliases)
# ===================================

# 程序执行
command alias c continue
command alias n next
command alias s step
command alias f finish
command alias tu thread until

# 断点管理
command alias b breakpoint
command alias bl breakpoint list
command alias bd breakpoint delete
command alias be breakpoint enable
command alias bdi breakpoint disable

# 堆栈信息
command alias bt thread backtrace

# 打印变量
command alias p expression
command alias po expression -O --

# ===================================
#  自定义设置 (Settings)
# ===================================

# 在进入 main 函数前自动暂停
# settings set target.run-args --stop-at-entry true

# 设置反汇编风格为 Intel
settings set target.x86-disassembly-flavor intel

# 调整源码显示的上下文行数
settings set source.lines-before-cursor 5
settings set source.lines-after-cursor 5

# ===================================
#  自动执行命令 (Stop Hooks)
# ===================================

# 每次暂停时，自动显示当前位置的源码和反汇编指令
# target stop-hook add -o "frame info"
# target stop-hook add -o "disassemble --start-address \$pc --count 1"

# ===================================
#  加载 Python 脚本
# ===================================

# 如果有自定义脚本，取消以下注释并修改路径
# command script import ~/.lldb/my_utils.py

# 启动时打印一条欢迎信息
!echo "LLDB is ready."

```

将此文件保存为 `~/.lldbinit`，下次启动 LLDB 时，这些配置将自动生效，为您提供一个更高效、更顺手的调试环境。
