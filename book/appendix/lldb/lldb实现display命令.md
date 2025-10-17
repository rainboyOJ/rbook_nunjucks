---
title: "在LLDB中实现GDB的display功能"
date: 2025-10-16 23:19
toc: true
tags: ["LLDB", "GDB", "调试"]
categories: ["开发工具"]
---

[[TOC]]

## 核心功能

GDB 的 `display` 命令可以在程序每次停止时自动打印表达式的值，便于跟踪变量变化。在 LLDB 中，此功能通过 `target stop-hook` 命令实现。在较新版本的 LLDB 中，`display` 也被设为 `target stop-hook` 的一个便捷别名。

## 核心命令: `target stop-hook`

该命令用于注册一个“停止钩子”，它会在程序每次因断点、单步执行等原因暂停时，自动执行预设的 LLDB 命令。

**基本语法:**
```lldb
target stop-hook add --one-liner "<要执行的命令>"
```
*   别名: `ta st a -o "..."`

## 实现方法

### 1. 自动显示变量

在程序每次停止时，自动打印一个或多个变量的值。

```lldb
(lldb) target stop-hook add -o "frame variable <var1> <var2>"

# 如果你的 LLDB 版本支持，也可以直接使用 display 别名
(lldb) display <var1>
```

**示例：**
```lldb
# 每次停止时显示变量 my_variable 的值
(lldb) target stop-hook add -o "frame variable my_variable"

# 同时显示 argc 和 argv
(lldb) target stop-hook add -o "frame variable argc argv"
```

### 2. 条件显示

`target stop-hook` 支持在满足特定条件时才触发，功能比 GDB 的 `display` 更灵活。

*   **仅在特定函数中显示:**
    使用 `--name` (别名 `-n`) 选项指定函数。
    ```lldb
    # 仅在 main 函数中停止时，显示 argc 和 argv
    (lldb) target stop-hook add -n main -o "frame variable argc argv"
    ```

*   **仅在特定类的成员函数中显示:**
    使用 `--classname` (别名 `-c`) 选项指定类名。
    ```lldb
    # 仅在 MyClass 的方法中停止时，显示 *this
    (lldb) target stop-hook add -c MyClass -o "frame variable *this"
    ```

## 钩子管理

| 命令 | 功能 |
| :--- | :--- |
| `target stop-hook list` | 列出所有已设置的停止钩子及其编号 |
| `target stop-hook delete <编号>` | 删除指定编号的停止钩子 |

## GDB vs LLDB 命令速查

| GDB 命令 | LLDB 等效命令 |
| :--- | :--- |
| `display my_var` | `target stop-hook add -o "frame variable my_var"` 或 `display my_var` |
| `display/i $pc` | `target stop-hook add -o "disassemble -p"` |
| `undisplay <num>` | `target stop-hook delete <num>` |
| `info display` | `target stop-hook list` |
