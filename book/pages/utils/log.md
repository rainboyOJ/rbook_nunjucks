
---
id: "debug-log"
title: "调试输出工具"
date: 2026-06-16 00:00
toc: true
tags: ["调试", "工具函数"]
categories: ["工具"]
code_template:
  - title: 调试输出
    desc: "竞赛调试时输出变量名和值"
    tags: ["debug", "log"]
    code: /code/utils/log.cpp
---

[[TOC]]

## 用法

`debug(a, b, c)` 会同时输出变量名和变量值，适合本地调试。提交评测前可以删除或用宏开关屏蔽。

## 代码模板

@include-code(/code/utils/log.cpp, cpp)
