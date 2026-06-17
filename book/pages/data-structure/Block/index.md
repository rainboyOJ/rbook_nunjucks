---
id: "block-legacy"
title: "分块算法旧入口"
date: 2025-12-01 09:17
toc: true
tags: ["分块", "归档"]
categories: ["数据结构"]
---

[[TOC]]

## 归档说明

这一页是旧目录 `data-structure/Block` 下的分块算法入口。正式教程已经迁移到：

[分块思想](../../data_structure/block_data/decompose/index.md)

正式模板由正式教程维护：`/code/data-struture/Block/range_add_range_sum_mod.cpp`。

本页不再维护正文和模板引用，避免 `data-structure` 与 `data_structure` 两套目录重复。

## 旧页要点

分块适合把区间操作拆成“散块暴力处理 + 整块快速处理”的问题。当线段树难以维护复杂信息，而暴力又太慢时，分块通常是一个实现成本较低的折中方案。
