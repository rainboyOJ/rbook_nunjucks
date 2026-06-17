---
id: "heap-legacy"
title: "堆旧入口"
date: 2026-06-16 00:00
toc: true
tags: ["数据结构", "堆", "归档"]
categories: ["数据结构"]
---

[[TOC]]

## 归档说明

这一页是旧目录 `data-structure/heap` 下的堆入口。正式教程已经迁移到：

[堆](../../data_structure/heap/index.md)

正式模板由正式教程维护：`/code/data-struture/heap堆.cpp`。

本页不再维护正文和模板引用，避免 `data-structure` 与 `data_structure` 两套目录重复。

## 旧页要点

二叉堆用于动态维护集合极值。小根堆的核心操作是：

- 插入元素后向上调整；
- 删除堆顶后用末尾元素补到根，再向下调整；
- 每次调整只沿父子链移动，复杂度为 $O(\log n)$。
