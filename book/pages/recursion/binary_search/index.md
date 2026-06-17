---
id: "binary-search-recursion-archive"
title: "二分查找（旧路径归档）"
date: 2026-06-16 00:00
toc: true
tags: ["二分查找", "递归", "归档"]
categories: ["基础算法"]
---

[[TOC]]

## 说明

本文是旧目录 `recursion/binary_search` 的归档入口。

正式教程已经迁移到：[二分查找](../../base/binary_search/index.md)。

可复用模板已经归位到正式教程：`/code/base/binary_search.cpp`。

## 旧文要点

二分查找解决的是在单调序列或单调答案空间中定位边界的问题。

典型模型是：找到第一个满足 `check(pos)` 的位置。若中点 `mid` 已经满足条件，答案在左半边；否则答案在右半边。

旧文中的暴力扫描与二分模板不再单独维护，统一参考正式教程。
