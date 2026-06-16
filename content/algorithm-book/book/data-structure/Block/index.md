---
id: "block"
title: "分块算法"
date: 2025-12-01 09:17
toc: true
tags: ["block"]
categories: [""]
code_template:
  - title: 分块算法
    desc: "暴力区间查询"
    tags: ["分块"]
    code: "/code/data-struture/Block/block.cpp"
---

[[TOC]]

- [[problem: luogu,P3372]]
- [[problem: hdu,5057]]


## 适用条件

- 区间最值
- 区间求和
- 带修改的全局第 K 大
- 核心前提：信息的“局部性”与“可合并性”
- 信息的独立性:只要维护的区间信息不影响其他区间，就可以用分块？


一句话记住分块：

当你发现一个区间问题暴力太慢，而线段树又太难写或者根本维护不了这种复杂信息时，想想分块，它很可能是那个“刚刚好”的解法。