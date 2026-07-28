---
id: data-structure-persistent-segment-tree
title: persistent-segment-tree
description: 可持久化线段树的实现要点：记录两个 SGT 的同步位置。
tags: ["可持久化线段树", "主席树", "数据结构"]
---

可持久化线段树


只记录两个sgt的同地方


psgt

核心

1. 只更新了一条$logn$的链 https://www.luogu.com.cn/article/oqgnbs3n
2. 对应结点相减的含义

- persistent segment tree ; psgt
- 图片解释了,建立psgt 的过程 [【学习笔记】主席树](https://www.luogu.com.cn/article/oqgnbs3n)


- hdu 4348
- hdu 5919
- [[problem: luogu,P3834]] 
- [[problem: hdu,4417]]