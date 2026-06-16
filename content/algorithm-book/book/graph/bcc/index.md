---
id: "bcc"
title: "点双连通分量"
date: 2025-12-29 14:45
toc: true
tags: ["图论"]
categories: ["bcc"]
code_template:
  - title: v-bcc 点双连通分量
    desc: "点双连通分量"
    tags: ["无向图连通性"]
    code: /code/graph/v-bcc.cpp
  - title: e-bcc 边双连通分量
    desc: "边双连通分量"
    tags: ["无向图连通性"]
    code: /code/graph/e-bcc.cpp
---

[[TOC]]



为什么不和`cut-node` 一样判断 root ?

思考下面的图

```
    2
  /   \
 /     \
1 ----- 3
```

求 **点双连通分量 (v-BCC)** 和求 **强连通分量 (SCC)** 或 **边双连通分量 (e-BCC)** 在栈操作上的**最核心区别**。

**核心结论：** **绝对不能把割点 `u` 出栈**，因为一个割点 `u` 可能同时属于多个点双连通分量。它必须留在栈里（或者说留在递归结构中），作为后续其他子树（其他 BCC）的“连接点”。


@include_md("./模拟练习v-bcc.md");

## 模板代码

@include-code(/code/graph/v-bcc.cpp, cpp)


## 边双连通分量

@include-code(/code/graph/e-bcc.cpp, cpp)