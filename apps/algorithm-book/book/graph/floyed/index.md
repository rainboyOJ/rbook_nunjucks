---
title: "floyed"
date: 2025-11-03 07:22
toc: true
tags: [""]
categories: [""]
draft: true
---

[[TOC]]


算法来源

集合分类:

- 任意两个的路径组成集合A
- 集合A 按起点终点分类 $A_{i,j}$
- $A_{i,j}$ filter 中间经过最大点为k的最短路 : $A_{k,i,j}$
- $B_{k,i,j} = \bigcup_{s=1}^k A_{s,i,j}$ 中间经过最大点不超过k的最短路 组成的集合
- $B_{k,i,j}$ 与 $B_{k-1,i,j}$ 之间的关系?
  - $B_{k,i,j} = B_{k-1,i,j} \cup A_{k,i,j}$
  - $min(B_{k,i,j}) = min( min(B_{k-1,i,j}) ,A_{k,i,j})$
- 如何求$A_{k,i,j}$?
  - **根据最优问题的子问题是最优的**这个性质
  - $A_{k,i,j} = min(B_{k-1,i,k}) + min(B_{k-1,k,j})$

转移方程


$f(k,i,j)$ 中间经过的最大点不超过k的最短路
$$
f(k,i,j) = min\{f(k-1,i,j) , f(k-1,i,k)  + f(k-1,k,j) \}
$$