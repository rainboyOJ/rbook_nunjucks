---
title: "随机数生成工具"
date: 2026-06-17 00:00
toc: true
tags: ["随机数", "工具"]
categories: ["工具"]
---

[[TOC]]

## 说明

本页记录竞赛和对拍中常用的随机数生成工具，不作为算法教程维护。

模板文件位置：

- `/code/utils/random.cpp`

## mt19937

`mt19937` 是 C++ 标准库中的梅森旋转随机数引擎，适合生成整数、随机数组、随机操作序列等测试数据。

参考文档：

- <https://en.cppreference.com/w/cpp/numeric/random>
- <https://en.cppreference.com/w/cpp/numeric/random/mersenne_twister_engine>
