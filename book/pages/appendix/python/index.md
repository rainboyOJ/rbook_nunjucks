---
title: 有用的python
date: 2026-06-16 00:00
toc: true
---

## 说明

比赛可以使用python3,学一下python3,很有用的.

- 当成计算器来使用
  - 计算排列组合
- 快速代码
  - itertools 库,排序,组合


## 计算器

```
+ - * / // ** %
```


```python
import math
# 排列
math.perm(5, 2)
# 组合
math.comb(5, 2)
```



## 引用


- [排列組合的計算 – 自然科學和數學計算 學習分享](https://hcppub.wordpress.com/2020/11/04/combination/)

## 常用场景

Python 在算法学习中适合做辅助工具：

- 用 `math.comb`、`math.perm` 快速核对组合数。
- 用 `itertools` 枚举小规模排列、组合，帮助观察规律。
- 写暴力程序生成小数据，用来和 C++ 正解对拍。

比赛提交前仍然要确认题目是否允许 Python，以及数据范围是否适合 Python。
