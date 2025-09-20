---
title: 第一章 - 算法基础概念
---

# 第一章：算法基础概念

## 1.1 什么是算法？

算法是解决特定问题的一系列明确指令。在计算机科学中，算法是程序的核心，它定义了如何从输入数据得到输出结果。

### 算法的基本特征：

- **输入**：算法有零个或多个输入
- **输出**：算法至少有一个输出
- **明确性**：每个步骤都必须清晰明确
- **有限性**：算法必须在有限的步骤内完成
- **有效性**：每个操作都必须足够基本，能够精确执行

## 1.2 算法的复杂度分析

### 时间复杂度

时间复杂度描述了算法执行时间随输入规模增长的变化趋势。我们通常使用大O符号来表示：

```javascript
// O(1) - 常数时间
function getFirstElement(arr) {
    return arr[0];
}

// O(n) - 线性时间
function findElement(arr, target) {
    for (let i = 0; i < arr.length; i++) {
        if (arr[i] === target) return i;
    }
    return -1;
}

// O(n²) - 平方时间
function bubbleSort(arr) {
    for (let i = 0; i < arr.length; i++) {
        for (let j = 0; j < arr.length - i - 1; j++) {
            if (arr[j] > arr[j + 1]) {
                [arr[j], arr[j + 1]] = [arr[j + 1], arr[j]];
            }
        }
    }
    return arr;
}
```

### 空间复杂度

空间复杂度描述了算法在执行过程中所需的额外存储空间随输入规模增长的变化趋势。

## 1.3 常见数据结构

### 数组（Array）

数组是最基本的数据结构，它是一组相同类型元素的集合。

```javascript
// 数组的基本操作
let arr = [1, 2, 3, 4, 5];

// 访问元素 - O(1)
console.log(arr[2]); // 3

// 插入元素 - O(n)
arr.push(6); // 在末尾插入
arr.unshift(0); // 在开头插入

// 删除元素 - O(n)
arr.pop(); // 删除末尾元素
arr.shift(); // 删除开头元素
```

### 链表（Linked List）

链表是一种线性数据结构，其中的元素在内存中不是连续存储的。

```javascript
// 链表节点定义
class ListNode {
    constructor(val) {
        this.val = val;
        this.next = null;
    }
}

// 创建链表
let head = new ListNode(1);
head.next = new ListNode(2);
head.next.next = new ListNode(3);
```

### 栈（Stack）

栈是一种后进先出（LIFO）的数据结构。

```javascript
class Stack {
    constructor() {
        this.items = [];
    }
    
    push(element) {
        this.items.push(element);
    }
    
    pop() {
        return this.items.pop();
    }
    
    peek() {
        return this.items[this.items.length - 1];
    }
    
    isEmpty() {
        return this.items.length === 0;
    }
}
```

### 队列（Queue）

队列是一种先进先出（FIFO）的数据结构。

```javascript
class Queue {
    constructor() {
        this.items = [];
    }
    
    enqueue(element) {
        this.items.push(element);
    }
    
    dequeue() {
        return this.items.shift();
    }
    
    front() {
        return this.items[0];
    }
    
    isEmpty() {
        return this.items.length === 0;
    }
}
```

## 1.4 基本算法思想

### 递归（Recursion）

递归是一种通过调用自身来解决问题的方法。

```javascript
// 计算阶乘
function factorial(n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

// 斐波那契数列
function fibonacci(n) {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}
```

### 分治法（Divide and Conquer）

分治法是将一个大问题分解成若干个小问题，分别解决后再合并结果。

```javascript
// 归并排序
function mergeSort(arr) {
    if (arr.length <= 1) return arr;
    
    const mid = Math.floor(arr.length / 2);
    const left = mergeSort(arr.slice(0, mid));
    const right = mergeSort(arr.slice(mid));
    
    return merge(left, right);
}

function merge(left, right) {
    const result = [];
    let i = 0, j = 0;
    
    while (i < left.length && j < right.length) {
        if (left[i] < right[j]) {
            result.push(left[i++]);
        } else {
            result.push(right[j++]);
        }
    }
    
    return result.concat(left.slice(i)).concat(right.slice(j));
}
```

## 1.5 小结

本章介绍了算法的基本概念、复杂度分析、常见数据结构和基本算法思想。这些基础知识是后续学习更复杂算法的基石。

### 关键点回顾：

1. 算法必须具有明确性、有限性和有效性
2. 时间复杂度和空间复杂度是衡量算法效率的重要指标
3. 选择合适的数据结构对算法性能至关重要
4. 递归和分治法是解决复杂问题的有力工具

在下一章中，我们将深入探讨更高级的算法主题，包括动态规划和贪心算法等。

---

> **练习建议**：
> - 实现一个链表类，包含插入、删除和查找操作
> - 使用递归实现二分查找算法
> - 比较不同排序算法的时间复杂度