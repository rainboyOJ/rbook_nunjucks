---
title: "表达式求值"
date: 2025-10-06 10:30
draft: false
toc: true
tags: ["数据结构", "栈", "算法"]
categories: ["算法学习"]
---

[[TOC]]

表达式求值是计算机科学中的一个基本问题，它涉及到如何解释和计算一个给定的算术表达式。常见的表达式形式是中缀表达式，即运算符位于操作数之间（例如 `3 + 4`）。然而，计算机处理后缀表达式（逆波兰表示法）更为高效。因此，表达式求值通常分为两步：

1.  将中缀表达式转换为后缀表达式。
2.  对后缀表达式进行求值。

本文将详细介绍这两个步骤的算法和实现。

## 后缀表达式求值

后缀表达式是一种没有括号的算术表达式，其中运算符位于其操作数之后。例如，中缀表达式 `3 + 4` 对应的后缀表达式是 `3 4 +`。

后缀表达式的求值过程非常直观，通常使用一个栈来完成。

### 算法步骤

1.  初始化一个空栈。
2.  从左到右扫描后缀表达式的每一个标记（操作数或运算符）。
3.  如果标记是 **操作数**（数字），则将其压入栈中。
4.  如果标记是 **运算符**，则从栈中弹出两个操作数（注意顺序：先弹出的是右操作数，后弹出的是左操作数）。
5.  执行该运算，并将计算结果压回栈中。
6.  扫描完整个表达式后，栈中应该只剩下一个元素，这个元素就是表达式的最终结果。

### 求值示例

让我们来计算后缀表达式 `1 2 3 * +` 的值，它对应的中缀表达式是 `1 + (2 * 3)`。

| 标记 (Token) | 操作 (Action) | 栈 (Stack) |
| :--- | :--- | :--- |
| `1` | 推入 1 | `[1]` |
| `2` | 推入 2 | `[1, 2]` |
| `3` | 推入 3 | `[1, 2, 3]` |
| `*` | 弹出 3 和 2, 计算 `2 * 3 = 6`, 推入 6 | `[1, 6]` |
| `+` | 弹出 6 和 1, 计算 `1 + 6 = 7`, 推入 7 | `[7]` |

表达式的最终结果是 `7`。

## 中缀表达式转后缀

将人类习惯的中缀表达式转换为计算机易于处理的后缀表达式，是整个求值过程的核心。这个转换算法通常被称为 **调度场算法**（Shunting-yard algorithm），它同样巧妙地利用了栈。

### 算法步骤

我们需要一个栈来临时存储运算符（包括括号），以及一个列表（或队列）来构建最终的后缀表达式。

1.  初始化一个空的操作符栈和一个空的后缀表达式列表。
2.  从左到右扫描中缀表达式的每个标记。
3.  **遇到数字：** 直接将其添加到后缀表达式列表中。
4.  **遇到左括号 `(`：** 将其压入操作符栈。
5.  **遇到右括号 `)`：**
    *   不断从操作符栈中弹出操作符，并添加到后缀表达式列表中，直到栈顶是左括号 `(`。
    *   最后，将这个左括号 `(` 从栈中弹出并丢弃。
6.  **遇到运算符（如 `+`, `-`, `*`, `/`）：**
    *   定义运算符的优先级：`*` 和 `/` 的优先级高于 `+` 和 `-`。
    *   当操作符栈不为空，且栈顶的操作符不是 `(`，并且栈顶操作符的优先级 **大于或等于** 当前扫描到的操作符的优先级时：
        *   持续从栈中弹出操作符，并添加到后缀表达式列表中。
    *   完成上述操作后，将当前扫描到的操作符压入栈中。
7.  **扫描结束：** 中缀表达式的所有标记都处理完毕后，如果操作符栈中仍有剩余的操作符，则依次将它们弹出并添加到后缀表达式列表中。

### 转换示例

让我们转换中缀表达式 `1 + (2 + 3) * 4 - 5`。

| 标记 | 操作 | 后缀表达式 | 操作符栈 |
| :--- | :--- | :--- | :--- |
| `1` | 添加到后缀 | `1` | `[]` |
| `+` | 栈空，推入 `+` | `1` | `[+]` |
| `(` | 推入 `(` | `1` | `[+, (]` |
| `2` | 添加到后缀 | `1 2` | `[+, (]` |
| `+` | 栈顶是 `(`, 优先级最低, 推入 `+` | `1 2` | `[+, (, +]` |
| `3` | 添加到后缀 | `1 2 3` | `[+, (, +]` |
| `)` | 弹出直到 `(`, 添加 `+`, 弹出 `(` | `1 2 3 +` | `[+]` |
| `*` | 栈顶 `+` 优先级低, 推入 `*` | `1 2 3 +` | `[+, *]` |
| `4` | 添加到后缀 | `1 2 3 + 4` | `[+, *]` |
| `-` | 栈顶 `*` 和 `+` 优先级高, 弹出并添加 | `1 2 3 + 4 * +` | `[]` |
| | 弹出操作完成后, 推入 `-` | `1 2 3 + 4 * +` | `[-]` |
| `5` | 添加到后缀 | `1 2 3 + 4 * + 5` | `[-]` |
| *结束* | 弹出栈中所有剩余操作符 | `1 2 3 + 4 * + 5 -` | `[]` |

最终得到的后缀表达式为 `1 2 3 + 4 * + 5 -`。

通过这两个算法的结合，我们就能完整地实现对复杂算术表达式的自动求值。

## 代码示例


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// --- 栈的实现 ---
#define STACK_SIZE 100

// 用于计算后缀表达式的double类型栈
double eval_stack[STACK_SIZE];
int eval_top = -1;

void eval_push(double item) {
    if (eval_top >= STACK_SIZE - 1) {
        printf("Stack Overflow for evaluation.\n");
        exit(1);
    }
    eval_stack[++eval_top] = item;
}

double eval_pop() {
    if (eval_top < 0) {
        printf("Stack Underflow for evaluation.\n");
        exit(1);
    }
    return eval_stack[eval_top--];
}

// 用于中缀转后缀的char类型栈
char op_stack[STACK_SIZE];
int op_top = -1;

void op_push(char item) {
    if (op_top >= STACK_SIZE - 1) {
        printf("Stack Overflow for operator.\n");
        exit(1);
    }
    op_stack[++op_top] = item;
}

char op_pop() {
    if (op_top < 0) {
        printf("Stack Underflow for operator.\n");
        exit(1);
    }
    return op_stack[op_top--];
}

char op_peek() {
    if (op_top < 0) {
        return '\0';
    }
    return op_stack[op_top];
}

int is_op_stack_empty() {
    return op_top == -1;
}

// 获取运算符优先级
int precedence(char op) {
    switch (op) {
        case '+':
        case '-':
            return 1;
        case '*':
        case '/':
            return 2;
        default:
            return 0; // for '('
    }
}

/**
 * 将中缀表达式转换为后缀表达式
 * 注意：这个实现假定操作数和运算符之间用空格分隔，以便解析。
 */
void infixToPostfix(const char* infix, char* postfix) {
    int i = 0, j = 0;
    char token;
    
    while (infix[i] != '\0') {
        token = infix[i];
        
        if (isspace(token)) {
            i++;
            continue;
        }

        if (isdigit(token) || token == '.') {
            // 处理多位数字或小数
            while(isdigit(infix[i]) || infix[i] == '.'){
                postfix[j++] = infix[i++];
            }
            i--; // 回退一个字符，因为外层循环会 i++
            postfix[j++] = ' ';
        } else if (token == '(') {
            op_push(token);
        } else if (token == ')') {
            while (!is_op_stack_empty() && op_peek() != '(') {
                postfix[j++] = op_pop();
                postfix[j++] = ' ';
            }
            op_pop(); // Pop '('
        } else { // 遇到运算符
            while (!is_op_stack_empty() && precedence(op_peek()) >= precedence(token)) {
                postfix[j++] = op_pop();
                postfix[j++] = ' ';
            }
            op_push(token);
        }
        i++;
    }
    
    while (!is_op_stack_empty()) {
        postfix[j++] = op_pop();
        postfix[j++] = ' ';
    }
    
    // 移除末尾多余的空格
    if (j > 0) {
      postfix[j-1] = '\0';
    } else {
      postfix[j] = '\0';
    }
}

/**
 * 对后缀表达式求值
 * 注意：这个实现假定操作数和运算符之间由空格分隔
 */
double evaluatePostfix(const char* postfix) {
    int i = 0;
    char token_str[50];

    while(postfix[i] != '\0'){
        // 跳过空格
        while(isspace(postfix[i])) {
            i++;
        }
        if(postfix[i] == '\0') break;

        if(isdigit(postfix[i]) || (postfix[i] == '-' && isdigit(postfix[i+1]))) {
            int k = 0;
            // 提取完整的数字（包括小数和负数）
            while(!isspace(postfix[i]) && postfix[i] != '\0') {
                token_str[k++] = postfix[i++];
            }
            token_str[k] = '\0';
            eval_push(atof(token_str));
        } else { // 遇到运算符
            double val2 = eval_pop();
            double val1 = eval_pop();
            switch(postfix[i]){
                case '+': eval_push(val1 + val2); break;
                case '-': eval_push(val1 - val2); break;
                case '*': eval_push(val1 * val2); break;
                case '/': eval_push(val1 / val2); break;
            }
            i++;
        }
    }
    return eval_pop();
}

int main() {
    // 示例来自文章: 1 + (2 + 3) * 4 - 5
    // 注意：为了让程序正确解析，我们在括号和运算符周围添加了空格
    const char* infix_expr = "1 + ( 2 + 3 ) * 4 - 5";
    char postfix_expr[256] = {0};
    
    printf("中缀表达式: %s\n", infix_expr);
    
    // 1. 中缀转后缀
    infixToPostfix(infix_expr, postfix_expr);
    printf("转换后的后缀表达式: %s\n", postfix_expr);
    
    // 2. 求值
    double result = evaluatePostfix(postfix_expr);
    printf("计算结果: %f\n", result); // 预期结果: 1 + 5 * 4 - 5 = 1 + 20 - 5 = 16

    printf("\n-----------\n");

    // 另一个示例: 1 + 2 * 3
    const char* infix_expr2 = "1 + 2 * 3";
    char postfix_expr2[256] = {0};
    printf("中缀表达式: %s\n", infix_expr2);
    infixToPostfix(infix_expr2, postfix_expr2);
    printf("转换后的后缀表达式: %s\n", postfix_expr2); // 预期: 1 2 3 * +
    double result2 = evaluatePostfix(postfix_expr2);
    printf("计算结果: %f\n", result2); // 预期: 7

    return 0;
}
```
