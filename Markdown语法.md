## tab fence

来自 [markdown-it-codetabs](https://github.com/cncws/markdown-it-codetabs/blob/main/README.md)


~~~md
```js [g1:JavaScript]
console.log("hello");
```

```py [g1:Python3]
print("hello")
```
~~~

## 伪代码 markdown-it-pseudocodejs

- 来自: https://github.com/samuelhorwitz/markdown-it-pseudocodejs
- 语法文档 : https://github.com/SaswatPadhi/pseudocode.js
- 我写的在线预览工具:https://pseudocode.roj.ac.cn/


使用

```
::: pseudocode
\begin{algorithm}
\caption{Hello World}
\begin{algorithmic}
\FUNCTION{HelloWorld}{}
		\RETURN \CALL{Print}{``Hello, world''}
\ENDFUNCTION
\end{algorithmic}
\end{algorithm}
::: 
```

## 扩展语法说明

本文档说明了 rbook 支持的扩展 Markdown 语法。

### Front Matter

- **描述:** 为 rbook 文章快速生成 Front Matter 元数据。
- **示例:**
  ```markdown
  ---
  title: "文件名"
  date: 2025-11-24 15:30
  toc: true
  tags: [""]
  categories: [""]
  ---

  [[TOC]]

  ```

### 代码相关

#### 引入代码文件

- **描述:** 引入一个完整的代码文件。
- **示例:**
  ```
  @include-code(./code/1.cpp, cpp)
  ```

#### rbook 代码块

- **描述:** 创建一个带有可选行号和高亮行的代码块。
- **示例:**
  ```markdown
  ```cpp,{linenos=table,hl_lines=[]}
  // code here
  ```
  ```

### 提示框 (Admonition)

使用 `!!!` 语法创建不同类型的提示框。

- **通用提示框:**
  - **描述:** 创建一个通用的提示框，类型可选。
  - **类型:** `note`, `abstract`, `info`, `tip`, `success`, `question`, `warning`, `failure`, `danger`, `bug`, `example`, `quote`
  - **示例:**
    ```markdown
    !!! note "Admonition Title"
    Admonition content here
    !!!
    ```

- **特定类型提示框:**
  - **描述:** 快速创建特定类型的提示框。
  - **示例 (`rbook-info`):**
    ```markdown
    !!! info "信息标题"
    信息内容 here
    !!!
    ```

- **数学专用提示框:**
  - **描述:** 创建数学相关的提示框。
  - **类型:** `definition`, `theorem`, `corollary`, `lemma`, `proof`, `exercise`, `problem`
  - **示例 (`rbook-definition`):**
    ```markdown
    !!! definition "定义标题"
    定义内容 here
    !!!
    ```

### 数学公式 (KaTeX)

- **描述:** 快速插入行内或块级数学公式。
- **示例:**
  - 行内: `$math expression$`
  - 块级: `$$math expression$$`

- **多行块级公式:**
  - **描述:** 创建多行块级数学公式。
  - **示例:**
    ```
    $$
    multi-line
    math expression
    here
    $$
    ```

### 其他

#### 折叠内容

- **描述:** 创建一个可折叠的内容区域。
- **示例:**
  ```
  {{% details title="Title" open=true %}}
  ## Markdown content
  Lorem markdownum insigne...}
  {{% /details %}}
  ```

#### 链接或图片

- **描述:** 快速插入 Markdown 格式的链接或图片。
- **示例:**
  - 图片: `![alt text](/images/filename.png)`
  - 链接: `[alt text](https://example.com)`

#### 表格

- **描述:** 创建一个 Markdown 表格。
- **示例:**
  ```markdown
  | Header 1 | Header 2 | Header 3 |
  |---|---|---|
  | Cell 1  | Cell 2  | Cell 3  |
  | Cell 4  | Cell 5  | Cell 6  |
  ```