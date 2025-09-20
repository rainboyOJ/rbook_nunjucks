## 用到的技术栈

- markdownit
- nunjucks
- commander
- front-matter
- js-yaml

## 项目结构

```
├── bin
│   └── index.js # 命令行入口 rbook build, rbook serve 等
├── book
│   └── chapter1 
│       └── index.md
├── lib
│   ├── index.js
│   ├── markdown.js # markdown 解析,渲染成html
│   ├── nunjucks.js # nunjucks 模板引擎
│   └── utils.js # 工具函数
├── theme
│   ├── layout
│   │   ├── index.njk
│   │   ├── article.njk
│   │   ├── menu.njk
│   │   └── layout.njk
│   ├── assets
│   │   ├── css
│   │   │   └── index.css
│   │   └── js
│   └── index.js
├── README.md
├── book-config.yaml # 配置文件
├── package.json
└── test
    ├── index.js
    └── test.md
```

book-config.yaml  含有全局变量

```
title: "我的书"
author: "张三"
description: "这是一本关于算法的书"

book:
  - title: "第一章"
    path: "chapter1"
    file: "index.md"
  - title: "第二章"
    - title: "第一节"
      path: "chapter2-1"
      file: "index.md"
    - title: "第二节"
      path: "chapter2-2"
      file: "index.md"
```

markdown 文件 在解析之前, 

1. 会先解析 front-matter, 然后将 front-matter 的内容合并到全局变量中
2. 然后再markdown文件当成 nunjucks 模板, 将全局变量渲染到模板中
3. 然后再解析 markdown
4. 最后将解析后的 markdown 渲染到模板(layout.njk)中

## 核心设计

管道设计

1. `[配置解析器]` : 解析配置文件, 将配置文件中的内容解析成对象, 然后将对象挂载到全局变量中
然后遍历`config.book`数组, 然后对每个`config.book`进行处理
2.  for each config.book,
    1. `[front-matter 解析器]` : 解析 markdown 文件中的 front-matter, 然后将 front-matter 的内容合并到全局变量中
    2. `[markdown 解析器]` : 解析 markdown 文件
        1. 然后再markdown文件当成 nunjucks 模板, 将全局变量渲染到模板中
        2. 将 markdown 文件中的内容解析成 html
    2. `[模板引擎]` : 将 markdown 文件当成 nunjucks 模板, 将全局变量渲染到模板中
    4. `[文件生成器]` : 将渲染后的 html 写入到dist文件中