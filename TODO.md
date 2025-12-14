- [x] menu 样式
    - menu 样式
    - menu rbook/index.js 获得menu json,不能使用原来的chapter,因为我们不知道哪些是有文章，哪些是没文章的
    - 参考tmp/book menu 下面的 menu 样式
    - fluent-mdl2:page-list, fluent-mdl2:page
- [x] 使用heti 作为我的markdown 主要排版 https://github.com/sivan/heti
  - [x] 去除markdown.css
  - [x] 引入heiti
  - [x] 定义自己的markdown.css 弥补heiti的不足

- [ ] h2 h3 ,都要有样式 h2 的样式是一个带背景的条状, h3 下划线 ,目标是尽可能的让读者可以很清晰的分辨文章的部分

- fence 增加一个fold 标记 ,这样就可以让 fence code 可以折叠

```js fold
```

增加高亮标记,高亮对应的行

```js [1-2]
```

@include-code 函数拓充,可以有fold,hightline参数


- [ ] code-tab 的样式完 善

- [ ] problems third-part的创建
  - [ ] link-map ,mind-map ?
  - [ ] 简单的题目filter, 根据OJ, 标题的名字
  - [ ] 题目的id转换
    - [ ] /oj-name/id
  - [ ] Problems类,管理problem
  - [ ] markdown 语法 id转换连接 `@problem(id)


## problem-list


- [x] 按时间排序(为高到低)
- [ ] 按标签,OJ 筛选
- [ ] 增加 book 字段,记录用到的rbook的哪些知识
- [ ] 增加 pre 字段,记录前置的题目
- [ ] 设计 dag 导航图 ,记录题目的网络