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