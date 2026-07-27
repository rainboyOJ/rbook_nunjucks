# 代码模板目录

`book/code/` 只存放可以脱离文章、在其他题目中复用的代码模板。

- 每个模板源码都必须在 `book/code.yaml` 中登记，并拥有唯一的 `id` 和 `path`。
- 文章通过 front matter 的 `code_template` 引用模板 ID。
- 文章中的一次性示例、暴力对照、测试程序和特定题目解法，应放在对应文章目录的 `code/` 子目录，并使用相对路径 `@include-code` 引用。
- 不要提交 `.out`、`.o`、`.dSYM` 等编译产物。

构建前的 pre-check 会检查 `book/code.yaml` 与本目录中的模板文件是否一一对应。
