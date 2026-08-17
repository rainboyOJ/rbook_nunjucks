# 本地参考源清单

每次研究都检查本清单中的四个当前内容入口。路径、正文格式、主要用途和许可证只在这里维护；不要把这些信息复制到其他 skill。

| 资料源 | 仓库路径 | 当前内容入口 | 正文格式 | 主要用途 | 许可证说明 |
|---|---|---|---|---|---|
| OI Wiki | `/home/rainboy/references/competitive-programming/OI-wiki/` | `docs/` | `*.md` | 中文术语、标准定义、知识结构 | 除特别注明和代码外，CC BY-SA 4.0，并附加 SATA |
| cp-algorithms | `/home/rainboy/references/competitive-programming/cp-algorithms/` | `src/` | `*.md` | 形式化推导、实现细节、可测试代码 | CC BY-SA 4.0；具体文件仍以仓库声明为准 |
| OI Beats | `/home/rainboy/references/competitive-programming/oi-beats/` | `docs/` | `*.md` | 教学角度、题型分类、应用补充 | 除代码外，CC BY-SA 4.0，并附加 SATA |
| Competitive Programmer's Handbook | `/home/rainboy/references/competitive-programming/Competitive_Programmers_Handbook/` | `book.tex`、`chapter01.tex` 到 `chapter30.tex` | `*.tex` | 教科书式主题组织、基础直觉、典型小例子和竞赛知识全景 | CC BY-NC-SA 4.0 |

## 特殊入口

- OI Beats 的旧内容入口是 `/home/rainboy/references/competitive-programming/oi-beats/docsOld/`。只有当前 `docs/` 没有直接相关资料时才检索它；不要把新旧版本当作两个独立来源。
- Competitive Programmer's Handbook 先从 `book.tex` 和各章的 `\chapter`、`\section` 标题定位，再检索 `chapter*.tex` 正文。优先读取 LaTeX 源文件；只有源文件缺失或不可读时才考虑 `book.pdf`。
- `list.tex` 是 Handbook 的参考文献表，不因关键词命中就视为算法正文。只有需要核对书中引用关系时才读取。

## 检索约定

- Markdown 来源先用 `rg --files` 定位文件名，再用 `rg -n -i -g '*.md'` 搜索正文。
- Handbook 先用 `rg -n '^\\(chapter|section|subsection)' /home/rainboy/references/competitive-programming/Competitive_Programmers_Handbook/chapter*.tex` 定位章节，再用 `rg -n -i -g 'chapter*.tex' '<关键词>' /home/rainboy/references/competitive-programming/Competitive_Programmers_Handbook/` 搜索英文术语、别名、操作和问题模型。
- 四个仓库都只读。不要拉取、切换分支、修改文件或运行会改变仓库状态的命令。
- 某个入口不存在或不可读时，记录失败的绝对路径并继续其他来源。

许可证信息只用于约束内容使用方式，不代替仓库内的完整许可证文本。研究时只提炼事实、结构与独立验证目标，不复制、近似改写、整段翻译或直接搬运来源代码。
