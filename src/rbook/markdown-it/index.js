import Shiki from 'markdown-it-shiki'; // highlight code
import Katex from 'katex'; // render math
import TexMath from 'markdown-it-texmath'; // render math
import implicitFigures from 'markdown-it-image-figures'; // 给图片添加 标题
import LinkAttributes from 'markdown-it-link-attributes'; // 给链接添加属性

import highlight from './lib/highlight.js';
import twemoji from 'twemoji';
import mdItContainer from 'markdown-it-container';
import uslug from 'uslug';
import admonition  from './lib/markdown-it-admonition.js';
import includeCode from './lib/include-code.js'; // 引入 include-code 插件
import mdLink2Url from './lib/md-link2url.js'; // 引入 md-link2url 插件

import { fileURLToPath } from 'url';
import path, { dirname, resolve } from 'path';

// 获取当前模块的目录路径
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
// 计算项目根目录 (当前文件向上三级)
const project_root = resolve(__dirname, '../../../');
const project_book_root = resolve(__dirname, '../../../book');

import MarkdownIt from 'markdown-it';


var md = MarkdownIt({
    html: true,
    linkify: true,
    typographer: true,
    // highlight:highlight
})

md.use(includeCode,{baseDir: project_root}) // 使用 include-code 插件

md.use(admonition)

md.use(mdLink2Url, {
    baseDir: project_book_root,
    baseUrl: '/'
})

md.use(TexMath, {
    engine: Katex,
    delimiters: ['dollars', 'beg_end', 'julia'],
    // katexOptions: { macros: { '\\R': '\\mathbb{R}' },strict:'error',throwOnError:true}
    katexOptions: {
        macros: { '\\R': '\\mathbb{R}' }, strict: function (errorCode, errMsg, token) {
            console.error('--->', errorCode, errMsg)
            console.log(token)
            return errMsg
        }
    }
})

// 不能使用
// md.use(LinkAttributes, {
//     matcher: (link) => /^https?:\/\//.test(link),
//     attrs: {
//         target: '_blank',
//         rel: 'noopener',
//     },
// })


// md.use( require("./lib/preWrapper"))
import lineNumber from "./lib/lineNumber.js";
import emoji from "markdown-it-emoji";
import taskCheckbox from "markdown-it-task-checkbox";
import defaultContainer from "./lib/container/default.js";
import foldContainer from "./lib/container/fold.js";
import classContainer from "./lib/container/class.js";
import blackboardContainer from "./lib/container/blackboard.js";
import iframe from "markdown-it-iframe";
import vizGallery from "./lib/viz-gallery.js";
import imsize from "markdown-it-imsize";
import insDel from "markdown-it-ins-del";
import inlineComments from "markdown-it-inline-comments";
import sup from "markdown-it-sup";
import sub from "markdown-it-sub";
import mark from "markdown-it-mark";
import abbr from "markdown-it-abbr";
import anchor from "markdown-it-anchor";
import tocDoneRight from "markdown-it-toc-done-right";
import tocAnchorExtent from './lib/tocAnchorExtent.js';
import codetabs from "markdown-it-codetabs";

md.use(lineNumber)
    .use(emoji)
    // .use( require("markdown-it-multimd-table") ,{
    //           multiline:  true,
    //           rowspan:    false,
    //           headerless: true,
    // })
    // .use( require("markdown-it-kbd") )
    .use(taskCheckbox)
    .use(mdItContainer, ...defaultContainer)
    .use(mdItContainer, ...foldContainer)
    .use(mdItContainer, ...classContainer)
    .use(mdItContainer, ...blackboardContainer)
    .use(iframe)
    .use(vizGallery)
    //.use( require('./lib/bilibili_frame.js'))
    //.use( require('./lib/heading.js'))
    .use(imsize)
    .use(insDel)
    .use(inlineComments)
    .use(sup)
    .use(sub)
    .use(mark)
    .use(abbr)
    .use(anchor, { level: [2, 3], permalink: false, permalinkBefore: true, permalinkSymbol: '§' })
    .use(tocDoneRight, { level: 2 })
    .use(implicitFigures, { figcaption: true })
    .use(tocAnchorExtent)
    .use(codetabs)

md.renderer.rules.emoji = function (token, idx) {
    return twemoji.parse(token[idx].content)
}

// 参数
// config
// mdit.env md渲染的env值
function render(md_raw, config = {}) {
    // config.mdit 渲染的配置,具体参考 markdown-it 文档
    const env = config.mdit || {};
    if (config.filePath) {
        env.filePath = config.filePath;
    }
    return md.render(md_raw, env)
}


export {
    render, //渲染函数
    md // markdown对象
}
export default md;