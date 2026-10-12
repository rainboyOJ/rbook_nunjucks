// markdown-it plugin for generating line numbers.
// It depends on preWrapper plugin.
import ejs from 'ejs';
import fs from 'fs';
import {join} from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';
import Prism from 'prismjs';
import loadLanguages from 'prismjs/components/index.js';
import { splitCodeTemplateMark } from '../../include-code.js';
loadLanguages() //加载所有的语言模块

const __dirname = dirname(fileURLToPath(import.meta.url));
const lineNumber_template = ejs.compile(fs.readFileSync(join(__dirname,"./lineNumber.html"),{encoding:"utf-8"}))

function copy_container_html() {
return `<div class="zeroclipboard-container">
    <div class="clipboard-copy">
      <svg aria-hidden="true" height="16" viewBox="0 0 16 16" version="1.1" width="16" data-view-component="true" class="octicon octicon-copy js-clipboard-copy-icon">
    <path d="M0 6.75C0 5.784.784 5 1.75 5h1.5a.75.75 0 0 1 0 1.5h-1.5a.25.25 0 0 0-.25.25v7.5c0 .138.112.25.25.25h7.5a.25.25 0 0 0 .25-.25v-1.5a.75.75 0 0 1 1.5 0v1.5A1.75 1.75 0 0 1 9.25 16h-7.5A1.75 1.75 0 0 1 0 14.25Z"></path><path d="M5 1.75C5 .784 5.784 0 6.75 0h7.5C15.216 0 16 .784 16 1.75v7.5A1.75 1.75 0 0 1 14.25 11h-7.5A1.75 1.75 0 0 1 5 9.25Zm1.75-.25a.25.25 0 0 0-.25.25v7.5c0 .138.112.25.25.25h7.5a.25.25 0 0 0 .25-.25v-7.5a.25.25 0 0 0-.25-.25Z"></path>
    </svg>
      <svg aria-hidden="true" height="16" viewBox="0 0 16 16" version="1.1" width="16" data-view-component="true" class="octicon octicon-check js-clipboard-check-icon color-fg-success">
    <path d="M13.78 4.22a.75.75 0 0 1 0 1.06l-7.25 7.25a.75.75 0 0 1-1.06 0L2.22 9.28a.751.751 0 0 1 .018-1.042.751.751 0 0 1 1.042-.018L6 10.94l6.72-6.72a.75.75 0 0 1 1.06 0Z"></path>
    </svg>
    </div>
  </div>`
}


const lineNumbers = (rawCode,lang,rawlang) => {
    const lines = String(rawCode).replace(/\r\n?/g, '\n').split('\n')
    while (lines.length > 1 && lines[lines.length - 1].trim() === '') {
        lines.pop()
    }
    const code = Prism.highlight(lines.join('\n'), Prism.languages[lang], lang)
    return lineNumber_template({code:code.split('\n'),lang,rawlang})

}

const lineNumber =  md => {
    const fence = md.renderer.rules.fence
    md.renderer.rules.fence =  (...args)=>{
        const [tokens, idx] = args
        const token = tokens[idx]
        const content = token.content
        // include-code 展开时附带的模板 ID 标记：从 info 里拆掉，
        // 语言识别不受影响，稍后写到代码块容器的 data 属性上。
        const splitInfo = splitCodeTemplateMark(token.info)
        const codeTemplateId = splitInfo.codeTemplateId
        token.info = splitInfo.info
        //mermaid
        let info = token.info.trim()
        if( token.info.trim() === 'mermaid')
        {
            return `<pre class="mermaid">\n${content}</pre>\n`;
        }
        const viz_reg = /^viz($|-\w+)/;
        if( info == "dot" || info == "graphviz" || viz_reg.test(info) ) {
            // return 'viz!!'
            return `<div class="graphviz"><pre class="dot">\n${content}</pre></div>\n`;
        }

        let lang = info.toLowerCase()
        if( lang === 'js')
            lang = 'javascript'
        else if (lang === 'vue' || lang === 'html') {
            lang = 'markup'
        }
        else if (lang === 'md') {
            lang = 'markdown'
        }
        else if (lang === 'ts') {
            lang = 'typescript'
        }
        else if (lang === 'py') {
            lang = 'python'
        }
        else if( lang === 'c++' ||  lang === 'c' || lang==="cpp"){
            lang = 'clike'
        }
        const support_lang = ['python','clike','markup','markdown','bash','sh','haskell','javascript']
        // console.log('lang--->',lang)
        if( support_lang.indexOf(lang) != -1) {
            // console.log('support_lang')
            return withCodeTemplateId(lineNumbers(content,lang,info), codeTemplateId)
        }
        return withCodeTemplateId(fence(...args), codeTemplateId)
    }
}

// 把模板 ID 写到代码块最外层标签（.line-numbers-mode 的 div 或 pre）上，
// 前端据此在复制按钮左侧渲染“查看模板”跳转按钮。
function withCodeTemplateId(html, codeTemplateId) {
    if (!codeTemplateId) return html
    const safe = String(codeTemplateId)
        .replace(/&/g, '&amp;')
        .replace(/"/g, '&quot;')
        .replace(/</g, '&lt;')
    return html.replace(/<(pre|div)\b/, `<$1 data-code-template-id="${safe}"`)
}

export default lineNumber
