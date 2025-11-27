import Path from "path";
import fs from "fs";
import fse from "fs-extra";
import archiver from "archiver";
import { fileURLToPath } from 'url';
import { dirname } from 'path';
// import rbook from "./fake_rbook.js";
import Problem from "../../src/problemlib.js";
import * as matter from 'gray-matter';

class Markdown {
    constructor(md_path = '') {
        this.name = 'rbook';
        this.front_matter = {};
        this.md_content = '';
        this.html_content = '';
        if( md_path && md_path.length > 0 ) {
            this.md_path = md_path; // md文件路径
            let raw_md = fs.readFileSync(md_path, 'utf8');
            let result = this.matter(raw_md);
            this.front_matter = result.data;
            this.md_content = result.content;
        }
    }

    /**
     * 解析FrontMatter和Markdown内容
     * @param {string} md_content - Markdown内容
     * @returns {Object} - { content: markdown, data: frontmatter }
     */
    matter(md_content) {
        try {
            const result = matter.default(md_content);
            return result
        } catch (error) {
            throw new Error(`解析Markdown失败: ${error.message}`);
        }
    }
}


const __dirname = dirname(fileURLToPath(import.meta.url));
//1. 加载数据menu
// import {flatten_menu_json} from "../../src/menu.js";

//2. 加载所有的 template 描述的array
var template_array = []

async function load_data() {
  const ProbEntiy = new Problem();
  template_array = ProbEntiy.problems
  return ProbEntiy.problems
}


export default async function nodejsPlugin() {
    await load_data();
    return {
        name: 'nodejs-loadBook-problem-plugin',
        config() {
            // 在 Vite 构建过程中执行 Node.js 代码
            // const data = fs.readFileSync('yourNodeJsFile.js', 'utf-8');

            // console.log(template_array)
            return {
                define: {
                    // 将计算得到的数据传递给前端
                    // $yourData: JSON.stringify(data)
                    template_array
                }
            };
        }
    };
}
