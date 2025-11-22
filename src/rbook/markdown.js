import * as matter from 'gray-matter';
// import markdownit from './markdownit.js';
import markdownit from './markdown-it/index.js';
import fs from 'fs';

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
            this.html_content = this.toHTML(result.content);
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
            // this.front_matter = result.data;
            // this.md_content = result.content;
            // this.html_content = this.toHTML(result.content);
            // return {
            //     content: result.content,j
            //     data: this.front_matter
            // };
            return result;
        } catch (error) {
            throw new Error(`解析Markdown失败: ${error.message}`);
        }
    }

    /**
     * 将Markdown转换为HTML
     * @param {string} md_content - Markdown内容
     * @returns {string} - HTML内容
     */
    toHTML(md_content) {
        return markdownit.render(md_content, { filePath: this.md_path });
    }

    toJSON() {
        return {
            front_matter: this.front_matter,
            md_content: this.md_content,
            html_content: this.html_content
        };
    }
}

export default Markdown;