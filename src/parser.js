const fs = require('fs');
const yaml = require('js-yaml');
const MarkdownIt = require('markdown-it');

// 创建markdown-it实例
const md = new MarkdownIt({
  html: true,
  linkify: true,
  typographer: true
});

/**
 * 解析FrontMatter和Markdown内容
 * @param {string} content - 文件内容
 * @returns {Object} - {data: FrontMatter数据, body: Markdown内容}
 */
function parseFrontMatter(content) {
  const frontMatterRegex = /^---\s*\n([\s\S]*?)\n---\s*\n([\s\S]*)$/;
  const match = content.match(frontMatterRegex);
  
  if (match) {
    try {
      const data = yaml.load(match[1]);
      const body = match[2];
      return { data: data || {}, body };
    } catch (error) {
      throw new Error(`FrontMatter解析失败: ${error.message}`);
    }
  }
  
  // 如果没有FrontMatter，返回空数据和全部内容
  return { data: {}, body: content };
}

/**
 * 将Markdown转换为HTML
 * @param {string} markdown - Markdown内容
 * @returns {string} - HTML内容
 */
function markdownToHtml(markdown) {
  return md.render(markdown);
}

/**
 * 读取并解析Markdown文件
 * @param {string} filePath - 文件路径
 * @returns {Object} - {data: FrontMatter数据, body: Markdown内容, html: HTML内容}
 */
function parseMarkdownFile(filePath) {
  if (!fs.existsSync(filePath)) {
    throw new Error(`文件不存在: ${filePath}`);
  }
  
  const content = fs.readFileSync(filePath, 'utf8');
  const { data, body } = parseFrontMatter(content);
  const html = markdownToHtml(body);
  
  return { data, body, html };
}

module.exports = {
  parseFrontMatter,
  markdownToHtml,
  parseMarkdownFile
};