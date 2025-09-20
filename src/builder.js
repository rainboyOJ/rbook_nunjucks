const fs = require('fs');
const path = require('path');
const yaml = require('js-yaml');
const nunjucks = require('nunjucks');
const { parseMarkdownFile } = require('./parser');

/**
 * 加载配置文件
 * @param {string} configPath - 配置文件路径
 * @returns {Object} - 配置对象
 */
function loadConfig(configPath = 'book.yaml') {
  if (!fs.existsSync(configPath)) {
    throw new Error(`配置文件不存在: ${configPath}`);
  }
  
  const content = fs.readFileSync(configPath, 'utf8');
  return yaml.load(content);
}

/**
 * 渲染模板
 * @param {string} templateName - 模板名称
 * @param {Object} context - 模板上下文数据
 * @returns {string} - 渲染后的HTML
 */
function renderTemplate(templateName, context) {
  const templatePath = path.join('theme', templateName);
  if (!fs.existsSync(templatePath)) {
    throw new Error(`模板文件不存在: ${templatePath}`);
  }
  
  // 配置nunjucks环境
  const env = nunjucks.configure('theme', {
    autoescape: true,
    watch: false
  });
  
  return env.render(templateName, context);
}

/**
 * 构建导航数据
 * @param {Object} config - 配置对象
 * @returns {Array} - 导航数组
 */
function buildNavigation(config) {
  const nav = [];
  
  // 首页
  nav.push({ title: '首页', path: '/', type: 'index' });
  
  // 关于页面（如果存在）
  if (fs.existsSync('book/about.md')) {
    nav.push({ title: '关于', path: '/about.html', type: 'page' });
  }
  
  // 章节导航
  if (config.chapters) {
    config.chapters.forEach(chapter => {
      nav.push({
        title: chapter.title,
        path: `/${chapter.path}/`,
        type: 'chapter'
      });
    });
  }
  
  return nav;
}

/**
 * 处理单个文件
 * @param {string} filePath - 文件路径
 * @param {string} templateType - 模板类型 (index, page, chapter)
 * @param {Object} config - 配置对象
 * @returns {string} - 渲染后的HTML
 */
function processFile(filePath, templateType = 'chapter', config) {
  const { data, html } = parseMarkdownFile(filePath);
  
  // 根据页面类型选择模板
  const templateMap = {
    'index': 'index.njk',
    'page': 'page.njk',
    'chapter': 'chapter.njk'
  };
  
  // 构建导航数据
  const nav = buildNavigation(config);
  
  // 渲染最终页面
  return renderTemplate(templateMap[templateType], {
    site: config,
    page: {
      title: data.title || '无标题',
      content: html,
      type: templateType,
      path: filePath
    },
    nav: nav
  });
}

/**
 * 构建所有文件
 */
function build() {
  // 确保dist目录存在
  if (!fs.existsSync('dist')) {
    fs.mkdirSync('dist', { recursive: true });
  }
  
  // 加载配置
  const config = loadConfig();
  
  // 构建首页
  if (fs.existsSync('book/index.md')) {
    const indexHtml = processFile('book/index.md', 'index', config);
    fs.writeFileSync('dist/index.html', indexHtml);
    console.log('✓ 首页构建完成');
  }
  
  // 构建关于页面（如果存在）
  if (fs.existsSync('book/about.md')) {
    const aboutHtml = processFile('book/about.md', 'page', config);
    fs.writeFileSync('dist/about.html', aboutHtml);
    console.log('✓ 关于页面构建完成');
  }
  
  // 构建章节页面
  if (config.chapters) {
    config.chapters.forEach(chapter => {
      const chapterPath = `book/${chapter.path}/index.md`;
      if (fs.existsSync(chapterPath)) {
        // 确保章节目录存在
        const distChapterPath = `dist/${chapter.path}`;
        if (!fs.existsSync(distChapterPath)) {
          fs.mkdirSync(distChapterPath, { recursive: true });
        }
        
        const html = processFile(chapterPath, 'chapter', config);
        fs.writeFileSync(`${distChapterPath}/index.html`, html);
        console.log(`✓ 章节构建完成: ${chapter.title}`);
      } else {
        console.warn(`⚠ 章节文件不存在: ${chapterPath}`);
      }
    });
  }
  
  // 复制主题资源
  if (fs.existsSync('theme/assets')) {
    const distAssetsPath = 'dist/assets';
    if (!fs.existsSync(distAssetsPath)) {
      fs.mkdirSync(distAssetsPath, { recursive: true });
    }
    
    // 复制CSS文件
    if (fs.existsSync('theme/assets/css')) {
      const distCssPath = 'dist/assets/css';
      if (!fs.existsSync(distCssPath)) {
        fs.mkdirSync(distCssPath, { recursive: true });
      }
      
      const cssFiles = fs.readdirSync('theme/assets/css');
      cssFiles.forEach(file => {
        if (file.endsWith('.css')) {
          fs.copyFileSync(
            path.join('theme/assets/css', file),
            path.join(distCssPath, file)
          );
          console.log(`✓ CSS文件复制完成: ${file}`);
        }
      });
    }
    
    // 复制JS文件
    if (fs.existsSync('theme/assets/js')) {
      const distJsPath = 'dist/assets/js';
      if (!fs.existsSync(distJsPath)) {
        fs.mkdirSync(distJsPath, { recursive: true });
      }
      
      const jsFiles = fs.readdirSync('theme/assets/js');
      jsFiles.forEach(file => {
        if (file.endsWith('.js')) {
          fs.copyFileSync(
            path.join('theme/assets/js', file),
            path.join(distJsPath, file)
          );
          console.log(`✓ JS文件复制完成: ${file}`);
        }
      });
    }
  }
}

module.exports = {
  loadConfig,
  renderTemplate,
  buildNavigation,
  processFile,
  build
};