const http = require('http');
const fs = require('fs');
const path = require('path');
const { loadConfig, processFile } = require('./builder');

// 内存缓存
const cache = {
  config: null,
  templates: {},
  assets: {},
  pages: {}
};

/**
 * 获取文件的MIME类型
 * @param {string} filePath - 文件路径
 * @returns {string} - MIME类型
 */
function getMimeType(filePath) {
  const ext = path.extname(filePath).toLowerCase();
  const mimeTypes = {
    '.html': 'text/html',
    '.css': 'text/css',
    '.js': 'application/javascript',
    '.json': 'application/json',
    '.png': 'image/png',
    '.jpg': 'image/jpeg',
    '.jpeg': 'image/jpeg',
    '.gif': 'image/gif',
    '.svg': 'image/svg+xml',
    '.ico': 'image/x-icon'
  };
  return mimeTypes[ext] || 'text/plain';
}

/**
 * 加载配置文件到内存
 */
function loadConfigToMemory() {
  try {
    cache.config = loadConfig();
    console.log('✓ 配置加载到内存');
  } catch (error) {
    console.error('配置加载失败:', error.message);
    throw error;
  }
}

/**
 * 加载主题资源到内存
 */
function loadAssetsToMemory() {
  // 加载CSS文件
  const cssPath = 'theme/assets/css/style.css';
  if (fs.existsSync(cssPath)) {
    cache.assets['/assets/css/style.css'] = fs.readFileSync(cssPath, 'utf8');
    console.log('✓ CSS资源加载到内存');
  }

  // 加载JS文件
  const jsPath = 'theme/assets/js/main.js';
  if (fs.existsSync(jsPath)) {
    cache.assets['/assets/js/main.js'] = fs.readFileSync(jsPath, 'utf8');
    console.log('✓ JS资源加载到内存');
  }
}

/**
 * 根据URL路径获取对应的Markdown文件路径
 * @param {string} url - 请求的URL
 * @returns {Object} - { filePath, templateType }
 */
function getMarkdownFileFromUrl(url) {
  if (url === '/' || url === '/index.html') {
    return { filePath: 'book/index.md', templateType: 'index' };
  } else if (url === '/about.html') {
    return { filePath: 'book/about.md', templateType: 'page' };
  } else if (url.endsWith('/')) {
    // 章节页面
    const chapterPath = url.slice(1, -1); // 移除首尾的斜杠
    return { 
      filePath: `book/${chapterPath}/index.md`, 
      templateType: 'chapter' 
    };
  }
  return null;
}

/**
 * 渲染页面到内存
 * @param {string} filePath - Markdown文件路径
 * @param {string} templateType - 模板类型
 * @returns {string} - 渲染后的HTML
 */
function renderPageToMemory(filePath, templateType) {
  try {
    if (!fs.existsSync(filePath)) {
      throw new Error(`文件不存在: ${filePath}`);
    }

    const html = processFile(filePath, templateType, cache.config);
    cache.pages[filePath] = {
      html,
      timestamp: Date.now()
    };
    
    return html;
  } catch (error) {
    console.error(`页面渲染失败: ${filePath}`, error.message);
    throw error;
  }
}

/**
 * 处理HTTP请求
 * @param {http.IncomingMessage} req - 请求对象
 * @param {http.ServerResponse} res - 响应对象
 */
function handleRequest(req, res) {
  const url = req.url;
  
  // 处理静态资源请求
  if (url.startsWith('/assets/')) {
    if (cache.assets[url]) {
      const mimeType = getMimeType(url);
      res.writeHead(200, { 
        'Content-Type': mimeType,
        'Cache-Control': 'no-cache'
      });
      res.end(cache.assets[url]);
    } else {
      res.writeHead(404, { 'Content-Type': 'text/html' });
      res.end('<h1>404 Not Found</h1><p>资源不存在</p>');
    }
    return;
  }

  // 处理页面请求
  const pageInfo = getMarkdownFileFromUrl(url);
  
  if (!pageInfo) {
    res.writeHead(404, { 'Content-Type': 'text/html' });
    res.end('<h1>404 Not Found</h1><p>页面不存在</p>');
    return;
  }

  try {
    // 懒渲染：只在请求时渲染
    const html = renderPageToMemory(pageInfo.filePath, pageInfo.templateType);
    
    res.writeHead(200, { 
      'Content-Type': 'text/html',
      'Cache-Control': 'no-cache'
    });
    res.end(html);
    
    console.log(`✓ 懒渲染完成: ${url} -> ${pageInfo.filePath}`);
  } catch (error) {
    res.writeHead(500, { 'Content-Type': 'text/html' });
    res.end(`<h1>500 Internal Server Error</h1><p>${error.message}</p>`);
  }
}

/**
 * 监听文件变化
 */
function setupFileWatchers() {
  // 监听Markdown文件变化
  if (fs.existsSync('book')) {
    fs.watch('book', { recursive: true }, (eventType, filename) => {
      if (filename && filename.endsWith('.md') && !filename.startsWith('.')) {
        const filePath = path.join('book', filename);
        console.log(`Markdown文件变化: ${filename}`);
        
        // 清除缓存，下次请求时重新渲染
        delete cache.pages[filePath];
        console.log(`✓ 缓存已清除: ${filePath}`);
      }
    });
  }

  // 监听主题文件变化
  if (fs.existsSync('theme')) {
    fs.watch('theme', { recursive: true }, (eventType, filename) => {
      if (filename && !filename.startsWith('.')) {
        console.log(`主题文件变化: ${filename}`);
        
        // 重新加载资源并清除所有页面缓存
        loadAssetsToMemory();
        cache.pages = {};
        console.log('✓ 所有页面缓存已清除，资源已重新加载');
      }
    });
  }

  // 监听配置文件变化
  if (fs.existsSync('book.yaml')) {
    fs.watchFile('book.yaml', () => {
      console.log('配置文件变化');
      
      // 重新加载配置并清除所有页面缓存
      loadConfigToMemory();
      cache.pages = {};
      console.log('✓ 配置已重新加载，所有页面缓存已清除');
    });
  }
}

/**
 * 启动开发服务器
 * @param {number} port - 端口号
 */
function serve(port = 3000) {
  // 初始化内存缓存
  console.log('正在初始化内存缓存...');
  try {
    loadConfigToMemory();
    loadAssetsToMemory();
    console.log('✓ 内存缓存初始化完成');
  } catch (error) {
    console.error('初始化失败:', error.message);
    console.log('继续启动服务器，但可能无法正常显示内容');
  }

  // 创建HTTP服务器
  const server = http.createServer(handleRequest);

  server.listen(port, () => {
    console.log(`开发服务器已启动`);
    console.log(`访问地址: http://localhost:${port}`);
    console.log('按 Ctrl+C 停止服务器');
  });

  // 设置文件监听
  setupFileWatchers();
}

module.exports = {
  serve
};