#!/usr/bin/env node

import rbook from '../src/rbook/index.js';

// 获取命令行参数
const args = process.argv.slice(2);
const command = args[0];

// 创建rbook实例
const app = new rbook();

// 简单的命令处理
switch (command) {
  case 'build':
    try {
      app.build();
    } catch (error) {
      console.error('构建失败:', error.message);
      process.exit(1);
    }
    break;
    
  default:
    console.log('用法:');
    console.log('  node bin/rbook.js build    - 构建静态网站');
    process.exit(1);
}