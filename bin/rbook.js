#!/usr/bin/env node

import fs from 'fs';
import path from 'path';
import { build } from '../src/builder.js';
import { serve } from '../src/server.js';

// 获取命令行参数
const args = process.argv.slice(2);
const command = args[0];

// 简单的命令处理
switch (command) {
  case 'build':
    console.log('开始构建...');
    try {
      build();
      console.log('构建完成！');
    } catch (error) {
      console.error('构建失败:', error.message);
      process.exit(1);
    }
    break;
    
  case 'serve':
    const port = args[1] || 3000;
    console.log(`启动开发服务器，端口: ${port}`);
    try {
      serve(port);
    } catch (error) {
      console.error('服务器启动失败:', error.message);
      process.exit(1);
    }
    break;
    
  default:
    console.log('用法:');
    console.log('  node bin/rbook.js build    - 构建静态网站');
    console.log('  node bin/rbook.js serve    - 启动开发服务器');
    process.exit(1);
}