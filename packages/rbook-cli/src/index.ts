import rbook from '@rbook/core';

export function run(argv = process.argv.slice(2)) {
  const command = argv[0];
  const app = new rbook();

  switch (command) {
    case 'build':
      app.build();
      app.build_glob();
      break;

    default:
      console.log('用法:');
      console.log('  node bin/rbook.js build    - 构建静态网站');
      process.exitCode = 1;
  }
}

export default run;
