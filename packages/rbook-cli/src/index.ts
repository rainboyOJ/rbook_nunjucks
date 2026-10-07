import rbook from '@rbook/core';
import { assertPreCheckContext } from '@rbook/search/preCheck';

export function run(argv = process.argv.slice(2)) {
  const command = argv[0];

  switch (command) {
    case 'build': {
      const context = assertPreCheckContext();
      const app = new rbook({
        config: context.site,
        codeTemplates: context.codes,
        articlePages: context.pages
      });
      app.build();
      app.build_glob();
      break;
    }

    default:
      console.log('用法:');
      console.log('  node bin/rbook.js build    - 构建静态网站');
      process.exitCode = 1;
  }
}

export default run;
