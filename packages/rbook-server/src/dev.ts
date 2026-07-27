import fs from 'fs';
import os from 'os';
import path from 'path';

// Paths are evaluated during module loading, so set the isolated runtime first.
const runtimeDir = process.env.RBOOK_RUNTIME_DIR
  ? path.resolve(process.env.RBOOK_RUNTIME_DIR)
  : path.join(os.tmpdir(), `rbook-dev-${process.pid}`);
process.env.RBOOK_RUNTIME_DIR = runtimeDir;

const [{ createApp }, { distDir }, { compileMarkdownCss, copyStaticAssets, buildStaticWidgetApps }, { default: DevRenderer }, { assertPreCheckContext }, { buildSearchIndexFromDocuments }, { setIndexPayload }] = await Promise.all([
  import('./app.js'),
  import('@rbook/core/paths'),
  import('./buildRuntime.js'),
  import('./devRenderer.js'),
  import('@rbook/search/preCheck'),
  import('@rbook/search/buildIndex'),
  import('@rbook/search')
]);

const preCheckContext = assertPreCheckContext();
setIndexPayload(buildSearchIndexFromDocuments(
  preCheckContext.site,
  preCheckContext.pages,
  preCheckContext.codes,
  { write: false }
));
fs.rmSync(runtimeDir, { recursive: true, force: true });
fs.mkdirSync(runtimeDir, { recursive: true });

console.log(`[dev] runtimeDir=${runtimeDir}`);
compileMarkdownCss(false);
copyStaticAssets();
buildStaticWidgetApps();

const app = await createApp({
  devRenderer: new DevRenderer(preCheckContext),
  staticDir: distDir
});

const host = process.env.HOST || '0.0.0.0';
const port = Number(process.env.PORT || 3000);

try {
  await app.listen({ host, port });
  console.log(`[dev] listening on http://${host === '0.0.0.0' ? 'localhost' : host}:${port}`);
} catch (error) {
  app.log.error(error);
  process.exit(1);
}
