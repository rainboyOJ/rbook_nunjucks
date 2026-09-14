import { createApp } from './app.js';
import DevRenderer from './devRenderer.js';
import { assertPreCheckContext } from '@rbook/search/preCheck';

const host = process.env.HOST || '0.0.0.0';
const port = Number(process.env.PORT || 3000);

const preCheckContext = assertPreCheckContext();
const app = await createApp({
  devRenderer: new DevRenderer(preCheckContext)
});

try {
  await app.listen({ host, port });
} catch (error) {
  app.log.error(error);
  process.exit(1);
}
