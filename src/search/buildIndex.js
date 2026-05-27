export { buildSearchIndex } from '@rbook/search/buildIndex';

import { buildSearchIndex } from '@rbook/search/buildIndex';
import { searchIndexPath } from '@rbook/core/paths';
import path from 'path';

if (import.meta.url === `file://${process.argv[1]}`) {
  const outputPath = process.argv[2] ? path.resolve(process.argv[2]) : searchIndexPath;
  const payload = buildSearchIndex({ outputPath });
  console.log(`Search index written: ${outputPath}`);
  console.log(`Pages: ${payload.stats.pages}, chunks: ${payload.stats.chunks}, errors: ${payload.stats.errors}`);
}
