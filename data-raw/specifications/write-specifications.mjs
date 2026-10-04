// Saves the specification each saved widget page's chart writes, offline, as a
// JSON array (see data-raw/specifications/save-pages.R):
//   node write-specifications.mjs <folder> <out.json>
import { chromium } from '@playwright/test';
import { writeFileSync } from 'node:fs';
import path from 'node:path';
import { pathToFileURL } from 'node:url';
const dir = path.resolve(process.argv[2]);
const names = ['group-comparison', 'association-scatter', 'correlation-matrix', 'biomarker-screen', 'cross-tab', 'stratified-survival'];
const browser = await chromium.launch();
const out = [];
for (const name of names) {
  const context = await browser.newContext(); await context.setOffline(true);
  const own = pathToFileURL(path.join(dir, `${name}.html`)).href;
  await context.route('**/*', (r) => (r.request().url() === own ? r.continue() : r.abort()));
  const page = await context.newPage(); const errors = []; page.on('pageerror', (e) => errors.push(String(e)));
  await page.goto(own); await page.waitForSelector('.gsm-bio-chart'); await page.waitForTimeout(1200);
  const spec = await page.evaluate(() => { const w = document.querySelector('.html-widget'); return HTMLWidgets.find('#' + w.id).chart().specification(); });
  out.push(spec);
  if (name === 'group-comparison') {
    // The same chart with a filter in force: Sex, F.
    await page.evaluate(() => { const el = document.querySelector('select[data-filter="SEX"]'); el.value = 'F'; el.dispatchEvent(new Event('change', { bubbles: true })); });
    await page.waitForTimeout(500);
    out.push(await page.evaluate(() => { const w = document.querySelector('.html-widget'); return HTMLWidgets.find('#' + w.id).chart().specification(); }));
  }
  console.log(name, 'errors', errors.length, errors.slice(0, 1));
  await context.close();
}
writeFileSync(process.argv[3], JSON.stringify(out, null, 2) + '\n');
console.log('wrote', out.length);
await browser.close();
