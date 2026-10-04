// Reads specifications with bio.viz's own reader, the copied bundle's
// BioViz.output.readSpecification, and writes what it made of each as JSON:
//
//   node bioviz-reader.mjs <bio.viz.js> <cases.json>
//
// <cases.json> is a JSON array of { case, text }, each `text` a specification
// as JSON text. For each case it writes { case, accepted, chart, settings } or
// { case, accepted: false, refusal }. The bundle is run in a context of its own
// with no page: reading a specification needs none. Nothing is fetched.
import { readFileSync } from 'node:fs';
import vm from 'node:vm';

const [bundle, cases] = process.argv.slice(2);
const context = { console };
context.globalThis = context;
vm.createContext(context);
vm.runInContext(`${readFileSync(bundle, 'utf8')};globalThis.BioViz = BioViz;`, context);
const { readSpecification } = context.BioViz.output;
const out = JSON.parse(readFileSync(cases, 'utf8')).map(({ case: name, text }) => {
  try {
    const read = readSpecification(text);
    return { case: name, accepted: true, chart: read.chart, settings: JSON.parse(JSON.stringify(read.settings)) };
  } catch (error) {
    return { case: name, accepted: false, refusal: String(error.message) };
  }
});
process.stdout.write(JSON.stringify(out));
