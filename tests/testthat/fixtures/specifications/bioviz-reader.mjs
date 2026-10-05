// Reads specifications with bio.viz's own reader, the copied bundle's
// BioViz.output.readSpecification, and writes what it made of each as JSON:
//
//   node bioviz-reader.mjs <bio.viz.js> <cases.json> [--object]
//
// <cases.json> is a JSON array of { case, text }, each `text` a specification
// as JSON text. For each case it writes { case, accepted, chart, settings } or
// { case, accepted: false, refusal }. The bundle is run in a context of its own
// with no page: reading a specification needs none. Nothing is fetched. With
// --object, each text is parsed in the bundle's context first and the reader
// is handed the object, as a page that holds a specification already read
// hands it: the reader checks such an object's depth, and not text's.
import { readFileSync } from 'node:fs';
import vm from 'node:vm';

const [bundle, cases, mode] = process.argv.slice(2);
const context = { console };
context.globalThis = context;
vm.createContext(context);
vm.runInContext(`${readFileSync(bundle, 'utf8')};globalThis.BioViz = BioViz;`, context);
const { readSpecification } = context.BioViz.output;
const parse = vm.runInContext('JSON.parse', context);
const out = JSON.parse(readFileSync(cases, 'utf8')).map(({ case: name, text }) => {
  try {
    const read = readSpecification(mode === '--object' ? parse(text) : text);
    return { case: name, accepted: true, chart: read.chart, settings: JSON.parse(JSON.stringify(read.settings)) };
  } catch (error) {
    return { case: name, accepted: false, refusal: String(error.message) };
  }
});
process.stdout.write(JSON.stringify(out));
