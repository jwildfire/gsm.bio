// Writes tests/testthat/fixtures/filter-states/states.json: what the filters
// open on for each case in cases.json beside it, worked out by the safety.viz
// kit the widgets ship (`SafetyViz.kit`).
//
//   node data-raw/filter-states.mjs
//
// Run from the repository root; data-raw/vendor-bio-viz.R runs it after it
// copies the bundles. Every bio.viz chart builds its filters through the kit:
// it normalises each spec, starts the state from the specs, and reconciles the
// state with the values each control offers before it first asks R
// (bio.viz, src/shared/chartHost.js, `addFilterControls`). R/chart.R's
// Chart_Filters() writes that rule a second time, so that R computes on the
// participants the chart keeps; this file holds it to the kit itself.
//
// Nothing is typed in: every state is the kit's, and the file records the
// bundle's checksum, which the tests compare with the vendored file's.

import { createHash } from 'node:crypto';
import { readFileSync, writeFileSync } from 'node:fs';
import vm from 'node:vm';

const lib = 'inst/htmlwidgets/lib';
const fixtures = 'tests/testthat/fixtures/filter-states';
const sha256 = (bytes) => createHash('sha256').update(bytes).digest('hex');

const record = JSON.parse(readFileSync(`${lib}/SOURCE.json`, 'utf8'));
const bundle = record.files.find((entry) => entry.library === 'safety.viz');
const bundleBytes = readFileSync(`${lib}/${bundle.file}`);
if (sha256(bundleBytes) !== bundle.sha256) {
  throw new Error(`${lib}/${bundle.file} does not match its record.`);
}
// The kit warns in the console when a start is not in the data; the warnings
// are kept beside each state.
let warnings = [];
const context = vm.createContext({ console: { ...console, warn: (text) => warnings.push(String(text)) } });
context.window = context;
context.self = context;
vm.runInContext(bundleBytes.toString('utf8'), context);
const { kit } = vm.runInContext('SafetyViz', context);

// The study's CSV files hold no quoted field, so a split is a read. Every value
// is text, as a page that reads a CSV file has it.
function parse(text) {
  const [header, ...lines] = text.replace(/\n$/, '').split('\n');
  const columns = header.split(',');
  return lines.map((line) => {
    const cells = line.split(',');
    return Object.fromEntries(columns.map((column, index) => [column, cells[index]]));
  });
}
const participantsFile = 'inst/extdata/synthetic_participants.csv';
const participantsBytes = readFileSync(participantsFile);
const participants = parse(participantsBytes.toString('utf8'));
const idCol = 'USUBJID';

// What a filter control offers, as bio.viz's chartHost lists it.
const valuesOf = (spec) =>
  [
    ...new Set(
      participants
        .map((row) => row[spec.value_col])
        .filter((entry) => entry !== undefined && entry !== null && entry !== '')
        .map(String)
    )
  ].sort((a, b) => a.localeCompare(b, undefined, { numeric: true }));

const { cases } = JSON.parse(readFileSync(`${fixtures}/cases.json`, 'utf8'));
const states = cases.map((entry) => {
  warnings = [];
  const specs = entry.filters.map((spec) => kit.normalizeFilterSpec(spec));
  const state = kit.initFilterState(specs);
  // The participant's id is no filter: it gets no control, and so no restriction.
  const drawn = specs.filter((spec) => spec.value_col !== idCol);
  const reconciled = kit.reconcileFilters(state, drawn, valuesOf);
  return {
    case: entry.case,
    state,
    offered: Object.fromEntries(reconciled.map(({ spec, values }) => [spec.value_col, values.length])),
    warnings
  };
});

const head = {
  what:
    'What the vendored safety.viz kit opens each filter on for each case in cases.json, as bio.viz ' +
    "charts build their filters: the state by column (null for All), how many values each control " +
    'offers, and the warnings the kit gave.',
  written_by: 'data-raw/filter-states.mjs',
  bundle: { file: `${lib}/${bundle.file}`, sha256: bundle.sha256, bio_viz_commit: record.commit },
  study: [{ file: participantsFile, sha256: sha256(participantsBytes) }]
};
const lines = [
  '{',
  ...Object.entries(head).map(([key, value]) => `  ${JSON.stringify(key)}: ${JSON.stringify(value)},`),
  '  "states": [',
  ...states.map((state, index) => `    ${JSON.stringify(state)}${index < states.length - 1 ? ',' : ''}`),
  '  ]',
  '}'
];
writeFileSync(`${fixtures}/states.json`, lines.join('\n') + '\n');
console.log(
  `Wrote ${fixtures}/states.json: ${states.length} filter states by safety.viz ${bundle.version} from bio.viz at ${record.commit.slice(0, 7)}`
);
