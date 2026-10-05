// Writes tests/testthat/fixtures/filter-states/states.json: what the filters
// open on for each case in cases.json beside it, worked out by the bundles the
// widgets ship.
//
//   node data-raw/filter-states.mjs
//
// Run from the repository root; data-raw/vendor-bio-viz.R runs it after it
// copies the bundles. Every bio.viz chart builds its filters the same way: it
// normalises each spec and starts the state with the safety.viz kit
// (`normalizeFilterSpec`, `initFilterState`), and then, before it first asks
// R, bio.viz's `addFilterControls` lists the values each control offers and
// reconciles the state with them through the kit's `reconcileFilters`
// (bio.viz, src/shared/chartHost.js). R/chart.R's Chart_Filters() writes that
// rule a second time, so that R computes on the participants the chart keeps;
// this file holds it to the bundles themselves.
//
// The kit's functions are called as `SafetyViz.kit` exports them. bio.viz does
// not export `addFilterControls`, so its text is cut out of the vendored
// bio.viz bundle and run as it is, on a stand-in for a chart that holds the
// tables, the specs and the state, and draws no control. Nothing of the rule is
// typed here; the file records both bundles' checksums, which the tests compare
// with the vendored files'.

import { createHash } from 'node:crypto';
import { readFileSync, writeFileSync } from 'node:fs';
import vm from 'node:vm';

const lib = 'inst/htmlwidgets/lib';
const fixtures = 'tests/testthat/fixtures/filter-states';
const sha256 = (bytes) => createHash('sha256').update(bytes).digest('hex');

const record = JSON.parse(readFileSync(`${lib}/SOURCE.json`, 'utf8'));
const read = (library) => {
  const entry = record.files.find((file) => file.library === library);
  const bytes = readFileSync(`${lib}/${entry.file}`);
  if (sha256(bytes) !== entry.sha256) throw new Error(`${lib}/${entry.file} does not match its record.`);
  return { entry, text: bytes.toString('utf8') };
};
const safety = read('safety.viz');
const bio = read('bio.viz');

// The kit warns in the console when a start is not in the data; the warnings
// are kept beside each state.
let warnings = [];
const context = vm.createContext({ console: { ...console, warn: (text) => warnings.push(String(text)) } });
context.window = context;
context.self = context;
vm.runInContext(safety.text, context);

// bio.viz's addFilterControls and startFilters, cut from its bundle: each from
// its first line to the closing brace at the same indent. startFilters is how
// a chart starts its state: the kit's, with a filter of several values given
// an empty start opening on no value, so that it lets nobody through.
const lines = bio.text.split('\n');
const cut = (name) => {
  const first = lines.findIndex((line) => line.startsWith(`  function ${name}(`));
  const last = lines.findIndex((line, index) => index > first && line === '  }');
  if (first < 0 || last < 0) throw new Error(`${name} was not found in the bio.viz bundle.`);
  vm.runInContext(lines.slice(first, last + 1).join('\n'), context);
  return vm.runInContext(name, context);
};
const { kit } = vm.runInContext('SafetyViz', context);
const addFilterControls = cut('addFilterControls');
const startFilters = cut('startFilters');

// The study's CSV files hold no quoted field, so a split is a read. Every value
// is text, as a page that reads a CSV file has it.
function parse(text) {
  const [header, ...rows] = text.replace(/\n$/, '').split('\n');
  const columns = header.split(',');
  return rows.map((row) => {
    const cells = row.split(',');
    return Object.fromEntries(columns.map((column, index) => [column, cells[index]]));
  });
}
const participantsFile = 'inst/extdata/synthetic_participants.csv';
const participantsBytes = readFileSync(participantsFile);
const study = parse(participantsBytes.toString('utf8'));
// A case's own table, given by column, as rows: the values as JSON has them,
// as a widget's page has R's.
const rowsOf = (columns) =>
  Array.from({ length: Object.values(columns)[0].length }, (_, index) =>
    Object.fromEntries(Object.keys(columns).map((name) => [name, columns[name][index]]))
  );

// What the filters open on: the kit's state after bio.viz reconciles it, how
// many values each control offers, and the warnings.
function open(participants, filters) {
  warnings = [];
  const filterSpecs = filters.map((spec) => kit.normalizeFilterSpec(spec));
  const state = { filters: startFilters({ kit, filterSpecs, settings: { filters } }) };
  const offered = {};
  const chart = {
    kit: { ...kit, renderFilterControl: ({ spec, values }) => ((offered[spec.value_col] = values.length), { dataset: {} }) },
    state,
    filterSpecs,
    settings: { id_col: 'USUBJID', participant_id_col: null },
    tables: { participants }
  };
  addFilterControls(chart, { addSection: () => null, addControl: () => null }, () => null);
  return { state: state.filters, offered, warnings };
}

const { cases } = JSON.parse(readFileSync(`${fixtures}/cases.json`, 'utf8'));
const states = cases.map((entry) => {
  const participants = entry.participants ? rowsOf(entry.participants) : study;
  const opened = { case: entry.case, ...open(participants, entry.filters) };
  if (entry.differs) {
    // The spec as the widget hands it to the page: R's value named as its start.
    const named = entry.filters.map((spec) => ({ ...spec, start: entry.differs.r }));
    opened.named = open(participants, named).state;
  }
  return opened;
});

const head = {
  what:
    'What the vendored bundles open each filter on for each case in cases.json, as bio.viz charts ' +
    'build their filters: the state by column (null for All), how many values each control offers, ' +
    "the warnings the kit gave, and, for a case where R's first value differs, the state with R's value named as the start.",
  written_by: 'data-raw/filter-states.mjs',
  bundle: { file: `${lib}/${safety.entry.file}`, sha256: safety.entry.sha256, bio_viz_commit: record.commit },
  bio_viz_bundle: { file: `${lib}/${bio.entry.file}`, sha256: bio.entry.sha256 },
  study: [{ file: participantsFile, sha256: sha256(participantsBytes) }]
};
const out = [
  '{',
  ...Object.entries(head).map(([key, value]) => `  ${JSON.stringify(key)}: ${JSON.stringify(value)},`),
  '  "states": [',
  ...states.map((state, index) => `    ${JSON.stringify(state)}${index < states.length - 1 ? ',' : ''}`),
  '  ]',
  '}'
];
writeFileSync(`${fixtures}/states.json`, out.join('\n') + '\n');
console.log(
  `Wrote ${fixtures}/states.json: ${states.length} filter states by safety.viz ${safety.entry.version} and bio.viz ${bio.entry.version} from bio.viz at ${record.commit.slice(0, 7)}`
);
