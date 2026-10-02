// Writes tests/testthat/fixtures/core-frames/frames.json: what the core of the
// vendored bio.viz bundle (`BioViz.core.frame`) returns for each case in
// cases.json beside it.
//
//   node data-raw/core-frames.mjs
//
// Run from the repository root; data-raw/vendor-bio-viz.R runs it after it
// copies the bundle. Widget_GroupComparison() resolves the chart's variables to
// one row per participant in R, and the chart resolves them again in the page:
// the one place the same rule is written twice. bio.viz's own fixtures hold R to
// the chart for three value types with a participant table. This file holds it
// for the rest: fold and percent change, several baseline visits, the results
// table alone, and every reason a participant is left out.
//
// Nothing is typed in: every frame is written by the bundle the widget ships,
// and the file records that bundle's checksum, which the tests compare with the
// vendored file's.

import { createHash } from 'node:crypto';
import { readFileSync, writeFileSync } from 'node:fs';
import vm from 'node:vm';

const lib = 'inst/htmlwidgets/lib';
const fixtures = 'tests/testthat/fixtures/core-frames';
const sha256 = (bytes) => createHash('sha256').update(bytes).digest('hex');

const record = JSON.parse(readFileSync(`${lib}/SOURCE.json`, 'utf8'));
const bundle = record.files.find((entry) => entry.library === 'bio.viz');
const bundleBytes = readFileSync(`${lib}/${bundle.file}`);
if (sha256(bundleBytes) !== bundle.sha256) {
  throw new Error(`${lib}/${bundle.file} does not match its record.`);
}
const context = vm.createContext({});
vm.runInContext(bundleBytes.toString('utf8'), context);
const { core } = vm.runInContext('BioViz', context);

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

const studyFiles = {
  results: 'inst/extdata/synthetic_results.csv',
  participants: 'inst/extdata/synthetic_participants.csv'
};
const studyBytes = Object.fromEntries(
  Object.entries(studyFiles).map(([name, file]) => [name, readFileSync(file)])
);
const { edge, cases } = JSON.parse(readFileSync(`${fixtures}/cases.json`, 'utf8'));
const sources = {
  study: {
    results: parse(studyBytes.results.toString('utf8')),
    participants: parse(studyBytes.participants.toString('utf8'))
  },
  edge
};

// A case's tables: the participant table left out when the case says so, with
// the columns it names carried onto the results rows first.
function tablesOf(entry) {
  const { results, participants } = sources[entry.tables];
  if (entry.participants !== false) return { results, participants };
  const carry = entry.carry || [];
  const byId = new Map(participants.map((row) => [row.USUBJID, row]));
  return {
    results: results.map((row) => ({
      ...row,
      ...Object.fromEntries(carry.map((column) => [column, byId.get(row.USUBJID)[column]]))
    }))
  };
}

const frames = cases.map((entry) => {
  const made = core.frame(tablesOf(entry), entry.variables, entry.settings);
  return {
    case: entry.case,
    participants: made.participants,
    baseline_visits: made.baseline_visits,
    dropped: made.dropped,
    data: made.data
  };
});

// One frame per line, so a change to one case is a one-line change in review.
const head = {
  what:
    "What the core of the vendored bio.viz bundle returns for each case in cases.json: the frame's " +
    'rows, the participants seen, who was left out and why, and the baseline visits used.',
  written_by: 'data-raw/core-frames.mjs',
  bundle: { file: `${lib}/${bundle.file}`, sha256: bundle.sha256, bio_viz_commit: record.commit },
  study: Object.entries(studyFiles).map(([name, file]) => ({
    file,
    sha256: sha256(studyBytes[name])
  }))
};
const lines = [
  '{',
  ...Object.entries(head).map(([key, value]) => `  ${JSON.stringify(key)}: ${JSON.stringify(value)},`),
  '  "frames": [',
  ...frames.map((frame, index) => `    ${JSON.stringify(frame)}${index < frames.length - 1 ? ',' : ''}`),
  '  ]',
  '}'
];
writeFileSync(`${fixtures}/frames.json`, lines.join('\n') + '\n');
console.log(
  `Wrote ${fixtures}/frames.json: ${frames.length} frames by bio.viz at ${record.commit.slice(0, 7)}`
);
