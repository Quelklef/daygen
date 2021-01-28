(function() {

const exports =
window.STATE = {};

function loadVersion() {
  const s = window.localStorage.getItem('version');
  if (s === null) return 0;
  return parseInt(s, 10);
}

function saveVersion(v) {
  if (!(typeof v === 'number' && v >= 0 && Math.round(v) === v)) throw 'bad!';
  window.localStorage.setItem('version', v);
}

const loadState =
exports.loadState =
function loadState() {
  return JSON.parse(window.localStorage.getItem('state'));
}

const saveState =
exports.saveState =
function saveState(state) {
  window.localStorage.setItem('state', JSON.stringify(state));
}

exports.runMigrations =
function runMigrations() {
  let version = loadVersion();
  let state = loadState();
  while (version in migrations) {
    const migration = migrations[version];
    state = migration(state);
    version++;
  }
  saveState(state);
  saveVersion(version);
}

const migrations = [];

/* v0 -> v1

interface State {
  sigmas: Array<Sigmas>;
  editing: boolean;
}

interface Sigma {
  uuid: string;
  variants: Array<Variant>;
  current: string;
  history: Array<string>;
  elasticity: number;
}

interface Variant {
  uuid: string;
  name: string;
  weight: number;
}

*/
migrations.push(function(old) {
  return { sigmas: [], editing: false };
});

})();
