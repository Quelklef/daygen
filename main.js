(function() {

function genUuid(state) {
  const chars = '1234567890abcdefghijklmnoparstuvwxyz';
  const ids = new Set(state.sigmas.map(s => s.uuid));
  let id;
  do {
    id = new Array(12).fill(null).map(() => chars[Math.floor(Math.random() * chars.length)]).join('');
  } while (ids.has(id));
  return id;
}

function randomChoice(seq) {
  return seq[Math.floor(Math.random() * seq.length)];
}

function exampleSigma(state) {
  const examples = [
    { name: 'Social media',       variants: [ ['allowed', 1], ['disallowed', 1] ] },
    { name: 'Eating sugar',       variants: [ ['delicious day', 1], ['healthy day', 2] ] },
    { name: 'Communiation',       variants: [ ['words', 1], ['grunting and dancing', 9] ] },
    { name: 'Working stance',     variants: [ ['sitting', 1], ['standing', 1] ] },
    { name: 'Diet',               variants: [ ['omnivore', 1], ['vegetarian', 1], ['vegan', 1] ] },
    { name: 'Clothing',           variants: [ ['pants only', 1], ['shirt only', 1] ] },
    { name: 'Dress style',        variants: [ ['fancy', 1], ['casual', 1] ] },
    { name: 'Internet',           variants: [ ['on', 1], ['off', 1] ] },
    { name: 'Utensils',           variants: [ ['allowed', 1], ['eat with your hands!', 1] ] },
    { name: 'Ordering food',      variants: [ ['allowed', 1], ['cook!', 2] ] },
    { name: 'Masturbation',       variants: [ ['required', 1], ['optional', 1] ] },
    { name: 'Vibe',               variants: [ ['cat', 1], ['dog', 1] ] },
    { name: 'Stocks',             variants: [ ['buy', 1], ['sell', 1] ] },
    { name: 'Today is exempt',    variants: [ ['no', 19], ['yes', 1] ] },
    { name: 'Walking style',      variants: [ ['walk', 1], ['jog', 1], ['crawl', 1] ] },
    { name: 'Music',              variants: [ ['allowed', 1], ['listen to life', 1] ] },
    { name: 'Obeying stop signs', variants: [ ['allowed', 1], ['forbidden', 1] ] },
    { name: 'Lawfulness',         variants: [ ['follow the law', 1], ['above the law', 1] ] },
    { name: 'Political stance',   variants: [ ['blue', 1], ['red', 1] ] },
    { name: 'Sexuality',          variants: [ ['straight', 1], ['gay', 1], ['bi', 1], ['pan', 1], ['ace', 1], ['just vibing', 1] ] },
    { name: 'Spirituality',       variants: [ ['on', 1], ['off', 1] ] },
  ];

  // Give them the proper shape
  const sigmas = examples.map(({ name, variants }) => {
    variants = variants.map(([name, weight]) => ({
      uuid: genUuid(state),
      name: name,
      weight: weight,
    }));
    return {
      name: name + ' (for example)',
      uuid: genUuid(state),
      variants: variants,
      current: randomChoice(variants).uuid,
      history: [],
      elasticity: 0,
    };
  });
  return randomChoice(sigmas);
}

// Using global state :O
window.STATE.runMigrations();
let state = window.STATE.loadState();
rerender();

function save() {
  console.log('saving state', state);
  window.STATE.saveState(state);
}

function rerender() {
  const $root = document.getElementById('root');
  const rendered = render(state);
  $root.innerHTML = '';
  $root.append(rendered);
}

function render(state) {
  const $container = document.createElement('div');
  $container.classList.add(':root');

  const $randomize = document.createElement('button');
  $randomize.innerText = "It's a new day!";
  $randomize.classList.add(':root:randomize', ':button');
  $randomize.addEventListener('click', () => {
    console.warn('TODO');
  });
  $container.append($randomize);

  $container.append(document.createElement('hr'));

  const $addSigma = document.createElement('button');
  $addSigma.innerText = '+';
  $addSigma.classList.add(':root:add-sigma', ':button');
  $addSigma.addEventListener('click', () => {
    state.sigmas.push(exampleSigma(state));
    save();
    rerender();
  });
  $container.append($addSigma);

  const $toggleEditing = document.createElement('button');
  $toggleEditing.innerText = 'edit';
  $toggleEditing.classList.add(':root:toggle-editing', ':button');
  if (state.editing) $toggleEditing.classList.add('--pressed');
  $toggleEditing.addEventListener('click', () => {
    state.editing = !state.editing;
    save();
    rerender();
  });
  $container.append($toggleEditing);

  $container.append(...state.sigmas.map(s => renderSigma(s, state.editing)));

  // hehe
  if (state.editing)
    for (const $el of $container.querySelectorAll('*'))
      $el.classList.add('--editable');

  return $container;
}

function renderSigma(sigma, isEditable) {
  const $sigma = document.createElement('div');
  $sigma.classList.add(':sigma');

  const $name = document.createElement(isEditable ? 'input' : 'span');
  $name.classList.add(':sigma:name')
  $name.type = 'text';
  $name[isEditable ? 'value' : 'innerText'] = sigma.name;
  if (isEditable) $name.addEventListener('change', event => {
    sigma.name = event.target.value;
    save();
  });
  $sigma.append($name);

  const $elasticity = document.createElement(isEditable ? 'input' : 'span');
  $elasticity.classList.add(':sigma:elasticity');
  $elasticity[isEditable ? 'value' : 'innerText'] = sigma.elasticity;
  if (isEditable) $elasticity.type = 'number';
  if (isEditable) $elasticity.addEventListener('change', event => {
    const e = parseInt(event.target.value, 10);
    if (!Number.isNaN(e)) {
      sigma.elasticity = e;
      save();
    }
  });
  $sigma.append($elasticity);

  if (isEditable) $sigma.append(renderObliterate(() => {
    state.sigmas = state.sigmas.filter(s => s.uuid !== sigma.uuid);
    save();
    rerender();
  }));

  $sigma.append(...sigma.variants.map(v => renderVariant(v, v.name === sigma.current, isEditable)));

  return $sigma;
}

function renderVariant(variant, isCurrent, isEditable) {
  const $variant = document.createElement('span');
  $variant.classList.add(':variant');

  const $name = document.createElement(isEditable ? 'input' : 'span');
  $name.classList.add(':variant:name');
  $name[isEditable ? 'value' : 'innerText'] = variant.name;
  if (isCurrent) $name.classList.add('--current');
  if (isEditable) $name.addEventListener('change', event => {
    variant.name = event.target.value;
    save();
  });
  $variant.append($name);

  const $weight = document.createElement(isEditable ? 'input' : 'span');
  $weight.classList.add(':variant:weight');
  $weight[isEditable ? 'value' : 'innerText'] = variant.weight;
  if (isEditable) $weight.addEventListener('input', event => {
    const n = parseInt(event.target.value);
    if (!Number.isNaN(n)) {
      variant.weight = n;
      save();
    }
  });
  $variant.append($weight);

  if (isEditable) $variant.append(renderObliterate(() => {
    state.sigmas = state.sigmas.map(sigma => ({ ...sigma, variants: sigma.variants.filter(v => v.uuid != variant.uuid) }));
    save();
    rerender();
  }));

  return $variant;
}

function renderObliterate(onClick) {
  const $obliterate = document.createElement('button');
  $obliterate.classList.add(':sigma:oblierate', ':button');
  $obliterate.innerText = 'delete_outline';
  $obliterate.addEventListener('click', onClick);
  return $obliterate;
}

})();
