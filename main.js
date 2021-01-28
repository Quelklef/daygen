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

  const $header = document.createElement('div');
  $header.classList.add(':root:header');

  const $title = document.createElement('h1');
  $title.innerText = 'Day Gen!'
  $header.append($title);

  const $randomize = document.createElement('button');
  $randomize.innerText = "It's a new day!";
  $randomize.classList.add(':root:randomize', ':button');
  $randomize.addEventListener('click', () => {
    for (const sigma of state.sigmas) {
      const raffle = sigma.variants.flatMap(v => repeat([v.uuid], v.weight));
      const choice = randomChoice(raffle);
      sigma.current = choice;
      sigma.history.push(choice);
    }

    save();
    rerender();

    function repeat(array, n) {
      const result = [];
      for (let i = 0; i < n; i++)
        result.push(...array);
      return result;
    }
  });
  $header.append($randomize);

  $container.append($header);

  const $toggleEditing = document.createElement('button');
  $toggleEditing.innerText = 'editing: ' + (state.editing ? 'on' : 'off');
  $toggleEditing.classList.add(':root:toggle-editing', ':button');
  if (state.editing) $toggleEditing.classList.add('--pressed');
  $toggleEditing.addEventListener('click', () => {
    state.editing = !state.editing;
    save();
    rerender();
  });
  $container.append($toggleEditing);

  const $addSigma = document.createElement('button');
  $addSigma.innerText = '+';
  $addSigma.classList.add(':root:add-sigma', ':button');
  $addSigma.addEventListener('click', () => {
    state.sigmas.push(exampleSigma(state));
    save();
    rerender();
  });
  if (state.editing) $container.append(' | ', $addSigma);

  $container.append(...state.sigmas.map(s => renderSigma(s, state.editing, state)));

  // hehe
  if (state.editing)
    for (const $el of $container.querySelectorAll('*'))
      $el.classList.add('--editable');

  return $container;
}

function renderSigma(sigma, isEditable, state) {
  const $sigma = document.createElement('div');
  $sigma.classList.add(':sigma');

  const $name = document.createElement('input');
  $name.classList.add(':sigma:name', '~dual')
  $name.type = 'text';
  if (!isEditable) $name.readOnly = true;
  $name.value = sigma.name;
  if (isEditable) $name.addEventListener('change', event => {
    sigma.name = event.target.value;
    save();
  });
  $sigma.append($name);

  /*
  const $elasticity = document.createElement('input');
  $elasticity.type = 'number';
  if (!isEditable) $elasticity.readOnly = true;
  $elasticity.classList.add(':sigma:elasticity', '~dual');
  $elasticity.value = sigma.elasticity;
  $elasticity.addEventListener('change', event => {
    const e = parseInt(event.target.value, 10);
    if (!Number.isNaN(e)) {
      sigma.elasticity = e;
      save();
    }
  });
  $sigma.append($elasticity);
  */

  if (isEditable) $sigma.append(renderObliterate(':sigma:obliterate', () => {
    state.sigmas = state.sigmas.filter(s => s.uuid !== sigma.uuid);
    save();
    rerender();
  }));

  const sumWeights = sigma.variants.map(v => v.weight).reduce((a, b) => a + b, 0);
  const $variants = document.createElement('div');
  $variants.classList.add(':sigma:variants');

  $variants.append(...sigma.variants.map(v => renderVariant(v, sumWeights, v.uuid === sigma.current, isEditable)));

  const $addVariant = document.createElement('button');
  $addVariant.classList.add(':sigma:add-variant', ':button');
  $addVariant.innerText = '+';
  $addVariant.addEventListener('click', () => {
    sigma.variants.push({
      uuid: genUuid(state),
      name: '!?',
      weight: 1,
    });
    save();
    rerender();
  });
  if (isEditable) $variants.append($addVariant);

  $sigma.append($variants);

  return $sigma;
}

function renderVariant(variant, sumWeights, isCurrent, isEditable) {
  const $variant = document.createElement('span');
  $variant.classList.add(':variant');
  if (isCurrent) $variant.classList.add('--current');

  const $name = document.createElement('input');
  $name.classList.add(':variant:name', '~dual');
  $name.value = variant.name;
  if (!isEditable) $name.readOnly = true;
  $name.addEventListener('change', event => {
    variant.name = event.target.value;
    save();
  });
  $variant.append($name);

  const $weight = document.createElement('input');
  $weight.type = 'number';
  $weight.classList.add(':variant:weight', '~dual');
  $weight.value = variant.weight;
  if (!isEditable) $weight.readOnly = true;
  $weight.addEventListener('input', event => {
    const n = parseInt(event.target.value);
    if (!Number.isNaN(n)) {
      console.log(n);
      variant.weight = n;
      save();
    }
  });
  $variant.append(' ', $weight);

  if (isEditable) $variant.append(renderObliterate(':variant:obliterate', () => {
    state.sigmas = state.sigmas.map(sigma => ({ ...sigma, variants: sigma.variants.filter(v => v.uuid != variant.uuid) }));
    save();
    rerender();
  }));

  return $variant;
}

function renderObliterate(clazz, onClick) {
  const $obliterate = document.createElement('button');
  $obliterate.classList.add(clazz, ':button', '~material-icons');
  $obliterate.innerText = 'delete_outline';
  $obliterate.addEventListener('click', onClick);
  return $obliterate;
}

})();
