exports.localStorageSetString = key => val => () => window.localStorage.setItem(key, val);
exports.localStorageGetString = key => () => window.localStorage.getItem(key);
