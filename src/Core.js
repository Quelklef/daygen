let uuidCounter = parseInt(localStorage.getItem('uuid-counter') || '0');
exports.genUuid = () => {
  const uuid = uuidCounter + '';
  uuidCounter++;
  localStorage.setItem('uuid-counter', uuidCounter);
  return uuid;
};
