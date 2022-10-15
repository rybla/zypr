export const set = (key) => (val) => (target) => () => {
  target[key] = val;
}

export const classList_add = (className) => (target) => () => {
  target.classList.add(className);
}

export const classList_remove = (className) => (target) => () => {
  target.classList.remove(className);
}

export const classList_toggle = (className) => (target) => () => {
  target.classList.toggle(className);
}