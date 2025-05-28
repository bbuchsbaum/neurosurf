export let DEBUG = false;
export function setDebug(value) {
  DEBUG = value;
}
export function debugLog(...args) {
  if (DEBUG) {
    console.log(...args);
  }
}
