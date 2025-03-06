export class EventEmitter {
  constructor() {
    this._events = {};
  }

  on(event, listener) {
    if (!this._events[event]) {
      this._events[event] = [];
    }
    this._events[event].push(listener);
    return () => this.removeListener(event, listener);
  }

  emit(event, ...args) {
    if (this._events[event]) {
      this._events[event].forEach((listener) => listener(...args));
    }
  }

  removeListener(event, listenerToRemove) {
    if (this._events[event]) {
      this._events[event] = this._events[event].filter(
        (listener) => listener !== listenerToRemove
      );
    }
  }
}