(function () {
  if (typeof window === 'undefined') return;
  if (!window.THREE) return;

  const THREE = window.THREE;
  if (THREE.Controls) return;
  if (!THREE.EventDispatcher) return;

  class Controls extends THREE.EventDispatcher {
    constructor(object, domElement) {
      super();
      this.object = object;
      this.domElement = domElement;
      this.enabled = true;
    }

    connect(domElement) {
      this.domElement = domElement || this.domElement;
    }

    disconnect() {}

    dispose() {
      this.disconnect();
    }

    update() {}
  }

  THREE.Controls = Controls;
})();

