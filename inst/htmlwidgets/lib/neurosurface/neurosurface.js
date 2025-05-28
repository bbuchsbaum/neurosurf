          dispose() {
            if (this.mesh) {
              this.mesh.geometry.dispose();
              this.mesh.material.dispose();
              this.mesh = null;
            }
          }
          dispose() {
            if (this.rangeListener) this.rangeListener();
            if (this.thresholdListener) this.thresholdListener();
            if (this.alphaListener) this.alphaListener();
            super.dispose();
          }
          dispose() {
            super.dispose();
          }
