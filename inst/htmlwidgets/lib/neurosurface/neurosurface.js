
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

            this.mouse = new Vector2();
            this.intersectionPoint = new Vector3();

            this.animationId = null;
            this.paneContainer = null;
            paneContainer.style.zIndex = '1000';
            this.container.appendChild(paneContainer);
            this.paneContainer = paneContainer;
            animate() {
              this.animationId = requestAnimationFrame(this.animate);
              this.controls.update();
              this.render();
            }
            setInitialZoom(zoom) {
              this.config.initialZoom = zoom;
              if (this.camera) {
                const direction = this.camera.position.clone().sub(this.controls.target).normalize();
                const distance = this.camera.position.distanceTo(this.controls.target);
                this.camera.position.copy(this.controls.target.clone().add(direction.multiplyScalar(distance / zoom)));
                this.camera.updateProjectionMatrix();
                this.controls.update();
              }
            }

            dispose() {
              if (this.animationId !== null) {
                cancelAnimationFrame(this.animationId);
                this.animationId = null;
              }
              if (this.controls) {
                this.controls.dispose();
                this.controls = null;
              }
              if (this.pane && typeof this.pane.dispose === 'function') {
                this.pane.dispose();
              }
              if (this.paneContainer && this.paneContainer.parentNode) {
                this.paneContainer.parentNode.removeChild(this.paneContainer);
                this.paneContainer = null;
              }
            }

          }
