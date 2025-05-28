
        setData(newData) {
            const isArray = Array.isArray(newData) || ArrayBuffer.isView(newData);
            if (!isArray) {
              console.error('setData expects an array or typed array of numbers');
              return;
            }
            if (newData.length !== this.data.length) {
              console.error(`New data length (${newData.length}) does not match the current data length (${this.data.length})`);
              return;
            }
            this.data = ArrayBuffer.isView(newData) ? new Float32Array(newData) : Float32Array.from(newData);
            this.updateColors();
          }
        setColors(newColors) {
            if (!Array.isArray(newColors)) {
              console.error('setColors expects an array of color strings');
              return;
            }
            if (newColors.length !== this.indices.length) {
              console.error(`Colors array length (${newColors.length}) does not match the number of indices (${this.indices.length})`);
              return;
            }

            this.colors = new Float32Array(newColors.length * 3);
            for (let i = 0; i < newColors.length; i++) {
              if (typeof newColors[i] !== 'string') {
                console.error(`Color at index ${i} is not a valid string`);
                return;
              }
              const color = new Color(newColors[i]);
              this.colors[i * 3] = color.r;
              this.colors[i * 3 + 1] = color.g;
              this.colors[i * 3 + 2] = color.b;
            }
            this.updateColors();

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
