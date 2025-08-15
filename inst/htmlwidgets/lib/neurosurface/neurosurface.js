
          addSurface(surface, id) {
            console.log('Adding surface:', surface, 'with id:', id);
            if (this.surfaces.has(id)) {
              const oldSurface = this.surfaces.get(id);
              if (oldSurface && oldSurface.mesh) {
                this.scene.remove(oldSurface.mesh);
                if (oldSurface.mesh.geometry) {
                  oldSurface.mesh.geometry.dispose();
                }
                if (oldSurface.mesh.material) {
                  if (Array.isArray(oldSurface.mesh.material)) {
                    oldSurface.mesh.material.forEach(m => m.dispose && m.dispose());
                  } else if (typeof oldSurface.mesh.material.dispose === 'function') {
                    oldSurface.mesh.material.dispose();
                  }
                }
              }
            }
            this.surfaces.set(id, surface);
            removeSurface(id) {
              const surface = this.surfaces.get(id);
              if (surface) {
                this.scene.remove(surface.mesh);
                if (surface.mesh && surface.mesh.geometry) {
                  surface.mesh.geometry.dispose();
                }
                if (surface.mesh && surface.mesh.material) {
                  if (Array.isArray(surface.mesh.material)) {
                    surface.mesh.material.forEach(m => m.dispose && m.dispose());
                  } else if (typeof surface.mesh.material.dispose === 'function') {
                    surface.mesh.material.dispose();
                  }
                }
                this.surfaces.delete(id);
              }
            }

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

