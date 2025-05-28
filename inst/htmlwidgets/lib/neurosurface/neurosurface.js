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
