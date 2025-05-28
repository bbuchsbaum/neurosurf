            directionalLightIntensity: 0.5,
            enablePointLight: false,
            rotationSpeed: 2,
            if (this.config.enablePointLight) {
              this.pointLight = new PointLight(0xFFFFFF);
              this.pointLight.position.set(0, 0, 500);
              this.scene.add(this.pointLight);
            }
          }
            lightingFolder.addBinding(this.config, 'directionalLightIntensity', {
              label: 'Light Intensity',
              min: 0,
              max: 1,
              step: 0.01,
            }).on('change', (ev) => {
              this.updateDirectionalLightIntensity(ev.value);
            });

            lightingFolder.addBinding(this.config, 'enablePointLight', {
              label: 'Point Light'
            }).on('change', (ev) => {
              this.updatePointLightEnabled(ev.value);
            });
          updateDirectionalLightIntensity(intensity) {
            if (this.directionalLight) {
              this.directionalLight.intensity = intensity;
              this.render();
            }
          }

          updatePointLightEnabled(enabled) {
            this.config.enablePointLight = enabled;
            if (enabled) {
              if (!this.pointLight) {
                this.pointLight = new PointLight(0xFFFFFF);
                this.pointLight.position.set(0, 0, 500);
                this.scene.add(this.pointLight);
              }
            } else if (this.pointLight) {
              this.scene.remove(this.pointLight);
              this.pointLight = null;
            }
            this.render();
          }
