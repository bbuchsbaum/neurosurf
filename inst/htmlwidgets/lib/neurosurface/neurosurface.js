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
          }
