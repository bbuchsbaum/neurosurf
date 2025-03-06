import * as THREE from 'three';
import ColorMap from './ColorMap.js';

export class SurfaceGeometry {
  constructor(vertices, faces, hemi) {
    this.vertices = new Float32Array(vertices);
    this.faces = new Uint32Array(faces);
    this.hemi = hemi;
    this.mesh = null;

    console.log("SurfaceGeometry constructor called");
    console.log("Vertices:", this.vertices.length);
    console.log("Faces:", this.faces.length);
    console.log("Hemi:", this.hemi);

    this.createMesh();
  }

  createMesh() {
    const geometry = new THREE.BufferGeometry();
    geometry.setAttribute('position', new THREE.Float32BufferAttribute(this.vertices, 3));
    geometry.setIndex(new THREE.Uint32BufferAttribute(this.faces, 1));
    
    const material = new THREE.MeshPhongMaterial({
      color: 0xA9A9A9, // Set default color to dark gray
      flatShading: false,
      vertexColors: false
    });

    this.mesh = new THREE.Mesh(geometry, material);
    console.log("SurfaceGeometry construction complete");
    console.log("Mesh:", this.mesh);
  }
}

export class NeuroSurface {
  constructor(geometry, indices, data, config = {}) {
    this.geometry = geometry;
    this.indices = new Uint32Array(indices);
    this.data = new Float32Array(data);
    this.mesh = null;
    this.threshold = config.thresh || [0, 0];
    this.irange = config.irange || [Math.min(...data), Math.max(...data)];

    this.config = {
      color: new THREE.Color(0xA9A9A9), // Set default color to dark gray
      flatShading: false,
      shininess: 30,
      specularColor: 0x111111,
      alpha: 1,
      ...config
    };
  }

  update(property, value) {
    const methodName = `update${property.charAt(0).toUpperCase() + property.slice(1)}`;
    if (this[methodName]) {
      this[methodName](value);
    } else {
      console.warn(`Update method for ${property} not implemented in ${this.constructor.name}`);
    }
  }

  updateConfig(newConfig) {
    this.config = { ...this.config, ...newConfig };
    if (this.mesh) {
      Object.assign(this.mesh.material, {
        color: this.config.color,
        specular: this.config.specularColor,
        shininess: this.config.shininess,
        flatShading: this.config.flatShading,
        transparent: this.config.alpha < 1,
        opacity: this.config.alpha
      });
      this.mesh.material.needsUpdate = true;
    }
  }

  mapValueToColor(value) {
    if (value > this.threshold[0] && value < this.threshold[1]) {
      return new THREE.Color(0, 0, 0, 0);  // Return transparent for values within the threshold
    }

    const normalizedValue = (value - this.irange[0]) / (this.irange[1] - this.irange[0]);
    const index = Math.min(Math.floor(normalizedValue * (this.colorMap.length - 1)), this.colorMap.length - 1);
    return this.colorMap[index];
  }

  createMesh() {
    const geometry = new THREE.BufferGeometry();
    geometry.setAttribute('position', new THREE.Float32BufferAttribute(this.geometry.vertices, 3));
    geometry.setIndex(new THREE.Uint32BufferAttribute(this.geometry.faces, 1));
    
    const material = new THREE.MeshBasicMaterial({
      vertexColors: true // Will be set to true for colored surfaces
    });

    this.mesh = new THREE.Mesh(geometry, material);
    this.updateColors();
    return this.mesh;
  }

  updateMesh() {
    if (!this.mesh) {
      return this.createMesh();
    }
    this.mesh.geometry.attributes.position.needsUpdate = true;
    this.mesh.geometry.index.needsUpdate = true;
    return this.mesh;
  }

  updateColors() {
    if (!this.mesh || !this.colorMap) {
      console.warn('Mesh or ColorMap not initialized in updateColors');
      return;
    }

    const vertexCount = this.geometry.vertices.length / 3;
    const componentsPerColor = this.colorMap.hasAlpha ? 4 : 3;
    const colors = new Float32Array(vertexCount * componentsPerColor);

    if (this.config.alpha > 0) {
      if (!this.data) {
        console.error('Data not initialized in updateColors');
        return;
      }

      const mappedColors = this.colorMap.getColorArray(this.data);

      for (let i = 0; i < this.indices.length; i++) {
        const index = this.indices[i];
        const colorIndex = index * componentsPerColor;
        for (let j = 0; j < componentsPerColor; j++) {
          colors[colorIndex + j] = mappedColors[colorIndex + j];
        }
      }
    } else {
      // When alpha is 0, use the default color for all vertices
      const defaultColor = new THREE.Color(this.config.color);
      for (let i = 0; i < colors.length; i += componentsPerColor) {
        colors[i] = defaultColor.r;
        colors[i + 1] = defaultColor.g;
        colors[i + 2] = defaultColor.b;
        if (componentsPerColor === 4) {
          colors[i + 3] = 1; // Full opacity when alpha is 0
        }
      }
    }

    this.mesh.geometry.setAttribute('color', new THREE.BufferAttribute(colors, componentsPerColor));
    this.mesh.geometry.attributes.color.needsUpdate = true;
    this.mesh.material.vertexColors = true;
    this.mesh.material.transparent = this.colorMap.hasAlpha;
    this.mesh.material.needsUpdate = true;
  }
}

export class ColorMappedNeuroSurface extends NeuroSurface {
  constructor(geometry, indices, data, colorMap, config = {}) {
    super(geometry, indices, data, config);
    
    this.colorMap = null;
    this.rangeListener = null;
    this.thresholdListener = null;
    this.alphaListener = null;

    this.createMesh();  // Create the mesh first
    this.setColorMap(colorMap);  // Set the color map and update colors
  }

  setColorMap(colorMap) {
    if (this.colorMap) {
      // Remove old listeners
      this.rangeListener();
      this.thresholdListener();
      this.alphaListener();
    }

    if (!(colorMap instanceof ColorMap)) {
      if (typeof colorMap === 'string') {
        this.colorMap = ColorMap.fromPreset(colorMap);
      } else if (Array.isArray(colorMap)) {
        this.colorMap = new ColorMap(colorMap);
      } else {
        console.error('Invalid colorMap provided. Using default.');
        this.colorMap = ColorMap.fromPreset('jet');
      }
    } else {
      this.colorMap = colorMap;
    }

    this.colorMap.setThreshold(this.threshold);
    this.colorMap.setRange(this.irange);
    this.colorMap.setAlpha(this.config.alpha);

    // Set up new listeners
    this.rangeListener = this.colorMap.on('rangeChanged', (range) => {
      console.log('ColorMappedNeuroSurface: Received rangeChanged event', range);
      this.irange = range;
      this.updateColors();
    });
    this.thresholdListener = this.colorMap.on('thresholdChanged', (threshold) => {
      console.log('ColorMappedNeuroSurface: Received thresholdChanged event', threshold);
      this.threshold = threshold;
      this.updateColors();
    });
    this.alphaListener = this.colorMap.on('alphaChanged', (alpha) => {
      console.log('ColorMappedNeuroSurface: Received alphaChanged event', alpha);
      this.config.alpha = alpha;
      this.updateColors();
    });

    if (this.mesh) {
      this.updateColors();
    }
  }

  createMesh() {
    const geometry = new THREE.BufferGeometry();
    geometry.setAttribute('position', new THREE.Float32BufferAttribute(this.geometry.vertices, 3));
    geometry.setIndex(new THREE.Uint32BufferAttribute(this.geometry.faces, 1));
    
    // Use MeshPhongMaterial for better shading
    const material = new THREE.MeshPhongMaterial({
      vertexColors: true,
      transparent: true,
      opacity: this.config.alpha,
      shininess: this.config.shininess || 30,
      specular: new THREE.Color(this.config.specularColor || 0x111111),
      flatShading: this.config.flatShading || false
    });

    this.mesh = new THREE.Mesh(geometry, material);
    
    // Compute vertex normals for better lighting
    geometry.computeVertexNormals();
    
    return this.mesh;
  }

  updateColors() {
    console.log('Updating colors. Mesh:', !!this.mesh, 'ColorMap:', !!this.colorMap);
    if (!this.mesh || !this.colorMap) {
      console.warn('Mesh or ColorMap not initialized in updateColors');
      console.log('Mesh:', this.mesh);
      console.log('ColorMap:', this.colorMap);
      return;
    }

    const vertexCount = this.geometry.vertices.length / 3;
    const componentsPerColor = 4; // Always use RGBA
    const colors = new Float32Array(vertexCount * componentsPerColor);

    console.log("threshold", this.threshold);
    console.log("irange", this.irange);
    console.log("alpha", this.config.alpha);
    console.log("data", this.data);

    const baseSurfaceColor = new THREE.Color(this.config.color);

    if (this.data) {
      for (let i = 0; i < this.indices.length; i++) {
        const index = this.indices[i];
        const value = this.data[i];
        const color = this.colorMap.getColor(value);
        const colorIndex = index * componentsPerColor;
        
        if (value >= this.threshold[0] && value <= this.threshold[1]) {
          // Use opaque base color when value is within threshold
          colors[colorIndex] = baseSurfaceColor.r;
          colors[colorIndex + 1] = baseSurfaceColor.g;
          colors[colorIndex + 2] = baseSurfaceColor.b;
          colors[colorIndex + 3] = 1; // Fully opaque
        } else {
          // Blend with base surface color based on alpha
          const overlayAlpha = color[3] * this.config.alpha;
          colors[colorIndex] = overlayAlpha * color[0] + (1 - overlayAlpha) * baseSurfaceColor.r;
          colors[colorIndex + 1] = overlayAlpha * color[1] + (1 - overlayAlpha) * baseSurfaceColor.g;
          colors[colorIndex + 2] = overlayAlpha * color[2] + (1 - overlayAlpha) * baseSurfaceColor.b;
          colors[colorIndex + 3] = 1; // Always opaque
        }
      }
    } else {
      // When no data, use the opaque default color for all vertices
      for (let i = 0; i < colors.length; i += componentsPerColor) {
        colors[i] = baseSurfaceColor.r;
        colors[i + 1] = baseSurfaceColor.g;
        colors[i + 2] = baseSurfaceColor.b;
        colors[i + 3] = 1; // Fully opaque
      }
    }

    this.mesh.geometry.setAttribute('color', new THREE.BufferAttribute(colors, componentsPerColor));
    this.mesh.geometry.attributes.color.needsUpdate = true;
    this.mesh.material.vertexColors = true;
    this.mesh.material.needsUpdate = true;

    // Ensure transparency is set correctly
    this.mesh.material.transparent = true;
    this.mesh.material.opacity = 1; // We're using per-vertex color blending now
  }

  updateConfig(newConfig) {
    super.updateConfig(newConfig);
    if (this.mesh && this.mesh.material) {
      this.mesh.material.shininess = this.config.shininess || 30;
      this.mesh.material.specular = new THREE.Color(this.config.specularColor || 0x111111);
      this.mesh.material.flatShading = this.config.flatShading || false;
      this.mesh.material.needsUpdate = true;
    }
    if (this.colorMap) {
      this.colorMap.setAlpha(this.config.alpha);
    }
    this.updateColors(); // Reapply colors with new config
  }

  setData(newData) {
    if (newData.length !== this.data.length) {
      console.error('New data length does not match the current data length');
      return;
    }
    this.data = newData;
    this.updateColors();
  }
}

export class VertexColoredNeuroSurface extends NeuroSurface {
  constructor(geometry, indices, colors, config = {}) {
    super(geometry, indices, new Float32Array(indices.length), config);
    this.setColors(colors);
  }

  setColors(newColors) {
    this.colors = new Float32Array(newColors.length * 3);
    for (let i = 0; i < newColors.length; i++) {
      const color = new THREE.Color(newColors[i]);
      this.colors[i * 3] = color.r;
      this.colors[i * 3 + 1] = color.g;
      this.colors[i * 3 + 2] = color.b;
    }
    this.updateColors();
  }

  updateColors() {
    if (!this.mesh) return;

    const colors = new Float32Array(this.geometry.vertices.length);
    for (let i = 0; i < this.indices.length; i++) {
      const index = this.indices[i];
      colors[index * 3] = this.colors[i * 3];
      colors[index * 3 + 1] = this.colors[i * 3 + 1];
      colors[index * 3 + 2] = this.colors[i * 3 + 2];
    }

    this.mesh.geometry.setAttribute('color', new THREE.BufferAttribute(colors, 3));
    this.mesh.geometry.attributes.color.needsUpdate = true;
    this.mesh.material.vertexColors = true;
    this.mesh.material.needsUpdate = true;
  }

  createMesh() {
    const mesh = super.createMesh();
    mesh.material.vertexColors = true;
    this.updateColors();
    return mesh;
  }
}



