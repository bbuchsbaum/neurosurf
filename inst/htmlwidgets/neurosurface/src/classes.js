import * as THREE from 'three';
import ColorMap from './ColorMap.js';
import { debugLog } from './debug.js';

export class SurfaceGeometry {
  constructor(vertices, faces, hemi, vertexCurv = null) {
    this.vertices = new Float32Array(vertices);
    this.faces = new Uint32Array(faces);
    this.hemi = hemi;
    this.vertexCurv = vertexCurv ? new Float32Array(vertexCurv) : null;
    this.mesh = null;
    this.hemisphere = hemi; // Add hemisphere property for viewer

    debugLog('SurfaceGeometry constructor called');
    debugLog('Vertices:', this.vertices.length);
    debugLog('Faces:', this.faces.length);
    debugLog('Hemi:', this.hemi);

    this.createMesh();
  }

  createMesh() {
    try {
      const geometry = new THREE.BufferGeometry();
      geometry.setAttribute('position', new THREE.Float32BufferAttribute(this.vertices, 3));
      geometry.setIndex(new THREE.Uint32BufferAttribute(this.faces, 1));
      if (this.vertexCurv) {
        geometry.setAttribute('curv', new THREE.Float32BufferAttribute(this.vertexCurv, 1));
      }
    
    const material = new THREE.MeshPhongMaterial({
      color: 0xA9A9A9, // Set default color to dark gray
      flatShading: false,
      vertexColors: false
    });

      this.mesh = new THREE.Mesh(geometry, material);
      debugLog('SurfaceGeometry construction complete');
      debugLog('Mesh:', this.mesh);
    } catch (error) {
      console.error('Error creating mesh:', error);
      throw error;
    }
  }

  dispose() {
    if (this.mesh) {
      if (this.mesh.geometry) {
        this.mesh.geometry.dispose();
      }
      if (this.mesh.material) {
        this.mesh.material.dispose();
      }
      this.mesh = null;
    }
    this.vertices = null;
    this.faces = null;
    this.vertexCurv = null;
  }
}

export class NeuroSurface {
  constructor(geometry, indices, data, config = {}) {
    this.geometry = geometry;
    this.indices = new Uint32Array(indices);
    this.data = new Float32Array(data);
    this.vertexCurv = geometry.vertexCurv || null;
    this.mesh = null;
    this.threshold = config.thresh || [0, 0];
    this.irange = config.irange || [Math.min(...data), Math.max(...data)];
    this.hemisphere = geometry.hemisphere; // Pass through hemisphere

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
    if (this.vertexCurv) {
      geometry.setAttribute('curv', new THREE.Float32BufferAttribute(this.vertexCurv, 1));
    }
    
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
    
    // Get or create color attribute
    let colorAttribute = this.mesh.geometry.getAttribute('color');
    let colors;
    
    if (!colorAttribute || colorAttribute.array.length !== vertexCount * componentsPerColor) {
      // Create new buffer only if it doesn't exist or has wrong size
      colors = new Float32Array(vertexCount * componentsPerColor);
      colorAttribute = new THREE.BufferAttribute(colors, componentsPerColor);
      this.mesh.geometry.setAttribute('color', colorAttribute);
    } else {
      // Reuse existing buffer
      colors = colorAttribute.array;
    }

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

    // Mark the attribute as needing update
    colorAttribute.needsUpdate = true;
    this.mesh.material.vertexColors = true;
    this.mesh.material.transparent = this.colorMap.hasAlpha;
    this.mesh.material.needsUpdate = true;
  }

  dispose() {
    if (this.mesh) {
      if (this.mesh.geometry) {
        this.mesh.geometry.dispose();
      }
      if (this.mesh.material) {
        if (Array.isArray(this.mesh.material)) {
          this.mesh.material.forEach(mat => mat.dispose());
        } else {
          this.mesh.material.dispose();
        }
      }
      this.mesh = null;
    }
    
    // Don't dispose geometry as it's shared
    this.geometry = null;
    this.indices = null;
    this.data = null;
    this.vertexCurv = null;
  }
}

export class ColorMappedNeuroSurface extends NeuroSurface {
  constructor(geometry, indices, data, colorMap, config = {}) {
    super(geometry, indices, data, config);
    
    this.colorMap = null;
    this.rangeListener = null;
    this.thresholdListener = null;
    this.alphaListener = null;
    this.viewer = null; // Will be set by viewer when added

    this.createMesh();  // Create the mesh first
    if (colorMap) {
      this.setColorMap(colorMap);  // Set the color map and update colors
    }
  }

  setColorMap(colorMap) {
    // Clean up old listeners if they exist
    this.removeColorMapListeners();

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
      debugLog('ColorMappedNeuroSurface: Received rangeChanged event', range);
      this.irange = range;
      this.updateColors();
    });
    this.thresholdListener = this.colorMap.on('thresholdChanged', (threshold) => {
      debugLog('ColorMappedNeuroSurface: Received thresholdChanged event', threshold);
      this.threshold = threshold;
      this.updateColors();
    });
    this.alphaListener = this.colorMap.on('alphaChanged', (alpha) => {
      debugLog('ColorMappedNeuroSurface: Received alphaChanged event', alpha);
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
    debugLog('Updating colors. Mesh:', !!this.mesh, 'ColorMap:', !!this.colorMap);
    if (!this.mesh || !this.colorMap) {
      console.warn('Mesh or ColorMap not initialized in updateColors');
      debugLog('Mesh:', this.mesh);
      debugLog('ColorMap:', this.colorMap);
      return;
    }
    
    // Safety check for geometry
    if (!this.mesh.geometry) {
      console.warn('Mesh geometry not initialized');
      return;
    }

    const vertexCount = this.geometry.vertices.length / 3;
    const componentsPerColor = 4; // Always use RGBA
    
    // Get or create color attribute
    let colorAttribute = this.mesh.geometry.getAttribute('color');
    let colors;
    
    if (!colorAttribute || colorAttribute.array.length !== vertexCount * componentsPerColor) {
      // Create new buffer only if it doesn't exist or has wrong size
      colors = new Float32Array(vertexCount * componentsPerColor);
      colorAttribute = new THREE.BufferAttribute(colors, componentsPerColor);
      this.mesh.geometry.setAttribute('color', colorAttribute);
    } else {
      // Reuse existing buffer
      colors = colorAttribute.array;
    }

    debugLog('threshold', this.threshold);
    debugLog('irange', this.irange);
    debugLog('alpha', this.config.alpha);
    debugLog('data', this.data);

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

    // Mark the attribute as needing update
    colorAttribute.needsUpdate = true;
    this.mesh.material.vertexColors = true;
    this.mesh.material.needsUpdate = true;

    // Ensure transparency is set correctly
    this.mesh.material.transparent = true;
    this.mesh.material.opacity = 1; // We're using per-vertex color blending now
    
    // Request a render if we have access to the viewer
    if (this.viewer && this.viewer.requestRender) {
      this.viewer.requestRender();
    }
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

  removeColorMapListeners() {
    if (this.colorMap) {
      // Remove listeners using stored references
      if (this.rangeListener) {
        this.colorMap.off('rangeChanged', this.rangeListener);
        this.rangeListener = null;
      }
      if (this.thresholdListener) {
        this.colorMap.off('thresholdChanged', this.thresholdListener);
        this.thresholdListener = null;
      }
      if (this.alphaListener) {
        this.colorMap.off('alphaChanged', this.alphaListener);
        this.alphaListener = null;
      }
    }
  }

  dispose() {
    // Remove event listeners first
    this.removeColorMapListeners();
    
    // Call parent dispose
    super.dispose();
    
    // Clean up color map reference
    this.colorMap = null;
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

    const vertexCount = this.geometry.vertices.length / 3;
    const componentsPerColor = 3;
    
    // Get or create color attribute
    let colorAttribute = this.mesh.geometry.getAttribute('color');
    let colors;
    
    if (!colorAttribute || colorAttribute.array.length !== vertexCount * componentsPerColor) {
      // Create new buffer only if it doesn't exist or has wrong size
      colors = new Float32Array(vertexCount * componentsPerColor);
      colorAttribute = new THREE.BufferAttribute(colors, componentsPerColor);
      this.mesh.geometry.setAttribute('color', colorAttribute);
    } else {
      // Reuse existing buffer
      colors = colorAttribute.array;
    }
    
    // Update colors in place
    for (let i = 0; i < this.indices.length; i++) {
      const index = this.indices[i];
      colors[index * 3] = this.colors[i * 3];
      colors[index * 3 + 1] = this.colors[i * 3 + 1];
      colors[index * 3 + 2] = this.colors[i * 3 + 2];
    }

    // Mark the attribute as needing update
    colorAttribute.needsUpdate = true;
    this.mesh.material.vertexColors = true;
    this.mesh.material.needsUpdate = true;
  }

  createMesh() {
    const mesh = super.createMesh();
    mesh.material.vertexColors = true;
    this.updateColors();
    return mesh;
  }

  dispose() {
    // Clean up colors array
    this.colors = null;
    
    // Call parent dispose
    super.dispose();
  }
}



