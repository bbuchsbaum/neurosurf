import * as THREE from 'three';
import { TrackballControls } from 'three/examples/jsm/controls/TrackballControls.js';
import { ColorMappedNeuroSurface, VertexColoredNeuroSurface } from './classes.js';
import { Pane } from 'tweakpane';
import * as EssentialsPlugin from '@tweakpane/plugin-essentials';

export class NeuroSurfaceViewer {
  constructor(container, width, height, config = {}, viewpoint = 'lateral') {
    this.container = container;
    this.width = width;
    this.height = height;
    this.config = {
      ambientLightColor: 0x404040,
      directionalLightColor: 0xffffff,
      directionalLightIntensity: 0.5,
      rotationSpeed: 2,
      initialZoom: 4,
      ...config
    };
    this.viewpoint = viewpoint;

    this.scene = new THREE.Scene();
    this.camera = new THREE.PerspectiveCamera(75, this.width / this.height, 0.1, 1000);
    this.renderer = new THREE.WebGLRenderer({ antialias: true });

    this.setupRenderer();
    this.setupCamera();
    this.setupLighting();
    this.setupControls();

    this.surfaces = new Map(); // Store multiple surfaces

    this.raycaster = new THREE.Raycaster();
    this.mouse = new THREE.Vector2();
    this.intersectionPoint = new THREE.Vector3();

    this.dataRange = { min: 0, max: 500 }; // Initialize to default values
    this.intensityRange = { range: { min: 0, max: 500 } };
    this.thresholdRange = { range: { min: 0, max: 0 } }; // Set default threshold to [0, 0]

    this.viewpoints = {
      left_lateral: new THREE.Matrix4().set(
        0, 0, -1, 0,
        -1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 0, 1
      ),
      left_medial: new THREE.Matrix4().set(
        0, 0, 1, 0,
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 0, 1
      ),
      left_ventral: new THREE.Matrix4().set(
        -1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, -1, 0,
        0, 0, 0, 1
      ),
      left_posterior: new THREE.Matrix4().set(
        1, 0, 0, 0,
        0, 0, -1, 0,
        0, 1, 0, 0,
        0, 0, 0, 1
      ),
      right_lateral: new THREE.Matrix4().set(
        0, 0, 1, 0,
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 0, 1
      ),
      right_medial: new THREE.Matrix4().set(
        0, 0, -1, 0,
        -1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 0, 1
      ),
      right_ventral: new THREE.Matrix4().set(
        -1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, -1, 0,
        0, 0, 0, 1
      ),
      right_posterior: new THREE.Matrix4().set(
        1, 0, 0, 0,
        0, 0, -1, 0,
        0, 1, 0, 0,
        0, 0, 0, 1
      )
    };

    this.setupTweakPane();
    this.setViewpoint(this.viewpoint); // Set the initial viewpoint
    this.centerCamera(); // Center the camera after setting the viewpoint

    this.animate = this.animate.bind(this);
    this.animate();
  }

  setupRenderer() {
    this.renderer.setClearColor(0xffffff);  // Set to white (0xffffff)
    this.renderer.setSize(this.width, this.height);
    this.renderer.setPixelRatio(window.devicePixelRatio);
    this.container.appendChild(this.renderer.domElement);
  }

  setupCamera() {
    this.camera = new THREE.PerspectiveCamera(45, this.width / this.height, 0.1, 2000);
    this.camera.position.z = 500; // Start further away
    this.camera.lookAt(new THREE.Vector3(0, 0, 0));
  }

  setupLighting() {
    this.ambientLight = new THREE.AmbientLight(this.config.ambientLightColor);
    this.scene.add(this.ambientLight);

    this.directionalLight = new THREE.DirectionalLight(
      this.config.directionalLightColor,
      this.config.directionalLightIntensity
    );
    this.directionalLight.position.set(1, 1, 1).normalize();
    this.scene.add(this.directionalLight);

    this.light = new THREE.PointLight(0xFFFFFF);
    this.light.position.set(0, 0, 500);
    this.scene.add(this.light);
  }

  setupControls() {
    this.controls = new TrackballControls(this.camera, this.renderer.domElement);
    this.controls.rotateSpeed = this.config.rotationSpeed;
    this.controls.zoomSpeed = 1.2;
    this.controls.panSpeed = 0.8;
    this.controls.noZoom = false;
    this.controls.noPan = false;
    this.controls.staticMoving = true;
    this.controls.dynamicDampingFactor = 0.3;
  }

  setupTweakPane() {
    // Create a container for Tweakpane
    const paneContainer = document.createElement('div');
    paneContainer.style.position = 'absolute';
    paneContainer.style.top = '10px';
    paneContainer.style.right = '10px';
    paneContainer.style.width = '250px';
    paneContainer.style.zIndex = '1000';
    this.container.appendChild(paneContainer);

    this.pane = new Pane({
      container: paneContainer,
      title: 'NeuroSurface Controls',
      expanded: true,
    });

    // Register the essentials plugin
    this.pane.registerPlugin(EssentialsPlugin);

    // Set the width of the Tweakpane
    this.pane.element.style.width = '100%';

    // Lighting folder
    const lightingFolder = this.pane.addFolder({
      title: 'Lighting',
      expanded: false,
    });

    lightingFolder.addBinding(this.config, 'ambientLightColor', {
      label: 'Ambient Light',
      view: 'color',
    }).on('change', (ev) => {
      this.updateAmbientLight(ev.value);
    });

    lightingFolder.addBinding(this.config, 'directionalLightColor', {
      label: 'Directional Light',
      view: 'color',
    }).on('change', (ev) => {
      this.updateDirectionalLight(ev.value);
    });

    lightingFolder.addBinding(this.config, 'directionalLightIntensity', {
      label: 'Light Intensity',
      min: 0,
      max: 1,
      step: 0.01,
    }).on('change', (ev) => {
      this.updateDirectionalLightIntensity(ev.value);
    });

    // Camera folder
    const cameraFolder = this.pane.addFolder({
      title: 'Camera',
      expanded: false,
    });

    cameraFolder.addButton({
      title: 'Reset Camera',
    }).on('click', () => {
      this.resetCamera();
    });

    // Color Map folder
    const colorMapFolder = this.pane.addFolder({
      title: 'Color Map',
      expanded: true,
    });

    // Add intensity range slider
    this.intensityRangeControl = colorMapFolder.addBinding(this.intensityRange, 'range', {
      label: 'Intensity Range',
      min: this.dataRange.min,
      max: this.dataRange.max,
      step: 0.01,
    }).on('change', this.updateIntensityRange.bind(this));

    // Add threshold range slider
    this.thresholdRangeControl = colorMapFolder.addBinding(this.thresholdRange, 'range', {
      label: 'Threshold Range',
      min: this.dataRange.min,
      max: this.dataRange.max,
      step: 0.01,
    }).on('change', this.updateThresholdRange.bind(this));

    // Viewpoint folder
    const viewpointFolder = this.pane.addFolder({
      title: 'Viewpoint',
      expanded: false,
    });

    this.viewpointState = { viewpoint: this.viewpoint };
    viewpointFolder.addBinding(this.viewpointState, 'viewpoint', {
      label: 'Viewpoint',
      options: {
        lateral: 'lateral',
        medial: 'medial',
        ventral: 'ventral',
        posterior: 'posterior',
      },
    }).on('change', (ev) => {
      this.setViewpoint(ev.value);
    });
  }

  setViewpoint(viewpoint) {
    let umat;
    let hemisphere = 'left'; // Default to left hemisphere

    // Check if there are any surfaces and if the first one has a hemisphere property
    const firstSurface = this.surfaces.values().next().value;
    if (firstSurface && firstSurface.hemisphere) {
      hemisphere = firstSurface.hemisphere;
    }

    const fullViewpoint = `${hemisphere}_${viewpoint}`;

    if (this.viewpoints[fullViewpoint]) {
      umat = this.viewpoints[fullViewpoint];
    } else {
      console.warn(`Unknown viewpoint: ${fullViewpoint}`);
      umat = new THREE.Matrix4().identity();
    }

    console.log('Setting viewpoint to:', viewpoint);
    console.log('Current viewpoint state:', this.viewpointState);
    console.log("umat", umat);

    // Calculate the bounding box of all surfaces
    const box = new THREE.Box3();
    this.surfaces.forEach(surface => {
      box.expandByObject(surface.mesh);
    });
    const center = box.getCenter(new THREE.Vector3());
    const size = box.getSize(new THREE.Vector3());
    const maxDim = Math.max(size.x, size.y, size.z);

    // Set camera position based on umat
    const distance = maxDim * this.config.initialZoom; // Adjust this multiplier as needed
    const cameraPosition = new THREE.Vector3(0, 0, distance).applyMatrix4(umat);
    this.camera.position.copy(cameraPosition.add(center));

    // Set camera up vector
    const up = new THREE.Vector3(0, 1, 0).applyMatrix4(umat);
    this.camera.up.copy(up);

    // Set camera look at
    this.camera.lookAt(center);

    // Update controls
    this.controls.target.copy(center);
    this.controls.update();

    this.viewpoint = viewpoint;
    if (this.viewpointState) {
      this.viewpointState.viewpoint = viewpoint;
    }
   
    this.render();
  }

  updateColormap(presetName) {
    this.surfaces.forEach(surface => {
      if (surface instanceof ColorMappedNeuroSurface) {
        surface.setColorMap(ColorMap.fromPreset(presetName));
        surface.updateColors();
      }
    });
    this.render();
  }

  updateAmbientLight(color) {
    if (this.ambientLight) {
      this.ambientLight.color.setHex(color);
      this.render();
    }
  }

  updateDirectionalLight(color) {
    if (this.directionalLight) {
      this.directionalLight.color.setHex(color);
      this.render();
    }
  }

  updateDirectionalLightIntensity(intensity) {
    if (this.directionalLight) {
      this.directionalLight.intensity = intensity;
      this.render();
    }
  }

  updateIntensityRange() {
    console.log('NeuroSurfaceViewer: Updating intensity range', [this.intensityRange.range.min, this.intensityRange.range.max]);
    this.surfaces.forEach(surface => {
      if (surface instanceof ColorMappedNeuroSurface) {
        surface.colorMap.setRange([this.intensityRange.range.min, this.intensityRange.range.max]);
      }
    });
    // The surface should update automatically due to the event listener
  }

  updateThresholdRange() {
    console.log('NeuroSurfaceViewer: Updating threshold range', [this.thresholdRange.range.min, this.thresholdRange.range.max]);
    this.surfaces.forEach(surface => {
      if (surface instanceof ColorMappedNeuroSurface) {
        surface.colorMap.setThreshold([this.thresholdRange.range.min, this.thresholdRange.range.max]);
      }
    });
    // The surface should update automatically due to the event listener
  }

  resetCamera() {
    if (this.camera && this.controls) {
      this.setViewpoint(this.viewpoint); // Reset to the current viewpoint
      this.render();
    }
  }

  addSurface(surface, id) {
    console.log('Adding surface:', surface, 'with id:', id);
    this.surfaces.set(id, surface);
    if (!surface.mesh) {
      console.warn('Surface mesh not created. Creating now.');
      surface.createMesh();
    }
    this.scene.add(surface.mesh);
    
    if (surface instanceof ColorMappedNeuroSurface) {
      console.log('Updating data range for ColorMappedNeuroSurface');
      this.updateDataRange(surface.data);
      this.updateRangeControls();
    }
    
    if (this.surfaces.size === 1) {
      this.resetCamera(); // Center the camera when the first surface is added
    }
    this.render();
  }

  updateDataRange(data) {
    const min = Math.min(...data);
    const max = Math.max(...data);
    
    this.dataRange.min = min;
    this.dataRange.max = max;

    // Update intensity range to match data range
    this.intensityRange.range.min = min;
    this.intensityRange.range.max = max;

    // Keep threshold range at [0, 0]
    this.thresholdRange.range.min = 0;
    this.thresholdRange.range.max = 0;

    this.updateRangeControls();
  }

  updateRangeControls() {
    // Update intensity range control
    this.intensityRangeControl.min = this.dataRange.min;
    this.intensityRangeControl.max = this.dataRange.max;
    this.intensityRangeControl.value = this.intensityRange.range;

    // Update threshold range control
    this.thresholdRangeControl.min = this.dataRange.min;
    this.thresholdRangeControl.max = this.dataRange.max;
    this.thresholdRangeControl.value = this.thresholdRange.range;

    // Refresh the controls
    this.intensityRangeControl.refresh();
    this.thresholdRangeControl.refresh();
  }

  removeSurface(id) {
    const surface = this.surfaces.get(id);
    if (surface) {
      this.scene.remove(surface.mesh);
      this.surfaces.delete(id);
    }
  }

  onWindowResize(width, height) {
    this.width = width;
    this.height = height;
    this.camera.aspect = this.width / this.height;
    this.camera.updateProjectionMatrix();
    this.renderer.setSize(this.width, this.height);
  }

  animate() {
    requestAnimationFrame(this.animate);
    this.controls.update();
    this.render();
  }

  render() {
    if (this.renderer && this.scene && this.camera) {
      this.renderer.render(this.scene, this.camera);
    }
  }

  centerCamera() {
    if (this.surfaces.size === 0) return; // No surfaces to center on

    const box = new THREE.Box3();
    this.surfaces.forEach(surface => {
      box.expandByObject(surface.mesh);
    });

    const center = box.getCenter(new THREE.Vector3());
    const size = box.getSize(new THREE.Vector3());

    const maxDim = Math.max(size.x, size.y, size.z);
    const fov = this.camera.fov * (Math.PI / 180);
    let cameraDistance = Math.abs(maxDim / 2 / Math.tan(fov / 2));

    cameraDistance *= this.config.initialZoom; // Add some distance for better view
    
    const direction = this.camera.position.clone().sub(this.controls.target).normalize();
    this.camera.position.copy(center.clone().add(direction.multiplyScalar(cameraDistance)));
    this.controls.target.copy(center);

    this.camera.near = cameraDistance / 100;
    this.camera.far = cameraDistance * 100;
    this.camera.updateProjectionMatrix();

    this.controls.update();
  }

  // Add this method to the NeuroSurfaceViewer class

  setData(id, newData) {
    const surface = this.surfaces.get(id);
    if (surface && surface instanceof ColorMappedNeuroSurface) {
      surface.setData(newData);
      this.updateDataRange(newData);
      this.updateRangeControls();
      surface.updateColors();
      this.render();
      console.log(`Updated data for surface with id: ${id}`);
    } else {
      console.warn(`No ColorMappedNeuroSurface found with id: ${id}`);
    }
  }

  setVertexColors(id, colors) {
    const surface = this.surfaces.get(id);
    if (surface && surface instanceof VertexColoredNeuroSurface) {
      surface.setColors(colors);
      this.render();
      console.log(`Updated colors for surface with id: ${id}`);
    } else {
      console.warn(`No VertexColoredNeuroSurface found with id: ${id}`);
    }
  }

  setRotationSpeed(speed) {
    this.config.rotationSpeed = speed;
    if (this.controls) {
      this.controls.rotateSpeed = speed;
    }
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
}
