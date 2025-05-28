import * as THREE from 'three';
import { TrackballControls } from 'three/examples/jsm/controls/TrackballControls.js';
import { EffectComposer } from 'three/examples/jsm/postprocessing/EffectComposer.js';
import { RenderPass } from 'three/examples/jsm/postprocessing/RenderPass.js';
import { SSAOPass } from 'three/examples/jsm/postprocessing/SSAOPass.js';
import { RGBELoader } from 'three/examples/jsm/loaders/RGBELoader.js';
import { ColorMappedNeuroSurface, VertexColoredNeuroSurface } from './classes.js';
import { Pane } from 'tweakpane';
import * as EssentialsPlugin from '@tweakpane/plugin-essentials';
import { debugLog } from './debug.js';

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
      ssaoRadius: 4,
      ssaoKernelSize: 32,
      rimStrength: 0,
      metalness: 0.1,
      roughness: 0.6,
      useShaders: true,
      ...config
    };
    this.viewpoint = viewpoint;

    this.scene = new THREE.Scene();
    this.environmentMap = null;
    this.camera = new THREE.PerspectiveCamera(75, this.width / this.height, 0.1, 1000);
    this.renderer = new THREE.WebGLRenderer({ antialias: true });

    this.setupRenderer();
    this.setupCamera();
    this.setupLighting();
    this.setupEnvironment();
    this.setupControls();
    this.setupPicking();
    this.setupPostProcessing();

    this.surfaces = new Map(); // Store multiple surfaces
    this.rimStrengthUniforms = [];

    this.raycaster = new THREE.Raycaster();
    this.mouse = new THREE.Vector2();
    this.intersectionPoint = new THREE.Vector3();

    this.animationId = null; // Store animation frame id for cleanup
    this.paneContainer = null; // Reference to tweakpane container

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

  setupEnvironment() {
    new RGBELoader().load('assets/studio_small_09_2k.hdr', (tex) => {
      tex.mapping = THREE.EquirectangularReflectionMapping;
      this.environmentMap = tex;
      this.scene.environment = tex;
      this.updateMaterials();
    });
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

  setupPostProcessing() {
    this.composer = new EffectComposer(this.renderer);
    this.renderPass = new RenderPass(this.scene, this.camera);
    this.composer.addPass(this.renderPass);

    this.ssaoPass = new SSAOPass(this.scene, this.camera, this.width, this.height);
    this.ssaoPass.kernelRadius = this.config.ssaoRadius;
    this.ssaoPass.kernelSize = this.config.ssaoKernelSize;
    this.composer.addPass(this.ssaoPass);
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
    this.paneContainer = paneContainer;

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

    lightingFolder.addBinding(this.config, 'rimStrength', {
      label: 'Rim Strength',
      min: 0,
      max: 1,
      step: 0.01,
    }).on('change', (ev) => {
      this.updateRimStrength(ev.value);
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


    // SSAO folder
    const ssaoFolder = this.pane.addFolder({
      title: 'SSAO',
      expanded: false,
    });

    ssaoFolder.addBinding(this.config, 'ssaoRadius', {
      label: 'Radius',
      min: 0,
      max: 32,
      step: 0.1,
    }).on('change', (ev) => {
      if (this.ssaoPass) {
        this.ssaoPass.kernelRadius = ev.value;
        this.render();
      }
    });

    ssaoFolder.addBinding(this.config, 'ssaoKernelSize', {
      label: 'Kernel Size',
      min: 8,
      max: 64,
      step: 1,
    }).on('change', (ev) => {
      if (this.ssaoPass) {
        this.ssaoPass.kernelSize = ev.value;
        if (typeof this.ssaoPass.generateSampleKernel === 'function') {
          this.ssaoPass.generateSampleKernel();
        }
        this.render();
      }

    // Material folder
    const materialFolder = this.pane.addFolder({
      title: 'Material',
      expanded: false,
    });

    materialFolder.addBinding(this.config, 'metalness', {
      label: 'Metalness',
      min: 0,
      max: 1,
      step: 0.01,
    }).on('change', () => {
      this.updateMaterials();
    });

    materialFolder.addBinding(this.config, 'roughness', {
      label: 'Roughness',
      min: 0,
      max: 1,
      step: 0.01,
    }).on('change', () => {
      this.updateMaterials();
    });

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

    debugLog('Setting viewpoint to:', viewpoint);
    debugLog('Current viewpoint state:', this.viewpointState);
    debugLog('umat', umat);

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

  updateMaterials() {
    this.surfaces.forEach(surface => {
      if (!surface.mesh) return;
      if (!surface.mesh.material || !surface.mesh.material.isMeshPhysicalMaterial) {
        surface.mesh.material = new THREE.MeshPhysicalMaterial({
          metalness: this.config.metalness,
          roughness: this.config.roughness,
          clearcoat: 0.2,
          reflectivity: 0.45,
          vertexColors: true,
          envMapIntensity: 0.8,
          dithering: true,
          side: THREE.DoubleSide
        });
      }
      surface.mesh.material.metalness = this.config.metalness;
      surface.mesh.material.roughness = this.config.roughness;
      surface.mesh.geometry.computeVertexNormals();
      surface.mesh.material.needsUpdate = true;
    });
    this.render();
  }

  updateIntensityRange() {
    debugLog('NeuroSurfaceViewer: Updating intensity range', [this.intensityRange.range.min, this.intensityRange.range.max]);
    this.surfaces.forEach(surface => {
      if (surface instanceof ColorMappedNeuroSurface) {
        surface.colorMap.setRange([this.intensityRange.range.min, this.intensityRange.range.max]);
      }
    });
    // The surface should update automatically due to the event listener
  }

  updateThresholdRange() {
    debugLog('NeuroSurfaceViewer: Updating threshold range', [this.thresholdRange.range.min, this.thresholdRange.range.max]);
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
    debugLog('Adding surface:', surface, 'with id:', id);
    this.surfaces.set(id, surface);
    if (!surface.mesh) {
      console.warn('Surface mesh not created. Creating now.');
      surface.createMesh();
    }
    if (surface.mesh && surface.mesh.material) {
      this.applyRimLighting(surface.mesh.material);
      this.applyCurvatureShading(surface.mesh);
    }
    this.scene.add(surface.mesh);
    this.updateMaterials();
    
    if (surface instanceof ColorMappedNeuroSurface) {
      debugLog('Updating data range for ColorMappedNeuroSurface');
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
    if (this.composer && this.ssaoPass) {
      this.composer.setSize(this.width, this.height);
      this.ssaoPass.setSize(this.width, this.height);
    }
  }

  animate() {
    this.animationId = requestAnimationFrame(this.animate);
    this.controls.update();
    this.render();
  }

  render() {
    if (this.composer) {
      this.composer.render();
    } else if (this.renderer && this.scene && this.camera) {
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
      debugLog(`Updated data for surface with id: ${id}`);
    } else {
      console.warn(`No ColorMappedNeuroSurface found with id: ${id}`);
    }
  }

  setVertexColors(id, colors) {
    const surface = this.surfaces.get(id);
    if (surface && surface instanceof VertexColoredNeuroSurface) {
      surface.setColors(colors);
      this.render();
      debugLog(`Updated colors for surface with id: ${id}`);
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

  applyRimLighting(material) {
    material.onBeforeCompile = (shader) => {
      shader.uniforms.rimStrength = { value: this.config.rimStrength };
      shader.uniforms.rimColor = { value: new THREE.Color(0xffffff) };
      shader.fragmentShader = shader.fragmentShader.replace(
        'void main() {',
        'uniform vec3 rimColor;\nuniform float rimStrength;\nvoid main() {'
      );
      shader.fragmentShader = shader.fragmentShader.replace(
        '#include <output_fragment>',
        '#include <output_fragment>\nfloat f = pow(1.0 - dot(normalize(vNormal), normalize(vViewPosition)), 2.0);\ngl_FragColor.rgb += rimColor * f * rimStrength;'
      );
      this.rimStrengthUniforms.push(shader.uniforms.rimStrength);
    };
    material.needsUpdate = true;
  }

  applyCurvatureShading(mesh) {
    const curvAttr = mesh.geometry.getAttribute('curv');
    if (!curvAttr) return;

    if (this.config.useShaders && mesh.material && mesh.material.onBeforeCompile) {
      mesh.material.onBeforeCompile = (shader) => {
        shader.uniforms.warmColor = { value: new THREE.Color(0xff5533) };
        shader.uniforms.coolColor = { value: new THREE.Color(0x3366ff) };
        shader.vertexShader = shader.vertexShader.replace(
          'void main()',
          'attribute float curv;\nvarying float vCurv;\nvoid main()'
        );
        shader.vertexShader = shader.vertexShader.replace(
          '#include <begin_vertex>',
          '#include <begin_vertex>\nvCurv = curv;'
        );
        shader.fragmentShader = shader.fragmentShader.replace(
          'void main() {',
          'varying float vCurv;\nuniform vec3 warmColor;\nuniform vec3 coolColor;\nvoid main() {'
        );
        shader.fragmentShader = shader.fragmentShader.replace(
          '#include <dithering_fragment>',
          '#include <dithering_fragment>\nfloat c = smoothstep(-0.5,0.5,vCurv);\nvec3 tint = mix(coolColor,warmColor,c);\n gl_FragColor.rgb = mix(gl_FragColor.rgb, tint, 0.4);'
        );
      };
      mesh.material.needsUpdate = true;
    } else {
      const colors = mesh.geometry.getAttribute('color');
      if (!colors) return;
      const arr = colors.array;
      const curv = curvAttr.array;
      for (let i = 0; i < curv.length; i++) {
        const t = curv[i] > 0 ? [1, 0.7, 0.7] : [0.7, 0.7, 1];
        const idx = i * 3;
        arr[idx] *= t[0];
        arr[idx + 1] *= t[1];
        arr[idx + 2] *= t[2];
      }
      colors.needsUpdate = true;
    }
  }

  updateRimStrength(strength) {
    this.config.rimStrength = strength;
    this.rimStrengthUniforms.forEach(u => { u.value = strength; });
    this.render();
  }

  setupPicking() {
    this.renderer.domElement.addEventListener('click', this.onMouseClick.bind(this));
  }

  onMouseClick(event) {
    const rect = this.renderer.domElement.getBoundingClientRect();
    this.mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    this.mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;

    this.raycaster.setFromCamera(this.mouse, this.camera);
    const meshes = [];
    this.surfaces.forEach(surface => {
      if (surface.mesh) meshes.push(surface.mesh);
    });
    const intersects = this.raycaster.intersectObjects(meshes, true);
    if (intersects.length === 0) return;

    const intersect = intersects[0];
    const mesh = intersect.object;
    let surfId = null;
    let surface = null;
    for (const [id, surf] of this.surfaces.entries()) {
      if (surf.mesh === mesh) {
        surfId = id;
        surface = surf;
        break;
      }
    }
    if (!surface || !intersect.face) return;

    const face = intersect.face;
    const posAttr = surface.mesh.geometry.attributes.position;
    const verts = [face.a, face.b, face.c];
    let closestIdx = verts[0];
    let closestPos = new THREE.Vector3().fromBufferAttribute(posAttr, verts[0]);
    let minDist = closestPos.distanceTo(intersect.point);
    for (let i = 1; i < verts.length; i++) {
      const v = new THREE.Vector3().fromBufferAttribute(posAttr, verts[i]);
      const d = v.distanceTo(intersect.point);
      if (d < minDist) {
        minDist = d;
        closestIdx = verts[i];
        closestPos = v;
      }
    }

    let value = null;
    if (surface.data && surface.indices) {
      const arrayIdx = Array.from(surface.indices).indexOf(closestIdx);
      if (arrayIdx !== -1) value = surface.data[arrayIdx];
    }

    if (window.Shiny && typeof window.Shiny.setInputValue === 'function') {
      window.Shiny.setInputValue(
        'ns_vertex_click',
        { id: surfId, vertex: closestIdx, coord: [closestPos.x, closestPos.y, closestPos.z], value: value },
        { priority: 'event' }
      );
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
    if (this.composer) {
      this.composer = null;
      this.renderPass = null;
      this.ssaoPass = null;
    }
  }
}
