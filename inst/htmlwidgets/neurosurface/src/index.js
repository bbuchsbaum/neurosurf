import * as THREE from 'three';
import { NeuroSurfaceViewer } from './NeuroSurfaceViewer';
import { SurfaceGeometry, NeuroSurface, ColorMappedNeuroSurface, VertexColoredNeuroSurface } from './classes';

// Export the classes so they're available to the widget
export {
  NeuroSurfaceViewer,
  SurfaceGeometry,
  NeuroSurface,
  ColorMappedNeuroSurface,
  VertexColoredNeuroSurface,
  THREE
};

// Optionally, you can also attach these to the global window object
// This can be useful if you need to access these classes directly in the browser
if (typeof window !== 'undefined') {
  window.neurosurface = {
    NeuroSurfaceViewer,
    SurfaceGeometry,
    NeuroSurface,
    ColorMappedNeuroSurface,
    VertexColoredNeuroSurface,
    THREE
  };
}

console.log('Neurosurface module initialized');
