import * as THREE from 'three';
import { NeuroSurfaceViewer } from './NeuroSurfaceViewer';
import { SurfaceGeometry, NeuroSurface, ColorMappedNeuroSurface, VertexColoredNeuroSurface } from './classes';
import { debugLog, setDebug } from './debug.js';

// Export the classes so they're available to the widget
export {
  NeuroSurfaceViewer,
  SurfaceGeometry,
  NeuroSurface,
  ColorMappedNeuroSurface,
  VertexColoredNeuroSurface,
  THREE,
  debugLog,
  setDebug
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
    THREE,
    debugLog,
    setDebug
  };
}
debugLog('Neurosurface module initialized');
