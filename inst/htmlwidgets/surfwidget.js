'use strict';

HTMLWidgets.widget({

  name: 'surfwidget',

  type: 'output',

  factory: function(el, width, height) {
    let viewer;
    const surfaceId = 'main'; // Default ID for single surface

    return {

      renderValue: function(x) {
        if (!viewer) {
          neurosurface.debugLog('Creating NeuroSurfaceViewer');
          viewer = new neurosurface.NeuroSurfaceViewer(el, width, height, {
            ...x.config, 
            cmap: x.cmap,
            rotationSpeed: 2.5, // Increase rotation speed
            initialZoom: 2.5 // Increase initial zoom
          }, x.viewpoint);

          // Use the new methods to set rotation speed and initial zoom
          //viewer.setRotationSpeed(2.5);
          //viewer.setInitialZoom(2.5);
        }

        try {
          // Validate input arrays before constructing geometry
          const validateNumericArray = function(arr, name, multipleOf, expectedLength) {
            if (!Array.isArray(arr) || !arr.every(v => typeof v === 'number')) {
              throw new Error(`${name} must be an array of numbers`);
            }
            if (arr.length === 0) {
              throw new Error(`${name} cannot be empty`);
            }
            if (multipleOf && arr.length % multipleOf !== 0) {
              throw new Error(`${name} length must be a multiple of ${multipleOf}`);
            }
            if (expectedLength !== undefined && arr.length !== expectedLength) {
              throw new Error(`${name} length must be ${expectedLength}`);
            }
          };

          validateNumericArray(x.vertices, 'x.vertices', 3);
          validateNumericArray(x.faces, 'x.faces', 3);
          validateNumericArray(x.indices, 'x.indices');

          if (x.cmap) {
            validateNumericArray(x.data, 'x.data', undefined, x.indices.length);
          }
          if (x.vertexColors) {
            validateNumericArray(x.vertexColors, 'x.vertexColors', undefined, x.indices.length);
          }

          neurosurface.debugLog('Creating SurfaceGeometry');
          var geometry = new neurosurface.SurfaceGeometry(x.vertices, x.faces, x.hemi);
          neurosurface.debugLog('SurfaceGeometry created:', geometry);

          var surface;
          if (x.cmap) {
            neurosurface.debugLog('Creating ColorMappedNeuroSurface');
            surface = new neurosurface.ColorMappedNeuroSurface(
              geometry, 
              x.indices,
              x.data,
              x.cmap,
              { irange: x.irange, thresh: x.thresh, alpha: x.alpha, ...x.config }
            );
          } else if (x.vertexColors) {
            neurosurface.debugLog('Creating VertexColoredNeuroSurface');
            surface = new neurosurface.VertexColoredNeuroSurface(
              geometry, 
              x.indices,
              x.vertexColors,
              { alpha: x.alpha || 1, ...x.config }
            );
          } else {
            throw new Error("Neither color map nor vertex colors provided");
          }

          neurosurface.debugLog('Surface created:', surface);
          neurosurface.debugLog('Adding surface to viewer');
          viewer.addSurface(surface, surfaceId);
          if (viewer.animationId === null) {
            viewer.animate();
          }
          
        } catch (error) {
          console.error("Error in renderValue:", error);
        }
      },

      resize: function(width, height) {
        if (viewer) {
          viewer.onWindowResize(width, height);
        }
      },

      // Expose methods for R to call
      setColorMap: function(colorMap) {
        if (viewer) viewer.updateSurface(surfaceId, 'colorMap', colorMap);
      },

      setIRange: function(min, max) {
        if (viewer) viewer.updateSurface(surfaceId, 'irange', [min, max]);
      },

      setThreshold: function(min, max) {
        if (viewer) viewer.updateSurface(surfaceId, 'threshold', [min, max]);
      },

      setVertexColors: function(colors) {
        if (viewer) viewer.updateSurface(surfaceId, 'vertexColors', colors);
      },

      updateConfig: function(config) {
        if (viewer) viewer.updateSurface(surfaceId, 'config', config);
      },

      setRotationSpeed: function(speed) {
        if (viewer) viewer.setRotationSpeed(speed);
      },

      setZoom: function(zoom) {
        if (viewer) viewer.setInitialZoom(zoom);
      }
    };
  }
});

if (HTMLWidgets.shinyMode) {
  Shiny.addCustomMessageHandler('neurosurf-surfwidget-config', function(message) {
    var widget = HTMLWidgets.find('#' + message.id);
    if (widget && widget.updateConfig) {
      widget.updateConfig(message.config);
    }
  });
}
