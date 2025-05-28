HTMLWidgets.widget({

  name: 'surfwidget',

  type: 'output',

  factory: function(el, width, height) {
    var viewer;
    var surfaceId = 'main'; // Default ID for single surface

    function cleanup() {
      if (viewer && typeof viewer.dispose === 'function') {
        viewer.dispose();
        viewer = null;
        while (el.firstChild) {
          el.removeChild(el.firstChild);
        }
      }
    }

    var instance = {

      renderValue: function(x) {
        // dispose a previous viewer before creating a new one
        cleanup();

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

        try {
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
          viewer.animate();
          
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
      },

      // Clean up viewer resources
      destroy: cleanup
    };
    el.surfwidget = instance;
    return instance;
  },

  onBeforeDestroy: function(el) {
    if (el && el.surfwidget && typeof el.surfwidget.destroy === 'function') {
      el.surfwidget.destroy();
    }
  }
});
