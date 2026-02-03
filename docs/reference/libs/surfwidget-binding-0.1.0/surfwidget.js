'use strict';

HTMLWidgets.widget({

  name: 'surfwidget',

  type: 'output',

  factory: function(el, width, height) {
    let viewer;
    let viewerContainer;
    let colorbarCanvas;
    let colorbarWrapper;
    let tweakpanePromise = null;
    const surfaceId = 'main'; // Default ID for single surface
    let lastPayload = null;   // Remember last data for resize redraw
    const COLORBAR_WIDTH = 68; // px reserved for colorbar
    const dbg = function(...args) {
      try {
        if (typeof neurosurface !== 'undefined' &&
            typeof neurosurface.debugLog === 'function') {
          return neurosurface.debugLog(...args);
        }
      } catch (e) { /* ignore */ }
      if (typeof console !== 'undefined' && console.debug) {
        console.debug(...args);
      }
    };

    // Load Tweakpane from CDN for environments without import maps (e.g. file:// pkgdown pages)
    // and cache the module on window so surfviewjs can pick it up without bare-module imports.
    const ensureTweakpane = function() {
      try {
        if (typeof window === 'undefined') return Promise.resolve(null);
        const existing = window.Tweakpane || window.tweakpane;
        if (existing && existing.Pane) return Promise.resolve(existing);
        if (tweakpanePromise) return tweakpanePromise;

        tweakpanePromise = import('https://cdn.jsdelivr.net/npm/tweakpane@4.0.4/dist/tweakpane.min.js')
          .then((mod) => {
            window.Tweakpane = mod;
            window.tweakpane = mod;
            return mod;
          })
          .catch((e) => {
            console.warn('Failed to load tweakpane from CDN:', e);
            return null;
          });
        return tweakpanePromise;
      } catch (e) {
        return Promise.resolve(null);
      }
    };

    const safeToggleControls = function(show) {
      if (!viewer || typeof viewer.toggleControls !== 'function') return;
      try {
        const out = viewer.toggleControls(!!show);
        if (out && typeof out.catch === 'function') {
          out.catch((e) => console.warn('toggleControls failed:', e));
        }
      } catch (e) {
        console.warn('toggleControls failed:', e);
      }
    };

    // Build a side-by-side layout: viewer + colorbar
    const ensureLayout = function() {
      if (viewerContainer) return viewerContainer;

      el.innerHTML = '';
      const root = document.createElement('div');
      root.style.display = 'flex';
      root.style.alignItems = 'stretch';
      root.style.width = '100%';
      root.style.height = '100%';
      root.style.boxSizing = 'border-box';

      const viewDiv = document.createElement('div');
      viewDiv.style.flex = '1 1 auto';
      viewDiv.style.position = 'relative';
      viewDiv.style.minWidth = '120px';

      const barDiv = document.createElement('div');
      barDiv.style.flex = `0 0 ${COLORBAR_WIDTH}px`;
      barDiv.style.width = `${COLORBAR_WIDTH}px`;
      barDiv.style.display = 'flex';
      barDiv.style.alignItems = 'stretch';
      barDiv.style.padding = '4px 4px';
      barDiv.style.boxSizing = 'border-box';

      const barCanvas = document.createElement('canvas');
      barCanvas.style.width = '100%';
      barCanvas.style.height = '100%';
      barCanvas.style.display = 'block';
      barCanvas.width = COLORBAR_WIDTH;
      barDiv.appendChild(barCanvas);

      root.appendChild(viewDiv);
      root.appendChild(barDiv);
      el.appendChild(root);

      viewerContainer = viewDiv;
      colorbarCanvas = barCanvas;
      colorbarWrapper = barDiv;
      return viewerContainer;
    };

    const clamp01 = function(x) {
      if (typeof x !== 'number' || !isFinite(x)) return 0;
      return Math.max(0, Math.min(1, x));
    };

    const toCssColorStop = function(c) {
      if (typeof c === 'string') return c;
      if (Array.isArray(c) && c.length >= 3) {
        const r = Math.round(clamp01(c[0]) * 255);
        const g = Math.round(clamp01(c[1]) * 255);
        const b = Math.round(clamp01(c[2]) * 255);
        const a = c.length >= 4 ? clamp01(c[3]) : 1;
        return `rgba(${r},${g},${b},${a})`;
      }
      return null;
    };

    const resolveCmapStops = function(cmap) {
      if (!cmap) return null;

      // If passed a ColorMap-like object with `.colors`, use that directly.
      if (!Array.isArray(cmap) && typeof cmap === 'object' && Array.isArray(cmap.colors)) {
        cmap = cmap.colors;
      }

      // If passed a preset name, try to resolve to a preset.
      if (typeof cmap === 'string') {
        try {
          if (typeof neurosurface !== 'undefined' &&
              neurosurface.ColorMap &&
              typeof neurosurface.ColorMap.fromPreset === 'function') {
            const cm = neurosurface.ColorMap.fromPreset(cmap);
            if (cm && Array.isArray(cm.colors)) {
              cmap = cm.colors;
            } else {
              return null;
            }
          } else {
            return null;
          }
        } catch (e) {
          return null;
        }
      }

      if (!Array.isArray(cmap) || cmap.length === 0) return null;

      // Hex strings (from R) are already valid CSS color stops.
      if (typeof cmap[0] === 'string') return cmap;

      // Numeric RGBA arrays from surfviewjs presets.
      if (Array.isArray(cmap[0])) {
        const stops = cmap.map(toCssColorStop).filter((x) => !!x);
        return stops.length ? stops : null;
      }

      return null;
    };

    const drawColorbar = function(cmap, irange, label, heightPx, show = true) {
      const stops = resolveCmapStops(cmap);
      if (!colorbarCanvas || !show || !Array.isArray(stops) || !stops.length) {
        if (colorbarWrapper) colorbarWrapper.style.display = 'none';
        return;
      }
      colorbarWrapper.style.display = 'flex';

      const dpr = window.devicePixelRatio || 1;
      const w = COLORBAR_WIDTH;
      const h = Math.max(40, heightPx || colorbarCanvas.clientHeight || 200);
      colorbarCanvas.width = Math.round(w * dpr);
      colorbarCanvas.height = Math.round(h * dpr);
      colorbarCanvas.style.width = `${w}px`;
      colorbarCanvas.style.height = `${h}px`;
      const ctx = colorbarCanvas.getContext('2d');
      ctx.setTransform(dpr, 0, 0, dpr, 0, 0);

      // gradient
      const grad = ctx.createLinearGradient(0, h, 0, 0);
      const nStops = stops.length;
      for (let i = 0; i < nStops; i++) {
        grad.addColorStop(i / (nStops - 1), stops[i]);
      }
      ctx.fillStyle = grad;
      ctx.fillRect(0, 0, w - 22, h);

      // ticks
      const range = Array.isArray(irange) && irange.length === 2 ? irange : [0, 1];
      const ticks = 5;
      ctx.fillStyle = '#111';
      ctx.strokeStyle = '#111';
      ctx.lineWidth = 1;
      ctx.font = '11px sans-serif';
      ctx.textBaseline = 'middle';
      for (let t = 0; t < ticks; t++) {
        const frac = t / (ticks - 1);
        const y = h - frac * h;
        const val = range[0] + frac * (range[1] - range[0]);
        ctx.beginPath();
        ctx.moveTo(w - 22, y);
        ctx.lineTo(w - 16, y);
        ctx.stroke();
        ctx.fillText(val.toFixed(2), w - 14, y);
      }

      if (label) {
        ctx.save();
        ctx.translate(w - 4, h / 2);
        ctx.rotate(-Math.PI / 2);
        ctx.textAlign = 'center';
        ctx.fillText(label, 0, 0);
        ctx.restore();
      }
    };

    return {

      renderValue: function(x) {
        // Compute sizes up front (function scope) so they are visible in both try blocks
        const primaryLayer = (x.layers || []).find(l => !l.as_outline);
        const barCmap = x.cmap || (primaryLayer && primaryLayer.cmap);
        const colorbarEnabled = !(x.colorbar && x.colorbar.show === false) && !!resolveCmapStops(barCmap);
        const wNum = typeof width === 'number' ? width : parseFloat(width) || el.clientWidth || 400;
        const hNum = typeof height === 'number' ? height : parseFloat(height) || el.clientHeight || 300;
        const reservedWidth = colorbarEnabled ? COLORBAR_WIDTH + 8 : 0;
        const viewerWidth = Math.max(100, wNum - reservedWidth);
        const viewerHeight = hNum;

        try {
          lastPayload = x;
          const container = ensureLayout();

          if (!viewer) {
            dbg('Creating NeuroSurfaceViewer');

            // Start with any config values provided from R
            var config = Object.assign({}, x.config);
            // Only fill in defaults when the user did not specify them
            if (!('cmap' in config)) config.cmap = x.cmap;
            if (!('rotationSpeed' in config)) config.rotationSpeed = 2.5; // default rotation

            // Compatibility defaults for surfviewjs integration
            // These maintain the visual appearance of the previous version
            if (!('materialType' in config)) config.materialType = 'phong'; // Match old Phong material
            // Slightly brighter defaults to avoid overly dark scene
            if (!('ambientLightColor' in config)) config.ambientLightColor = 0x666666;
            if (!('ambientLightIntensity' in config)) config.ambientLightIntensity = 0.7;
            if (!('directionalLightIntensity' in config)) config.directionalLightIntensity = 0.7;
            // Force controls on to ensure Tweakpane is rendered
            config.useControls = x.config && x.config.useControls === false ? false : true;
            config.showControls = x.config && x.config.showControls === false ? false : true;
            if (!('controlType' in config)) config.controlType = 'trackball'; // Use trackball controls
            if (!('allowCDNFallback' in config)) config.allowCDNFallback = true;

            // Normalize any color strings coming from R to numeric hex values expected by surfviewjs
            const colorKeys = ['ambientLightColor', 'directionalLightColor', 'specularColor', 'color'];
            const normalizeColor = (val) => {
              if (typeof val === 'string') {
                try {
                  return new neurosurface.THREE.Color(val).getHex();
                } catch (e) {
                  console.warn('Invalid color string in config:', val);
                  return val;
                }
              }
              return val;
            };
            colorKeys.forEach((k) => {
              if (k in config) config[k] = normalizeColor(config[k]);
            });

            const makeViewer = (cfg) => new neurosurface.NeuroSurfaceViewer(
              container,
              viewerWidth,
              viewerHeight,
              cfg,
              x.viewpoint
            );

            try {
              viewer = makeViewer(config);
              if (config.showControls !== false && config.useControls !== false) {
                ensureTweakpane().finally(() => safeToggleControls(true));
              }
            } catch (initErr) {
              console.warn('Viewer init failed with controls; retrying without controls:', initErr);
              const cfgNoControls = { ...config, showControls: false, useControls: false };
              viewer = makeViewer(cfgNoControls);
            }

            // Use the new methods to set rotation speed and initial zoom
            //viewer.setRotationSpeed(2.5);
            //viewer.setInitialZoom(2.5);
          }
        } catch (error) {
          console.error("Error creating NeuroSurfaceViewer:", error);
          return; // abort render if viewer failed
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
          if (x.curv) {
            validateNumericArray(x.curv, 'x.curv', undefined, x.vertices.length / 3);
          }

          if (x.cmap) {
            validateNumericArray(x.data, 'x.data', undefined, x.indices.length);
          }
          if (x.vertexColors) {
            validateNumericArray(x.vertexColors, 'x.vertexColors', undefined, x.indices.length);
          }

          dbg('Creating SurfaceGeometry');
          var geometry = new neurosurface.SurfaceGeometry(x.vertices, x.faces, x.hemi, x.curv);
          dbg('SurfaceGeometry created:', geometry);

          var surface;
          if (x.cmap) {
            dbg('Creating ColorMappedNeuroSurface');
            // Multi-layer path
            if (Array.isArray(x.layers) && x.layers.length > 0 && neurosurface.MultiLayerNeuroSurface) {
              dbg('Creating MultiLayerNeuroSurface');
              surface = new neurosurface.MultiLayerNeuroSurface(geometry, {
                useGPUCompositing: config.useGPUCompositing || false,
                useWideLines: config.useWideLines !== false
              });

              const normalizeHex = (c) => {
                if (typeof c === 'string') {
                  const hex = c.trim();
                  if (/^#([0-9a-fA-F]{8})$/.test(hex)) return '#' + hex.slice(1, 7); // drop alpha channel
                  if (/^#([0-9a-fA-F]{6})$/.test(hex) || /^#([0-9a-fA-F]{3})$/.test(hex)) return hex;
                }
                return c;
              };

              const normalizeCmap = (cm) => {
                if (Array.isArray(cm)) return cm.map(normalizeHex);
                return cm;
              };

              const toLayer = (layer, idx) => {
                const base = {
                  type: layer.as_outline ? 'outline' : 'data',
                  id: layer.id || `layer_${idx}`,
                  data: layer.data || x.data,
                  indices: layer.indices || x.indices,
                  cmap: normalizeCmap(layer.cmap || x.cmap),
                  range: layer.color_range || layer.irange || x.irange,
                  threshold: layer.thresh || layer.threshold || x.thresh || [0,0],
                  opacity: layer.alpha ?? x.alpha ?? 1,
                  visible: layer.visible !== false,
                  blendMode: layer.blendMode || 'normal'
                };

                if (layer.as_outline) {
                  base.roiLabels = layer.data;
                  base.color = layer.outline_col || 0x000000;
                  base.width = layer.outline_width || 1.5;
                  base.halo = layer.outline_halo || false;
                  base.haloColor = layer.outline_halo_col;
                  base.haloWidth = layer.outline_halo_width;
                  base.offset = layer.outline_offset || 0;
                  base.roiSubset = layer.outline_rois;
                  base.useWideLines = config.useWideLines !== false;
                }

                try {
                  return neurosurface.Layer.fromConfig(base);
                } catch (err) {
                  console.warn('Failed to construct layer from config', base, err);
                  return null;
                }
              };

              (x.layers || []).forEach((layer, idx) => {
                const l = toLayer(layer, idx);
                if (l) surface.layerStack.addLayer(l);
              });

              surface.updateColors && surface.updateColors();
            } else {
              surface = new neurosurface.ColorMappedNeuroSurface(
                geometry, 
                x.indices,
                x.data,
                x.cmap,
                { ...config, irange: x.irange, thresh: x.thresh, alpha: x.alpha }
              );
            }
          } else if (x.vertexColors) {
            dbg('Creating VertexColoredNeuroSurface');
            surface = new neurosurface.VertexColoredNeuroSurface(
              geometry, 
              x.indices,
              x.vertexColors,
              { ...config, alpha: x.alpha || 1 }
            );
          } else {
            throw new Error("Neither color map nor vertex colors provided");
          }

          dbg('Surface created:', surface);
          dbg('Adding surface to viewer');
          viewer.addSurface(surface, surfaceId);
          // Fit the camera to the surface bounds to avoid over-zoomed default view
          if (viewer.centerCamera) {
            viewer.centerCamera();
          }

          // Draw colorbar if available
          // Choose colorbar source: primary layer or root cmap
          const primaryLayer = (x.layers || []).find(l => !l.as_outline);
          const barCmap = x.cmap || (primaryLayer && primaryLayer.cmap);
          const barRange = x.irange || (primaryLayer && (primaryLayer.color_range || primaryLayer.irange));
          const barState = {
            cmap: resolveCmapStops(barCmap) || barCmap,
            irange: barRange || [0, 1],
            label: x.colorbar ? x.colorbar.label : null
          };
          viewer._nsBarState = barState;

          const repaintColorbar = function() {
            drawColorbar(
              barState.cmap,
              barState.irange,
              barState.label,
              viewerHeight,
              colorbarEnabled
            );
          };

          // Initial paint
          repaintColorbar();

          // Listen to layer-level events from the viewer to keep the bar in sync
          if (viewer && typeof viewer.on === 'function' && !viewer._nsColorbarBound) {
            viewer._nsColorbarBound = true;

            const setBarCmap = (cm) => {
              barState.cmap = resolveCmapStops(cm) || cm;
              viewer._nsBarState = barState;
              repaintColorbar();
            };

            viewer.on('layer:colormap', (payload) => {
              setBarCmap(payload && payload.colormap);
            });

            viewer.on('surface:colormap', (payload) => {
              setBarCmap(payload && payload.colormap);
            });

            viewer.on('layer:intensity', ({ range }) => {
              if (Array.isArray(range) && range.length === 2) {
                barState.irange = range;
                viewer._nsBarState = barState;
                repaintColorbar();
              }
            });

            // Threshold changes don't alter the bar gradient, so we ignore them here.
            // Opacity is likewise not shown on the bar.
          }
          
          // Start the render loop if not already running
          if (viewer.startRenderLoop) {
            viewer.startRenderLoop();
          } else if (viewer.start) {
            viewer.start();
          }
          
        } catch (error) {
          console.error("Error in renderValue:", error);
        }
      },

      resize: function(width, height) {
        if (viewer) {
          const colorbarEnabled = colorbarWrapper && colorbarWrapper.style.display !== 'none';
          const reservedWidth = colorbarEnabled ? COLORBAR_WIDTH + 8 : 0;
          const wNum = typeof width === 'number' ? width : parseFloat(width) || el.clientWidth || 400;
          const hNum = typeof height === 'number' ? height : parseFloat(height) || el.clientHeight || 300;
          const viewerWidth = Math.max(100, wNum - reservedWidth);
          const viewerHeight = hNum;
          if (viewer.resize) {
            viewer.resize(viewerWidth, viewerHeight, { dpr: window.devicePixelRatio || 1 });
          } else {
            viewer.onWindowResize(viewerWidth, viewerHeight);
          }
          const bs = viewer._nsBarState || {};
          drawColorbar(bs.cmap, bs.irange, bs.label, viewerHeight, colorbarEnabled);
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

      destroy: function() {
        if (viewer && viewer.dispose) {
          viewer.dispose();
        }
        if (viewerContainer) {
          viewerContainer.innerHTML = '';
        }
        viewer = null;
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
