(function () {
  "use strict";

  function showFallback(el, message) {
    var mount = el.querySelector(".surfwidget-mount");
    var fallback = el.querySelector(".surfwidget-author-fallback");
    if (mount) mount.hidden = true;
    if (fallback) {
      if (message) fallback.textContent = message;
      fallback.hidden = false;
    }
  }

  function installFullscreen(el) {
    var toolbar = el.querySelector(".surfview-report-controls");
    if (!toolbar || toolbar.querySelector(".surfwidget-fullscreen")) return;

    var button = document.createElement("button");
    button.type = "button";
    button.className = "surfwidget-fullscreen";
    button.textContent = "Fullscreen";
    button.setAttribute("aria-label", "Show surface viewer fullscreen");
    if (!el.requestFullscreen) {
      button.disabled = true;
      button.title = "Fullscreen is unavailable in this browser";
    } else {
      button.addEventListener("click", function () {
        el.requestFullscreen().catch(function () {
          button.title = "Fullscreen could not be opened; the surface remains available here";
        });
      });
      el.addEventListener("fullscreenchange", function () { fitViewer(el); });
    }
    toolbar.appendChild(button);
  }

  function formatTick(v) {
    var s = Math.abs(v) >= 100 || v === Math.round(v) ? String(Math.round(v * 100) / 100)
      : String(Number(v.toPrecision(3)));
    return s.replace(/^-/, "−");
  }

  // A graphical colour key for the displayed map. The runtime's own legend is
  // a text range; keep it for screen readers and draw the scale beside it.
  // Only explicit colour arrays can be drawn; named runtime colormaps keep
  // the text legend.
  function renderColorbar(el, scene) {
    var toolbar = el.querySelector(".surfview-report-controls");
    var select = toolbar && toolbar.querySelector("select");
    if (!toolbar || !scene || !scene.layers) return;
    var bar = toolbar.querySelector(".surfwidget-colorbar");
    var text = Array.prototype.find.call(
      toolbar.querySelectorAll(":scope > span[aria-live]"),
      function (s) { return !s.style.color; }
    );
    var id = select && select.value ? select.value : scene.selectedLayer;
    var layer = scene.layers[id];
    var colors = layer && layer.colorMap;
    if (typeof colors === "string") colors = [colors];
    var lim = layer && layer.limits;
    if (!layer || !Array.isArray(colors) || colors.length < 2 || !lim ||
        !(lim[1] > lim[0])) {
      if (bar) bar.hidden = true;
      if (text) text.classList.remove("surfwidget-visually-hidden");
      return;
    }
    if (!bar) {
      bar = document.createElement("span");
      bar.className = "surfwidget-colorbar";
      bar.setAttribute("aria-hidden", "true");
      toolbar.insertBefore(bar, text ? text.nextSibling : null);
    }
    if (text) text.classList.add("surfwidget-visually-hidden");
    bar.hidden = (layer.legend && layer.legend.visible === false);
    var pos = function (v) { return 100 * (v - lim[0]) / (lim[1] - lim[0]); };
    var stops = colors.map(function (c, i) {
      return c + " " + (100 * i / (colors.length - 1)).toFixed(2) + "%";
    }).join(", ");
    var thr = layer.threshold;
    var gap = Array.isArray(thr) && thr.length === 2 && thr[1] > thr[0] &&
      thr[0] > lim[0] && thr[1] < lim[1] ? [pos(thr[0]), pos(thr[1])] : null;
    var ticks = gap ? [lim[0], thr[0], thr[1], lim[1]] : [lim[0], lim[1]];
    if (!gap && lim[0] < 0 && lim[1] > 0) ticks.splice(1, 0, 0);
    var title = (layer.legend && layer.legend.title) || layer.label || id;
    bar.replaceChildren();
    var label = document.createElement("span");
    label.className = "surfwidget-colorbar-title";
    label.textContent = title + (layer.units ? " (" + layer.units + ")" : "");
    var scale = document.createElement("span");
    scale.className = "surfwidget-colorbar-scale";
    var ramp = document.createElement("span");
    ramp.className = "surfwidget-colorbar-ramp";
    ramp.style.background = "linear-gradient(to right, " + stops + ")";
    scale.appendChild(ramp);
    if (gap) {
      var hole = document.createElement("span");
      hole.className = "surfwidget-colorbar-gap";
      hole.style.left = gap[0] + "%";
      hole.style.width = (gap[1] - gap[0]) + "%";
      scale.appendChild(hole);
    }
    ticks.forEach(function (v, i) {
      var t = document.createElement("span");
      t.className = "surfwidget-colorbar-tick";
      t.style.left = pos(v) + "%";
      t.textContent = formatTick(v);
      if (i === 0) t.classList.add("is-first");
      if (i === ticks.length - 1) t.classList.add("is-last");
      scale.appendChild(t);
    });
    bar.append(label, scale);
  }

  function installColorbar(el, scene) {
    var toolbar = el.querySelector(".surfview-report-controls");
    var select = toolbar && toolbar.querySelector("select");
    renderColorbar(el, scene);
    if (select && !select.__surfwidgetColorbar) {
      select.__surfwidgetColorbar = true;
      select.addEventListener("change", function () {
        setTimeout(function () { renderColorbar(el, scene); }, 0);
      });
    }
  }

  // The surfview runtime sizes its canvas to the height we pass and stacks
  // the report toolbar above it, so size the canvas to what remains of the
  // container after the toolbar or the widget overflows into the page.
  function fitViewer(el) {
    var handle = el.__surfviewHandle;
    var mount = el.querySelector(".surfwidget-mount");
    if (!handle || !mount) return;
    var toolbar = mount.querySelector(".surfview-report-controls");
    var extra = toolbar ? toolbar.getBoundingClientRect().height : 0;
    var w = mount.clientWidth;
    var h = mount.clientHeight - extra;
    if (w > 0 && h > 0) handle.resize(w, h);
  }

  function applyCall(handle, call) {
    if (!handle.viewer || !call || !call.method) return;
    var args = call.args || [];
    var viewer = handle.viewer;
    switch (call.method) {
      case "setColorMap":
        viewer.updateConfig({ colorMap: args[0] });
        break;
      case "setIRange":
        viewer.updateConfig({ colorRange: [args[0], args[1]] });
        break;
      case "setAlpha":
        viewer.updateConfig({ alpha: args[0] });
        break;
      case "setZoom":
        viewer.setZoom(args[0]);
        break;
      case "setRotationSpeed":
        viewer.updateConfig({ rotationSpeed: args[0] });
        break;
      default:
        console.warn("surfwidget compatibility call is unsupported:", call.method);
    }
  }

  HTMLWidgets.widget({
    name: "surfwidget",
    type: "output",

    factory: function (el, width, height) {
      return {
        renderValue: function (x) {
          if (el.__surfviewHandle) el.__surfviewHandle.dispose();
          el.replaceChildren();
          el.classList.add("surfwidget-root");
          el.setAttribute("role", "group");
          el.setAttribute("aria-label", x.altText || "Interactive cortical surface");

          var mount = document.createElement("div");
          mount.className = "surfwidget-mount";
          var fallback = document.createElement("div");
          fallback.className = "surfwidget-author-fallback";
          fallback.textContent = x.fallback || "Interactive surface view unavailable.";
          fallback.hidden = true;
          el.append(mount, fallback);

          var options = Object.assign({}, x.options || {}, {
            width: width,
            height: height,
            baseUrl: document.baseURI,
            onError: function (error) {
              showFallback(el, (x.fallback || "Interactive surface view unavailable.") +
                " " + error.message);
            }
          });

          try {
            var handle = surfview.mountSurfView(mount, x.scene, options);
            el.__surfviewHandle = handle;
            handle.ready.then(function () {
              if (handle.viewer && x.config && Object.keys(x.config).length) {
                handle.viewer.updateConfig(x.config);
              }
              installFullscreen(el);
              installColorbar(el, x.scene);
              fitViewer(el);
              // Name the opening view so the toolbar shows where the reader is.
              if (x.initialView && typeof handle.setView === "function") {
                try { handle.setView(x.initialView); } catch (e) { /* keep framing */ }
              }
              (x.calls || []).forEach(function (call) { applyCall(handle, call); });
            }).catch(options.onError);
          } catch (error) {
            options.onError(error);
          }
        },

        resize: function () {
          fitViewer(el);
        }
      };
    }
  });

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler("neurosurf-surfwidget-config", function (message) {
      var el = document.getElementById(message.id);
      var handle = el && el.__surfviewHandle;
      if (handle && handle.viewer) handle.viewer.updateConfig(message.config || {});
    });
  }
}());
