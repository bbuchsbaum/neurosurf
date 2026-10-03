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
              fitViewer(el);
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
