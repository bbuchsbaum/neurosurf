const { test, expect } = await import(new URL(
  "../../../jscode/surfviewjs/node_modules/@playwright/test/index.mjs",
  import.meta.url
));
const fs = await import("node:fs/promises");
const os = await import("node:os");

const base = process.env.NEUROSURF_RFE77_URL || "http://127.0.0.1:8765";
const receiptPath = process.env.NEUROSURF_RFE77_RECEIPT ||
  "/tmp/neurosurf-rfe77/browser-metrics.json";
const reports = [
  ["standalone", `${base}/standalone/`],
  ["rmarkdown", `${base}/rmd/portable-surface-report.html`],
  ["quarto", `${base}/quarto/portable-surface-report.html`]
];

async function installInstrumentation(page) {
  await page.addInitScript(() => {
    window.__contexts = new Set();
    window.__activeFrames = new Set();
    const originalContext = HTMLCanvasElement.prototype.getContext;
    HTMLCanvasElement.prototype.getContext = function (type, ...args) {
      const result = originalContext.call(this, type, ...args);
      if ((type === "webgl" || type === "webgl2") && result) {
        window.__contexts.add(result);
      }
      return result;
    };
    const originalRequest = window.requestAnimationFrame.bind(window);
    const originalCancel = window.cancelAnimationFrame.bind(window);
    window.requestAnimationFrame = callback => {
      let id;
      id = originalRequest(time => {
        window.__activeFrames.delete(id);
        callback(time);
      });
      window.__activeFrames.add(id);
      return id;
    };
    window.cancelAnimationFrame = id => {
      window.__activeFrames.delete(id);
      return originalCancel(id);
    };
  });
}

for (const [name, url] of reports) {
  test(`${name} report is offline, accessible, and interactive`, async ({ page }) => {
    const browserErrors = [];
    const remoteRequests = [];
    page.on("console", message => {
      if (message.type() === "error") browserErrors.push(message.text());
    });
    page.on("pageerror", error => browserErrors.push(error.message));
    page.on("requestfailed", request => browserErrors.push(request.url()));
    page.on("request", request => {
      if (new URL(request.url()).hostname !== "127.0.0.1") {
        remoteRequests.push(request.url());
      }
    });
    await installInstrumentation(page);
    await page.goto(url, { waitUntil: "domcontentloaded" });
    await expect(page.locator("canvas")).toBeVisible();
    await page.getByRole("combobox", {
      name: "Displayed surface map"
    }).selectOption("atlas");
    await page.getByRole("button", { name: "Medial" }).click();
    await page.getByRole("button", { name: "Reset", exact: true }).click();
    await expect(page.getByRole("button", {
      name: "Show surface viewer fullscreen"
    })).toBeEnabled();
    const proof = await page.evaluate(() => {
      const host = document.querySelector(".surfwidget-root");
      const handle = window.surfviewHandle || host?.__surfviewHandle;
      return {
        revision: window.surfview?.SURFVIEW_EMBED_THREE_REVISION,
        contexts: window.__contexts.size,
        png: handle.exportPNG().slice(0, 22),
        buttons: Array.from(document.querySelectorAll(
          ".surfview-report-controls button"
        )).map(button => button.textContent.trim())
      };
    });
    expect(proof.revision).toBe("185");
    expect(proof.contexts).toBe(1);
    expect(proof.png).toBe("data:image/png;base64,");
    expect(proof.buttons).toEqual(expect.arrayContaining([
      "Lateral", "Medial", "Reset", "PNG", "Fullscreen"
    ]));
    await page.emulateMedia({ media: "print" });
    await expect(page.locator(
      ".surfwidget-author-fallback, .surfview-author-fallback"
    ).filter({ hasText: "Left cortical surface" }).last()).toBeVisible();
    expect(browserErrors).toEqual([]);
    expect(remoteRequests).toEqual([]);
    await page.evaluate(() => {
      const host = document.querySelector(".surfwidget-root");
      (window.surfviewHandle || host?.__surfviewHandle).dispose();
    });
    await page.waitForTimeout(50);
    expect(await page.evaluate(() => window.__activeFrames.size)).toBe(0);
  });
}

test("failure modes expose authored fallback content", async ({ browser }) => {
  const noScript = await browser.newContext({ javaScriptEnabled: false });
  const noScriptPage = await noScript.newPage();
  await noScriptPage.goto(`${base}/standalone/`);
  await expect(noScriptPage.locator("noscript .surfview-author-fallback"))
    .toBeVisible();
  await noScript.close();

  for (const mode of ["load", "checksum", "webgl", "fullscreen"]) {
    const context = await browser.newContext();
    const page = await context.newPage();
    if (mode === "load") {
      await page.route("**/*.values.bin", route => route.abort());
    } else if (mode === "checksum") {
      await page.route("**/*.values.bin", async route => {
        const response = await route.fetch();
        const body = await response.body();
        body[0] ^= 255;
        await route.fulfill({ response, body });
      });
    } else if (mode === "webgl") {
      await page.addInitScript(() => {
        const original = HTMLCanvasElement.prototype.getContext;
        HTMLCanvasElement.prototype.getContext = function (type, ...args) {
          if (type === "webgl" || type === "webgl2") return null;
          return original.call(this, type, ...args);
        };
      });
    } else {
      await page.addInitScript(() => {
        Object.defineProperty(HTMLElement.prototype, "requestFullscreen", {
          configurable: true,
          value: undefined
        });
      });
    }
    await page.goto(`${base}/fsaverage6/`, { waitUntil: "domcontentloaded" });
    if (mode === "fullscreen") {
      await expect(page.getByRole("button", {
        name: "Show surface viewer fullscreen"
      })).toBeDisabled();
    } else {
      await expect(page.getByText("Bilateral fsaverage6", {
        exact: false
      }).last()).toBeVisible();
    }
    await context.close();
  }
});

test("fsaverage6 performance and parity receipt", async ({ browser, browserName }) => {
  test.setTimeout(120000);
  const context = await browser.newContext({ viewport: { width: 1200, height: 800 } });
  const tti = [];
  const firstFrame = [];
  let firstRun;

  for (let run = 0; run < 10; run += 1) {
    const page = await context.newPage();
    await installInstrumentation(page);
    await page.goto(`${base}/fsaverage6/`, { waitUntil: "domcontentloaded" });
    await page.waitForFunction(() => window.surfviewHandle?.viewer, null, {
      timeout: 10000
    });
    const timing = await page.evaluate(async () => {
      const ready = performance.now();
      await new Promise(resolve => requestAnimationFrame(() => resolve()));
      return { ready, firstFrame: performance.now() };
    });
    tti.push(timing.ready);
    firstFrame.push(timing.firstFrame);

    if (run === 0) {
      const visualBeforeDrag = await page.evaluate(async () => {
        const handle = window.surfviewHandle;
        const bytes = new Uint8Array(await (
          await fetch(handle.exportPNG({ width: 600, height: 400 }))
        ).arrayBuffer());
        const digest = Array.from(new Uint8Array(
          await crypto.subtle.digest("SHA-256", bytes)
        )).map(value => value.toString(16).padStart(2, "0")).join("");
        return { quaternion: handle.viewer.camera.quaternion.toArray(), digest };
      });
      const canvasBox = await page.locator("canvas").boundingBox();
      await page.mouse.move(canvasBox.x + canvasBox.width * 0.4,
                            canvasBox.y + canvasBox.height * 0.5);
      await page.mouse.down();
      await page.mouse.move(canvasBox.x + canvasBox.width * 0.65,
                            canvasBox.y + canvasBox.height * 0.4,
                            { steps: 8 });
      await page.mouse.up();
      await page.waitForTimeout(50);
      const visualAfterDrag = await page.evaluate(async () => {
        const handle = window.surfviewHandle;
        const bytes = new Uint8Array(await (
          await fetch(handle.exportPNG({ width: 600, height: 400 }))
        ).arrayBuffer());
        const digest = Array.from(new Uint8Array(
          await crypto.subtle.digest("SHA-256", bytes)
        )).map(value => value.toString(16).padStart(2, "0")).join("");
        return { quaternion: handle.viewer.camera.quaternion.toArray(), digest };
      });
      firstRun = await page.evaluate(async () => {
        const handle = window.surfviewHandle;
        const viewer = handle.viewer;
        const surfaces = Array.from(viewer.surfaces.values());
        const geometryIds = viewer.getSurfaceIds();
        const orientations = {};
        let deterministicViews = true;
        for (const view of ["lateral", "medial", "dorsal", "ventral", "reset"]) {
          handle.setView(view);
          orientations[view] = Array.from(viewer.surfaces.values()).map(
            surface => surface.mesh.quaternion.toArray()
          );
          handle.setView(view);
          const repeated = Array.from(viewer.surfaces.values()).map(
            surface => surface.mesh.quaternion.toArray()
          );
          deterministicViews = deterministicViews && orientations[view].every(
            (quaternion, surfaceIndex) => quaternion.every((value, index) =>
              Math.abs(value - repeated[surfaceIndex][index]) < 1e-10)
          );
        }
        const geometryRequestCount = () => performance.getEntriesByType("resource")
          .filter(entry => entry.name.includes(".vertices.bin") ||
            entry.name.includes(".faces.bin")).length;
        const geometryRequestsBeforeMapSwitch = geometryRequestCount();
        handle.selectLayer("reliability");
        const geometryRequestsAfterMapSwitch = geometryRequestCount();
        const identityStable = surfaces.every((surface, index) =>
          surface === Array.from(viewer.surfaces.values())[index]);

        const manifestAssets = Object.values(handle.manifest.assets);
        const uriAssets = manifestAssets.filter(asset => asset.uri);
        const buffers = await Promise.all(uriAssets.map(async asset => ({
          asset,
          buffer: await (await fetch(new URL(asset.uri, document.baseURI))).arrayBuffer()
        })));
        const decodeStart = performance.now();
        let decodeGuard = 0;
        const decodeRepeats = 100;
        for (let repeat = 0; repeat < decodeRepeats; repeat += 1) {
          for (const { asset, buffer } of buffers) {
            const typed = asset.dtype === "float32" ?
              new Float32Array(buffer) : new Uint32Array(buffer);
            decodeGuard += typed.length ?
              Number(typed[0]) + Number(typed[typed.length - 1]) : 0;
          }
        }
        const decodeMs = (performance.now() - decodeStart) / decodeRepeats;
        const valueDigests = {};
        for (const layer of Object.values(handle.manifest.layers)) {
          valueDigests[layer.id] = {};
          for (const [hemi, ref] of Object.entries(layer.values)) {
            valueDigests[layer.id][hemi] = handle.manifest.assets[ref.values].sha256;
          }
        }
        const rMetrics = await (await fetch("./r-metrics.json")).json();
        const authoritativeValueParity = JSON.stringify(valueDigests) ===
          JSON.stringify(rMetrics.value_sha256);

        handle.resize(900, 600);
        const canvas = viewer.renderer.domElement;
        const resize = {
          css: [canvas.clientWidth, canvas.clientHeight],
          drawingBuffer: [canvas.width, canvas.height]
        };
        const pngUrl = handle.exportPNG({ width: 900, height: 600, colorbar: true });
        const blob = await (await fetch(pngUrl)).blob();
        const bitmap = await createImageBitmap(blob);
        const output = document.createElement("canvas");
        output.width = bitmap.width;
        output.height = bitmap.height;
        const outputContext = output.getContext("2d");
        outputContext.drawImage(bitmap, 0, 0);
        const pixels = outputContext.getImageData(0, 0, output.width, output.height).data;
        let leftPixels = 0;
        let rightPixels = 0;
        for (let y = 0; y < output.height; y += 4) {
          for (let x = 0; x < output.width; x += 4) {
            const offset = 4 * (y * output.width + x);
            const nonblank = pixels[offset + 3] > 0 &&
              (pixels[offset] < 245 || pixels[offset + 1] < 245 || pixels[offset + 2] < 245);
            if (nonblank) (x < output.width / 2 ? leftPixels++ : rightPixels++);
          }
        }
        const digestBytes = new Uint8Array(await blob.arrayBuffer());
        const digest = Array.from(new Uint8Array(
          await crypto.subtle.digest("SHA-256", digestBytes)
        )).map(value => value.toString(16).padStart(2, "0")).join("");
        return {
          revision: window.surfview.SURFVIEW_EMBED_THREE_REVISION,
          contextCount: window.__contexts.size,
          geometryIds,
          identityStable,
          orientations,
          deterministicViews,
          geometryRequestsBeforeMapSwitch,
          geometryRequestsAfterMapSwitch,
          decodeMs,
          decodeGuard,
          valueDigests,
          authoritativeValueParity,
          rMetrics,
          resize,
          png: {
            width: bitmap.width,
            height: bitmap.height,
            sha256: digest,
            leftNonblankSamples: leftPixels,
            rightNonblankSamples: rightPixels
          }
        };
      });
      firstRun.transferredBytes = await page.evaluate(() => performance
        .getEntriesByType("resource")
        .reduce((sum, entry) => sum + entry.transferSize, 0));
      firstRun.encodedBodyBytes = await page.evaluate(() => performance
        .getEntriesByType("resource")
        .reduce((sum, entry) => sum + entry.encodedBodySize, 0));
      firstRun.dragCameraChanged = JSON.stringify(visualBeforeDrag.quaternion) !==
        JSON.stringify(visualAfterDrag.quaternion);
      firstRun.dragPixelDigestChanged = visualBeforeDrag.digest !==
        visualAfterDrag.digest;
    }

    await page.evaluate(() => window.surfviewHandle.dispose());
    await page.waitForTimeout(50);
    expect(await page.evaluate(() => window.__activeFrames.size)).toBe(0);
    await page.close();
  }

  const sortedTti = [...tti].sort((a, b) => a - b);
  const receipt = {
    browserName,
    browserVersion: browser.version(),
    os: `${os.type()} ${os.release()}`,
    architecture: os.arch(),
    cpu: os.cpus()[0]?.model || "unknown",
    cacheState: "one cold navigation followed by nine warm-cache navigations",
    runs: 10,
    ttiMs: tti,
    firstFrameMs: firstFrame,
    medianTtiMs: (sortedTti[4] + sortedTti[5]) / 2,
    ciRegressionCeilingMs: 5000,
    referenceCeilingMs: 2000,
    ...firstRun
  };
  await fs.writeFile(receiptPath, JSON.stringify(receipt, null, 2));

  expect(receipt.revision).toBe("185");
  expect(receipt.contextCount).toBe(1);
  expect(receipt.geometryIds).toHaveLength(2);
  expect(receipt.identityStable).toBe(true);
  expect(receipt.authoritativeValueParity).toBe(true);
  expect(receipt.deterministicViews).toBe(true);
  expect(new Set([
    JSON.stringify(receipt.orientations.lateral),
    JSON.stringify(receipt.orientations.medial),
    JSON.stringify(receipt.orientations.dorsal),
    JSON.stringify(receipt.orientations.ventral)
  ]).size).toBe(4);
  expect(receipt.dragCameraChanged).toBe(true);
  expect(receipt.dragPixelDigestChanged).toBe(true);
  expect(receipt.geometryRequestsAfterMapSwitch).toBe(
    receipt.geometryRequestsBeforeMapSwitch
  );
  expect(receipt.png.width).toBe(900);
  expect(receipt.png.height).toBe(600);
  expect(receipt.png.leftNonblankSamples).toBeGreaterThan(100);
  expect(receipt.png.rightNonblankSamples).toBeGreaterThan(100);
  expect(receipt.resize.css).toEqual([900, 600]);
  expect(receipt.rMetrics.manifest_bytes).toBeLessThanOrEqual(16384);
  expect(receipt.rMetrics.layer_bytes).toEqual([327696, 327696]);
  expect(receipt.medianTtiMs).toBeLessThanOrEqual(2000);
  expect(Math.max(...receipt.ttiMs)).toBeLessThanOrEqual(5000);
  await context.close();
});
