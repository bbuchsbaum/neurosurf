#!/usr/bin/env node
// Playwright is the sole browser backend. Set PLAYWRIGHT_MODULE to an installed
// playwright package and NEUROSURF_CHROMIUM to its installed Chromium executable.
// Generate the fixture first with tools/reference-specimen.R.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const http = require('node:http');
const path = require('node:path');
const { chromium } = require(process.env.PLAYWRIGHT_MODULE || 'playwright');
const root = path.resolve(process.argv[2] || '/tmp/neurosurf-reference');
const server = http.createServer((req, res) => {
  const relative = decodeURIComponent(new URL(req.url, 'http://localhost').pathname);
  if (relative === '/favicon.ico') { res.writeHead(204).end(); return; }
  const file = path.resolve(root, '.' + relative, relative.endsWith('/') ? 'index.html' : '');
  if (!file.startsWith(root + path.sep)) { res.writeHead(403).end(); return; }
  if (!fs.existsSync(file)) { res.writeHead(404).end(); return; }
  const type = { '.html': 'text/html', '.js': 'text/javascript', '.json': 'application/json',
    '.png': 'image/png', '.svg': 'image/svg+xml' }[path.extname(file)] || 'application/octet-stream';
  res.setHeader('Content-Type', type); res.end(fs.readFileSync(file));
});
(async () => {
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  let browser;
  const receipts = [];
  try {
    browser = await chromium.launch({ headless: true,
      ...(process.env.NEUROSURF_CHROMIUM ? { executablePath: process.env.NEUROSURF_CHROMIUM } : {}) });
    for (const mode of ['inline', 'bundled']) {
      const page = await browser.newPage({ viewport: { width: 1000, height: 800 } });
      const errors = []; const remote = [];
      page.on('pageerror', error => errors.push(error.message));
      page.on('console', msg => { if (msg.type() === 'error') errors.push(msg.text()); });
      page.on('request', req => { if (new URL(req.url()).hostname !== '127.0.0.1') remote.push(req.url()); });
      await page.goto(`http://127.0.0.1:${server.address().port}/${mode}/`);
      await page.waitForFunction(() => window.surfviewHandle?.controlTarget, { timeout: 30000 });
      await page.evaluate(() => {
        window.session = surfview.createSurfViewControlSession(surfviewHandle.controlTarget);
        window.capture = async () => {
          // Let the viewer finish its damped camera fit before comparing pixels.
          for (let frame = 0; frame < 60; frame++) {
            await new Promise(resolve => requestAnimationFrame(resolve));
          }
          const img = new Image(); img.src = surfviewHandle.exportPNG({width:1000,height:480,colorbar:false}); await img.decode();

          const c = document.createElement('canvas'); c.width = img.width; c.height = img.height;
          const ctx = c.getContext('2d'); ctx.drawImage(img, 0, 0);
          return Array.from(ctx.getImageData(0, 0, c.width, c.height).data);
        };
      });
      const result = await page.evaluate(async () => {
        const address = { surfaceId: 'left', layerId: 'psl' };
        const snapshot = () => surfviewHandle.controlTarget.getSnapshot();
        const initial = snapshot();
        const invalidScenes = [];
        for (const mode of ['legacy-schema', 'selected-reference', 'unknown-role']) {
          const m = JSON.parse(JSON.stringify(surfviewHandle.manifest));
          if (mode === 'legacy-schema') m.schemaVersion = 'surfview.scene.v1';
          if (mode === 'selected-reference') m.selectedLayer = 'psl';
          if (mode === 'unknown-role') m.layers.psl.role = 'mystery';
          try { surfview.mountSurfView(document.createElement('div'), m); invalidScenes.push(false); }
          catch { invalidScenes.push(true); }
        }
        const beforeReadout = session.setInspectionSelection({ kind: 'vertex', surfaceId: 'left', vertexIndex: 5 });
        const numerical = () => JSON.stringify({
          values: snapshot().selection.inspection?.values,
          maps: snapshot().surfaces.map(s => s.layers.filter(l => l.metadata?.role === 'map').map(l => l.scalarMapping))
        });
        const original = numerical();
        const on = await capture();
        session.setLayerVisibility(address, false);
        const off = await capture();
        session.setLayerVisibility(address, true);
        session.setLayerOpacity(address, 0);
        const zero = await capture();
        session.setLayerOpacity(address, 0.8);
        const stronger = await capture();
        const after = numerical();
        const changed = (a, b) => a.reduce((n, v, i) => n + Number(v !== b[i]), 0);
        let referencePixels = 0, opaqueEffectPixels = 0, opaqueEffectChanged = 0;
        for (let i = 0; i < on.length; i += 4) {
          if (off[i] > off[i + 2] + 30 && off[i] > off[i + 1] + 30) {
            opaqueEffectPixels++;
            if (on[i] !== off[i] || on[i + 1] !== off[i + 1] || on[i + 2] !== off[i + 2]) opaqueEffectChanged++;
          }
          if (on[i + 2] > on[i] + 20 && on[i + 2] > on[i + 1] + 20) referencePixels++;
        }
        session.setDisplayedLayer('graded');
        const gradedOn = await capture();
        session.setLayerVisibility(address, false);
        const gradedOff = await capture();
        session.setLayerVisibility(address, true);
        let translucentEffectChanged = 0;
        for (let i = 0; i < off.length; i += 4) {
          if (off[i] > off[i + 2] + 30 && off[i] > off[i + 1] + 30 &&
              (gradedOn[i] !== gradedOff[i] || gradedOn[i + 2] !== gradedOff[i + 2])) translucentEffectChanged++;
        }
        const switches = [];
        for (const map of ['graded', 'effect']) {
          session.setDisplayedLayer(map);
          switches.push(snapshot());
        }
        const forbidden = session.setDisplayedLayer('psl');
        const forbiddenMapToggle = session.setLayerVisibility({ surfaceId: 'left', layerId: 'effect' }, false);
        const invalidOpacity = session.setLayerOpacity(address, NaN);
        session.resetView();
        const reset = snapshot();
        surfviewHandle.resize(900, 500);
        const resized = snapshot();
        return { initial, reset, resized, switches, forbidden, forbiddenMapToggle, invalidOpacity, invalidScenes,
          beforeReadout, original, after, changedPixels: changed(on, off), zeroDifference: changed(zero, off),
          strongerDifference: changed(stronger, on), referencePixels, opaqueEffectPixels, opaqueEffectChanged, translucentEffectChanged,
          emittedRole: surfviewHandle.manifest.layers.psl.role,
          provenance: surfviewHandle.manifest.layers.psl.provenance };
      });
      assert.deepEqual(result.invalidScenes, [true, true, true]);
      assert.equal(result.emittedRole, 'reference');
      assert.equal(result.provenance.mesh, 'four-quads');
      assert.equal(result.beforeReadout.ok, true);
      assert.equal(JSON.parse(result.original).values.find(v => v.layerId === 'effect').value, 2);
      assert.equal(result.original, result.after, 'reference altered effect mapping/readouts');
      assert.ok(result.changedPixels > 100, 'reference toggle must change actual pixels');
      assert.ok(result.strongerDifference > 100, 'reference opacity must change actual pixels');
      assert.equal(result.zeroDifference, 0, 'zero reference opacity must match hidden pixels');
      assert.ok(result.referencePixels > 100);
      assert.ok(result.opaqueEffectPixels > 100);
      assert.equal(result.opaqueEffectChanged, 0, 'opaque effect must cover reference');
      assert.ok(result.translucentEffectChanged > 100, 'translucent effect must reveal reference');
      assert.equal(result.forbidden.ok, false); assert.equal(result.forbiddenMapToggle.ok, false);
      assert.equal(result.invalidOpacity.ok, false);
      for (const snap of [result.initial, result.reset, result.resized, ...result.switches]) {
        assert.deepEqual(snap.capabilities.exclusiveMap.availableLayerIds, ['effect', 'graded']);
        for (const s of snap.surfaces) {
          const r = s.layers.find(l => l.id === 'psl');
          assert.ok(s.layers.indexOf(r) < s.layers.findIndex(l => l.id === snap.capabilities.exclusiveMap.displayedLayerId));
          assert.equal(s.layers.filter(l => l.metadata?.role === 'map' && l.visible).length, 1);
        }
      }
      assert.ok(result.reset.surfaces.every(s => s.layers.find(l => l.id === 'psl').opacity === 0.4));
      assert.ok(result.switches.every(snap => snap.surfaces.every(s => s.layers.find(l => l.id === 'psl').opacity === 0.8)));
      assert.ok(!result.original.includes('"layerId":"psl"'));
      const selector = page.getByRole('combobox', { name: 'Displayed surface map' });
      assert.deepEqual(await selector.locator('option').evaluateAll(os => os.map(o => o.value)), ['effect', 'graded']);
      await page.evaluate(async () => {
        for (let frame = 0; frame < 60; frame++) await new Promise(resolve => requestAnimationFrame(resolve));
      });
      assert.ok(await page.locator('canvas').evaluate(c => c.getBoundingClientRect().height < 1000), 'bounded report height');
      await page.screenshot({ path: path.join(root, `${mode}-reference-visible.png`) });
      const toggle = page.getByRole('checkbox', { name: 'Show anatomical reference' });
      await toggle.focus(); await page.keyboard.press('Space');
      assert.equal(await toggle.isChecked(), false);
      await page.getByRole('slider', { name: 'Anatomical reference opacity' }).focus();
      await page.keyboard.press('ArrowRight');
      await page.screenshot({ path: path.join(root, `${mode}-browser.png`) });
      // Dispose/remount the emitted manifest, restoring reference state through public APIs.
      const remount = await page.evaluate(async () => {
        const manifest = JSON.parse(JSON.stringify(surfviewHandle.manifest));
        const saved = surfviewHandle.controlTarget.getSnapshot().surfaces[0].layers.find(l => l.id === 'psl');
        session.dispose(); const old = surfviewHandle.controlTarget; surfviewHandle.dispose();
        const handle = surfview.mountSurfView(document.getElementById('surfview-report'), manifest, {
          lazy: false, controls: true, height: 480, baseUrl: document.baseURI,
          bilateralGroup: { id: 'bilateral', leftSurfaceId: 'left', rightSurfaceId: 'right' }
        });
        await handle.ready;
        handle.controlTarget.setLayerVisibility({ surfaceId: 'left', layerId: 'psl' }, saved.visible);
        handle.controlTarget.setLayerOpacity({ surfaceId: 'left', layerId: 'psl' }, saved.opacity);
        const restored = handle.controlTarget.getSnapshot();
        const mounts = document.querySelectorAll('.surfview-mount').length;
        handle.dispose();
        return { restored, mounts, disposed: old.isDisposed(), rootsAfter: document.querySelectorAll('.surfview-mount').length };
      });
      assert.equal(remount.mounts, 1); assert.equal(remount.rootsAfter, 0); assert.equal(remount.disposed, true);
      assert.ok(remount.restored.surfaces.every(s => s.layers.find(l => l.id === 'psl').visible === false));
      assert.deepEqual(errors, []); assert.deepEqual(remote, []);
      receipts.push({ mode, changedChannels: result.changedPixels, referencePixels: result.referencePixels,
        opaqueEffectPixels: result.opaqueEffectPixels, zeroDifference: result.zeroDifference,
        effectReadoutsUnchanged: result.original === result.after,
        translucentEffectChanged: result.translucentEffectChanged, remount: true });
      await page.close();
    }
    fs.writeFileSync(path.join(root, 'browser-receipt.json'), JSON.stringify(receipts, null, 2));
    console.log(JSON.stringify(receipts, null, 2));
  } finally {
    if (browser) await browser.close();
    await new Promise(resolve => server.close(resolve));
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
