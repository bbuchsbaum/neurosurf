# Bundled viewer source

The active widget uses only `surfview.embed.iife.js` and the package's widget
adapter/CSS. `surfview.embed.commit` records the exact upstream commit,
upstream bundle hash, distributed bundle hash and source archive hash.
The upstream is https://github.com/bbuchsbaum/surfviewjs (MIT).

`surfview-source.tar.xz` contains:

- the pinned upstream `src/` tree, build configuration, package metadata and
  exact npm lockfile;
- the readable source of the runtime modules visited by the bundler, including
  code from Three.js, colormap, fflate and lerp, and their package/license files;
- `BUNDLED-MODULES.json` with the actual third-party source package versions.

It contains no node_modules executables, installed browsers, native addons,
credentials, or unrelated example datasets. Paths and tar timestamps are
normalized so the archive can be reproduced independently.

To inspect the source from an installed R package:

```r
archive <- system.file("htmlwidgets", "lib", "neurosurface",
                       "surfview-source.tar.xz", package = "neurosurf")
utils::untar(archive, exdir = tempfile("surfview-source-"))
```

For a verified rebuild of the distributed bundle and archive, use Node >=22,
Python >=3.12 and a checkout containing the commit named in the pin:

```sh
git clone https://github.com/bbuchsbaum/surfviewjs.git /tmp/surfviewjs
python3 tools/rebuild-surfview-runtime.py /tmp/surfviewjs
```

The script builds that commit in a temporary directory, verifies TypeScript
and the four runtime unit suites, collects the module graph, restores the
complete notices in the bundle, and refuses installation if either artifact
hash differs from the recorded pin. npm downloads only occur during this
explicit developer rebuild; R package installation never runs npm or fetches
viewer source.

To rebuild from the archive directly, extract it, run `npm ci --ignore-scripts`
in its `surfview/` directory, and run `npx vite build --config
vite.config.embed.js`. This reconstructs the upstream bundle identified by
`upstream_sha256`; the neurosurf script adds the original license notices.
