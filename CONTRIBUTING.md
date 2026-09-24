# Contributing to neurosurf

Thank you for your interest in contributing!

## Reporting bugs

Please open an issue at
<https://github.com/bbuchsbaum/neurosurf/issues>. Include a minimal
reproducible example using the [reprex](https://reprex.tidyverse.org)
package.

## Suggesting features

Open an issue describing the proposed feature and use case.

## Submitting pull requests

1.  Fork the repository and create a branch from `master`.
2.  Install dependencies: `devtools::install_deps()`.
3.  Make your changes and add tests in `tests/testthat/`.
4.  Run `devtools::check()` and ensure 0 errors, 0 warnings, 0 notes.
5.  Open a pull request with a clear description of the change.

## Editing the README

Edit `README.Rmd`, not `README.md`, and regenerate with
`devtools::build_readme()`. The quick-start chunk is evaluated, so the
README fails to build if its example stops working.

## Rebuilding the JavaScript viewer

The interactive viewer used by
[`surfwidget()`](https://bbuchsbaum.github.io/neurosurf/reference/surfwidget-methods.md)
and
[`write_surface_scene()`](https://bbuchsbaum.github.io/neurosurf/reference/write_surface_scene.md)
is [surfviewjs](https://github.com/bbuchsbaum/surfviewjs). Its
report-safe embed bundle is committed at
`inst/htmlwidgets/lib/neurosurface/surfview.embed.iife.js`, so package
users and most contributors never need Node.

The bundle is pinned in
`inst/htmlwidgets/lib/neurosurface/surfview.embed.commit`, which records
the surfviewjs commit, a runtime patch (`tools/surfview-runtime.patch`)
and its checksum, the package version, the artifact SHA-256, and the
Three.js revision.

The build targets need git, Node.js 18 or later, npm, and a surfviewjs
checkout (default `~/code/jscode/surfviewjs`; override with
`SURFVIEWJS_DIR=...`):

``` bash
make verify-build     # offline: bundle, patch, and marker checksums agree
make surfview         # rebuild the pinned commit; must reproduce the SHA-256
make surfview-repin   # pin the checkout's HEAD (or COMMIT=<sha>) and rewrite the marker
make show-version     # print the recorded provenance
```

Both build targets work from a clean `git archive` of the source commit
with the runtime patch applied, so uncommitted changes in the checkout
never reach the bundle. `make surfview` installs nothing unless the
rebuilt bundle matches the recorded SHA-256. To change the runtime
patch, edit `tools/surfview-runtime.patch` and run
`make surfview-repin`; commit the bundle, marker, and patch together.

## Code of Conduct

Please be respectful and constructive in all interactions.
