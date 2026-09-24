# Contributing to neurosurf

Thank you for your interest in contributing!

## Reporting bugs

Please open an issue at https://github.com/bbuchsbaum/neurosurf/issues.
Include a minimal reproducible example using the
[reprex](https://reprex.tidyverse.org) package.

## Suggesting features

Open an issue describing the proposed feature and use case.

## Submitting pull requests

1. Fork the repository and create a branch from `master`.
2. Install dependencies: `devtools::install_deps()`.
3. Make your changes and add tests in `tests/testthat/`.
4. Run `devtools::check()` and ensure 0 errors, 0 warnings, 0 notes.
5. Open a pull request with a clear description of the change.

## Editing the README

Edit `README.Rmd`, not `README.md`, and regenerate with
`devtools::build_readme()`. The quick-start chunk is evaluated, so the README
fails to build if its example stops working.

## Rebuilding the JavaScript viewer

The interactive viewer used by `surfwidget()` and `write_surface_scene()` is
[surfviewjs](https://github.com/bbuchsbaum/surfviewjs). Its report-safe embed
bundle is committed at
`inst/htmlwidgets/lib/neurosurface/surfview.embed.iife.js`, so package users
and most contributors never need Node.

The bundle is pinned in
`inst/htmlwidgets/lib/neurosurface/surfview.embed.commit`, which records the
surfviewjs commit, a runtime patch (`tools/surfview-runtime.patch`) and its
checksum, the package version, the artifact SHA-256, and the Three.js revision.
To reproduce the shipped bundle from source you need git, Node.js 18 or later,
npm, and a surfviewjs checkout containing the pinned commit:

```bash
python3 tools/rebuild-surfview-runtime.py ~/code/jscode/surfviewjs
make show-version    # print the recorded provenance
```

The script builds a clean archive of the pinned commit with the patch applied,
and installs the result only if its SHA-256 matches the recorded one. Moving to
a new surfviewjs commit or patch therefore means updating the marker's
`commit`, `patch_sha256`, `version`, and `sha256` fields together with the
bundle.

`make surfview` builds from the checkout's current `HEAD` without the runtime
patch and rewrites the marker without the patch fields, so it does not
reproduce the shipped bundle.

## Code of Conduct

Please be respectful and constructive in all interactions.
