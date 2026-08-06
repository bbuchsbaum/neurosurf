
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build
Status](https://app.travis-ci.com/bbuchsbaum/neurosurf.svg?branch=master)](https://app.travis-ci.com/bbuchsbaum/neurosurf)

# neurosurf

`neurosurf` is an R package for reading, manipulating, and visualizing
surface-based neuroimaging data represented as triangle meshes. It
focuses on surface geometry, vertex-wise data, smoothing, geodesic
neighborhoods, and both static and interactive rendering workflows.

The package is under active development.

## Installation

Install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("bbuchsbaum/neurosurf")
```

## Start here

The pkgdown site includes a few good entry points:

- [Introduction to NeuroSurf Data
  Structures](https://bbuchsbaum.github.io/neurosurf/articles/introduction-to-neurosurf.html)
- [Displaying Surfaces with
  RGL](https://bbuchsbaum.github.io/neurosurf/articles/displaying-surfaces.html)
- [Build bilateral interactive surface
  reports](https://bbuchsbaum.github.io/neurosurf/articles/interactive-surfaces.html)
- [Publication-quality surface
  figures](https://bbuchsbaum.github.io/neurosurf/articles/surface-figures.html)
- [Reference
  index](https://bbuchsbaum.github.io/neurosurf/reference/index.html)

The interactive-report article shows how to place both hemispheres and
several named maps in one `SurfaceScene`. The resulting `surfwidget()`
shares geometry across maps, runs without a CDN, and can also be written
as a self-contained or adjacent-asset HTML page.

## Development setup

`neurosurf` includes an interactive 3D visualization component powered
by [surfviewjs](https://github.com/bbuchsbaum/surfviewjs). The
report-safe embed bundle is committed, so package users do not need
Node. Maintainers control the source checkout at
`~/code/jscode/surfviewjs`; `make surfview` rebuilds it and records the
exact source commit and artifact checksum in
`inst/htmlwidgets/lib/neurosurface/surfview.embed.commit`.

### Prerequisites

- Node.js 18 or higher
- npm

### Building surfviewjs

``` bash
# Build the surfviewjs library and copy it into neurosurf
make surfview

# Verify the copied artifact and provenance marker
make verify-build

# Clean build artifacts
make clean-surfview
```

The Makefile will:

1.  Build the dedicated embed bundle from the controlled `surfviewjs`
    checkout
2.  Copy `surfview.embed.iife.js` into the R package
3.  Record its source commit, package version, SHA-256, and Three.js
    revision
4.  Verify that `surfwidget.yaml` loads that one self-contained
    dependency

### Version and provenance

``` bash
make show-version
```

`paper-light` is the default appearance preset: a white-background,
publication-oriented combination of lighting, material, and
figure-export defaults. It is not a widget class or an interaction mode.
`mode = "report"` separately enables the compact map, anatomical-view,
reset, fullscreen, and PNG controls. The old Tweakpane control surface
is deprecated and is not included in generated reports.

<!-- albersdown:theme-note:start -->

## Albers theme

This package uses the albersdown theme. Existing vignette theme hooks
are replaced so `albers.css` and local `albers.js` render consistently
on CRAN and GitHub Pages. The defaults are configured through
`params$family` and `params$preset` (`family = "teal"`,
`preset = "homage"`). The pkgdown site uses
`template: { package: albersdown }` together with generated
`pkgdown/extra.css` and `pkgdown/extra.js` so the theme is linked and
activated on site pages. <!-- albersdown:theme-note:end -->
