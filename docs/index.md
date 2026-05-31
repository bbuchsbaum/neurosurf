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
- [Interactive Surface Visualization with
  surfwidget](https://bbuchsbaum.github.io/neurosurf/articles/interactive-surfaces.html)
- [Publication-quality surface
  figures](https://bbuchsbaum.github.io/neurosurf/articles/index.html)
- [Reference
  index](https://bbuchsbaum.github.io/neurosurf/reference/index.html)

## Development setup

`neurosurf` includes an interactive 3D visualization component powered
by [surfviewjs](https://github.com/bbuchsbaum/surfviewjs). The built UMD
bundle is committed so
[`devtools::install_github()`](https://devtools.r-lib.org/reference/install-deprecated.html)
works without Node; Node is only needed if you want to rebuild the
JavaScript library locally.

### Prerequisites

- Node.js (v14 or higher)
- npm

### Building surfviewjs

``` bash
# Build the surfviewjs library and copy it into neurosurf
make surfview

# Check build status
make status

# Clean build artifacts
make clean-surfview
```

The Makefile will:

1.  Install npm dependencies in `surfviewjs` if needed
2.  Build the library with Vite
3.  Copy the UMD bundle to `inst/htmlwidgets/lib/neurosurface/`
4.  Synchronize the version number into `surfwidget.yaml`

### Workflow integration

``` bash
# Before R CMD check
make r-check

# Install package
make r-install

# Watch for changes during development
make watch-surfview  # Requires fswatch
```

### Version management

The version number in `surfwidget.yaml` is synchronized with
`surfviewjs/package.json` when `make surfview` runs. To update it
manually:

``` bash
make update-yaml
make show-version
```

## Albers theme

This package uses the albersdown theme. Existing vignette theme hooks
are replaced so `albers.css` and local `albers.js` render consistently
on CRAN and GitHub Pages. The defaults are configured via
`params$family` and `params$preset` (family = ‘teal’, preset =
‘homage’). The pkgdown site uses `template: { package: albersdown }`.
