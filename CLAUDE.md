# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`neurosurf` is an R package for surface-based neuroimaging data analysis. It provides data structures and visualization tools for working with triangle mesh surfaces (e.g., cortical surfaces) with support for FreeSurfer and GIFTI formats.

## Key Commands

### Package Development
```bash
# Install dependencies and build package
R -e "devtools::install_deps()"
R -e "devtools::build()"

# Check package (runs R CMD check)
R -e "devtools::check()"

# Run tests
R -e "devtools::test()"

# Run a single test file
R -e "testthat::test_file('tests/testthat/test_boundaries_methods.R')"

# Document package (generate/update roxygen documentation)
R -e "devtools::document()"

# Build and reload package
R -e "devtools::load_all()"
```

### JavaScript Development (for htmlwidgets)
```bash
# Navigate to JavaScript source
cd inst/htmlwidgets/neurosurface

# Install dependencies
npm install

# Build JavaScript bundle
npm run build
```

## Architecture

### R Package Structure
- **S4 Classes**: The package uses S4 object-oriented programming extensively
  - Core geometry classes: `SurfaceGeometry`, `NeuroSurface`, `ColorMappedNeuroSurface`
  - Vector classes: `NeuroSurfaceVector`, `BilatNeuroSurfaceVector`
  - ROI classes: `ROISurface`, `ROISurfaceVector`
  - File format classes for FreeSurfer, GIFTI, AFNI, and NIML formats

- **Key Components**:
  - `geometry.R`: Core surface geometry operations and data structures
  - `neuro_surface.R`: Surface data mapping and visualization structures
  - `surfwidget.R`: Interactive 3D visualization using htmlwidgets
  - `Searchlight.R`: Surface-based searchlight analysis for machine learning
  - `neighborhood.R`: Graph-based neighborhood computations on surfaces
  - `IO.R`: File I/O for various surface formats

### JavaScript/HTMLWidget Architecture
- Uses Three.js for 3D rendering
- Tweakpane for UI controls
- Built with Rollup bundler
- Main viewer: `inst/htmlwidgets/neurosurface/src/NeuroSurfaceViewer.js`

## Documentation Guidelines

When documenting S4 methods (per `.cursor/rules/roxygendocs.mdc`):
- Document parameters, description, and examples ONLY in the generic function
- Methods should only have `@rdname` matching the generic and `@export` if needed
- Use `@details` or `@note` in methods only for method-specific behavior
- Never use `@dontrun`, use `@donttest` for long-running examples

## Testing

Tests use `testthat` framework and are located in `tests/testthat/`. Key test areas:
- Boundary detection algorithms
- File I/O for different formats
- Neighborhood graph computations
- Searchlight analysis
- Surface smoothing operations

## Dependencies

Key R dependencies:
- `igraph`: Graph operations on surface meshes
- `rgl`: 3D rendering (backend)
- `Rvcg`: Mesh operations (smoothing, remeshing)
- `Matrix`: Sparse matrix operations
- `htmlwidgets`: Interactive visualizations
- `neuroim2`: Neuroimaging data structures (custom package)

## File Formats

The package supports:
- FreeSurfer surface formats (ASCII and binary)
- GIFTI surface format
- AFNI/SUMA surface formats
- NIML format

## HTMLWidget Integration

The `surfwidget()` function creates interactive 3D visualizations. Key features:
- Curvature-based shading (computed automatically for `SurfaceGeometry` objects)
- Multiple color mapping options
- Interactive controls via Tweakpane
- Support for bilateral surface display
