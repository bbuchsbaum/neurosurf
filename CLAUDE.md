# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Project Overview

`neurosurf` is an R package for surface-based neuroimaging data
analysis. It provides data structures and visualization tools for
working with triangle mesh surfaces (e.g., cortical surfaces) with
support for FreeSurfer and GIFTI formats.

## Key Commands

### Package Development

``` bash
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

``` bash
# Navigate to JavaScript source
cd inst/htmlwidgets/neurosurface

# Install dependencies
npm install

# Build JavaScript bundle
npm run build
```

## Architecture

### R Package Structure

- **S4 Classes**: The package uses S4 object-oriented programming
  extensively
  - Core geometry classes: `SurfaceGeometry`, `NeuroSurface`,
    `ColorMappedNeuroSurface`
  - Vector classes: `NeuroSurfaceVector`, `BilatNeuroSurfaceVector`
  - ROI classes: `ROISurface`, `ROISurfaceVector`
  - File format classes for FreeSurfer, GIFTI, AFNI, and NIML formats
- **Key Components**:
  - `geometry.R`: Core surface geometry operations and data structures
  - `neuro_surface.R`: Surface data mapping and visualization structures
  - `surfwidget.R`: Interactive 3D visualization using htmlwidgets
  - `Searchlight.R`: Surface-based searchlight analysis for machine
    learning
  - `neighborhood.R`: Graph-based neighborhood computations on surfaces
  - `IO.R`: File I/O for various surface formats

### JavaScript/HTMLWidget Architecture

- Uses Three.js for 3D rendering
- Tweakpane for UI controls
- Built with Rollup bundler
- Main viewer: `inst/htmlwidgets/neurosurface/src/NeuroSurfaceViewer.js`

## Documentation Guidelines

When documenting S4 methods (per `.cursor/rules/roxygendocs.mdc`): -
Document parameters, description, and examples ONLY in the generic
function - Methods should only have `@rdname` matching the generic and
`@export` if needed - Use `@details` or `@note` in methods only for
method-specific behavior - Never use `@dontrun`, use `@donttest` for
long-running examples

## Testing

Tests use `testthat` framework and are located in `tests/testthat/`. Key
test areas: - Boundary detection algorithms - File I/O for different
formats - Neighborhood graph computations - Searchlight analysis -
Surface smoothing operations

### Files Excluded from Coverage Targets

The following files have low or zero test coverage due to their nature:

- **`snapshot_surface.R`**: Screenshot functionality requiring
  interactive graphics device
- **`surfwidget.R`**: HTMLWidget creation requiring browser/JavaScript
  runtime
- **`view_surface.R`**: RGL 3D visualization requiring interactive
  graphics context
- **`zzz.R`**: Package initialization hooks (`.onLoad`, `.onAttach`)
- **`spec.R`**: Internal specification/configuration (no testable logic)

These files contain visualization, UI, or package infrastructure code
that cannot be meaningfully unit tested without interactive sessions or
external rendering contexts.

## Dependencies

Key R dependencies: - `igraph`: Graph operations on surface meshes -
`rgl`: 3D rendering (backend) - `Rvcg`: Mesh operations (smoothing,
remeshing) - `Matrix`: Sparse matrix operations - `htmlwidgets`:
Interactive visualizations - `neuroim2`: Neuroimaging data structures
(custom package)

## File Formats

The package supports: - FreeSurfer surface formats (ASCII and binary) -
GIFTI surface format - AFNI/SUMA surface formats - NIML format

## HTMLWidget Integration

The
[`surfwidget()`](https://bbuchsbaum.github.io/neurosurf/reference/surfwidget-methods.md)
function creates interactive 3D visualizations. Key features: -
Curvature-based shading (computed automatically for `SurfaceGeometry`
objects) - Multiple color mapping options - Interactive controls via
Tweakpane - Support for bilateral surface display

## Issue Tracking

This project uses **beads** for issue tracking. See AGENTS.md for full
workflow.

``` bash
bd list                   # List open issues
bd ready                  # Find tasks without blockers
bd create "title" -p 1    # Create issue (P0-P3 priority)
bd show neurosurf-xxx     # View issue details
bd close neurosurf-xxx    # Close issue
bd sync                   # Sync to git
```

## Session Management

When ending a work session, always “land the plane”:

1.  File issues for remaining work
2.  Run quality gates (`devtools::check()`, `devtools::test()`)
3.  Update issue status
4.  `bd sync && git push`
5.  Provide handoff context for next session

## MCP Agent Mail: coordination for multi-agent workflows

What it is - A mail-like layer that lets coding agents coordinate
asynchronously via MCP tools and resources. - Provides identities,
inbox/outbox, searchable threads, and advisory file reservations, with
human-auditable artifacts in Git.

Why it’s useful - Prevents agents from stepping on each other with
explicit file reservations (leases) for files/globs. - Keeps
communication out of your token budget by storing messages in a
per-project archive. - Offers quick reads (`resource://inbox/...`,
`resource://thread/...`) and macros that bundle common flows.

How to use effectively 1) Same repository - Register an identity: call
`ensure_project`, then `register_agent` using this repo’s absolute path
as `project_key`. - Reserve files before you edit:
`file_reservation_paths(project_key, agent_name, ["src/**"], ttl_seconds=3600, exclusive=true)`
to signal intent and avoid conflict. - Communicate with threads: use
`send_message(..., thread_id="FEAT-123")`; check inbox with
`fetch_inbox` and acknowledge with `acknowledge_message`. - Read fast:
`resource://inbox/{Agent}?project=<abs-path>&limit=20` or
`resource://thread/{id}?project=<abs-path>&include_bodies=true`. - Tip:
set `AGENT_NAME` in your environment so the pre-commit guard can block
commits that conflict with others’ active exclusive file reservations.

2.  Across different repos in one project (e.g., Next.js frontend +
    FastAPI backend)
    - Option A (single project bus): register both sides under the same
      `project_key` (shared key/path). Keep reservation patterns
      specific (e.g., `frontend/**` vs `backend/**`).
    - Option B (separate projects): each repo has its own `project_key`;
      use `macro_contact_handshake` or
      `request_contact`/`respond_contact` to link agents, then message
      directly. Keep a shared `thread_id` (e.g., ticket key) across
      repos for clean summaries/audits.

Macros vs granular tools - Prefer macros when you want speed or are on a
smaller model: `macro_start_session`, `macro_prepare_thread`,
`macro_file_reservation_cycle`, `macro_contact_handshake`. - Use
granular tools when you need control: `register_agent`,
`file_reservation_paths`, `send_message`, `fetch_inbox`,
`acknowledge_message`.

Common pitfalls - “from_agent not registered”: always `register_agent`
in the correct `project_key` first. - “FILE_RESERVATION_CONFLICT”:
adjust patterns, wait for expiry, or use a non-exclusive reservation
when appropriate. - Auth errors: if JWT+JWKS is enabled, include a
bearer token with a `kid` that matches server JWKS; static bearer is
used only when JWT is disabled.
