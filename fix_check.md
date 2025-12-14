- [x] Examples: `ColorMappedNeuroSurface` example fails because `read_surf_geometry` cannot find `std.8.lh.inflated.asc` in extdata. Ensure the file exists in `inst/extdata` and the example path resolves, or adjust the example to a shipped file.

- [x] Imports: Remove unused `fastmap` from `Imports` or use it; fix missing `htmlwidgets::invokeMethod` export/import; avoid `neuroim2:::parse_niml_file` or switch to an exported helper.

- [x] SurfaceSet docs: Add Rd entries for S4 methods `curvature/geometry/graph/nodes/vertices` on `SurfaceSet`.

- [x] Tests deps: Declare `bench` in Suggests/Imports if tests use it, or guard those tests.

- [x] Hidden files: Exclude `.claude` directory from the build (already in `.Rbuildignore`? verify removal from source).

- [x] Rd link: Add a package-qualified anchor for `FileFormat-class` in `read_meta_info-methods.Rd` or adjust the link target.
