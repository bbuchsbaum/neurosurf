## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Notes

* "New submission" — this is a first CRAN submission.

## Bundled files and tarball size

The source tarball is approximately 4.9 MB. It bundles a small set of
example data used by the examples, tests, and vignettes, plus the
JavaScript assets for the interactive viewer:

* decimated fsaverage `std.8` cortical surfaces (FreeSurfer ASCII),
* a Schaefer-200 parcellation volume (compressed NIfTI), and
* JavaScript assets (Three.js, Tweakpane) for the htmlwidgets-based
  interactive 3D surface viewer.

Larger optional test datasets are not shipped; they are downloaded on
demand via `neurosurf_download_testdata()`.

## Examples and vignettes

Some examples drive interactive 3D (rgl) rendering and are therefore
wrapped in `if (interactive())` so they do not require a graphics or
browser backend during automated checks. Vignettes render surfaces
off-screen and degrade gracefully when no rendering backend is available,
so they build without error on headless machines.

## Test environments

* local: macOS, R release — 0 errors | 0 warnings | 1 note
* R-universe: Linux, macOS, and Windows (release, devel, oldrel), plus
  source and WebAssembly builds.
