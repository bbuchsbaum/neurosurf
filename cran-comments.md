## Submission

This is the first CRAN submission of neurosurf 0.1.0.

## Dependencies

All strong dependencies are available on CRAN or supplied with R.
The package requires CRAN neuroim2 >= 0.19.1 and R >= 4.3.0. There are no
GitHub Remotes. The author name is unchanged; Authors@R uses a single given
name string to accommodate the pkgcheck ORCID parser.

## Source package and documentation

The locally built source tarball is approximately 4.4 MB. Built vignette
files total approximately 3.0 MB. A CI gate audits the actual tarball and
refuses source files above 10 MB or documentation above 5 MB.

The RGL vignette contains one small interactive scene. Its other examples
run on closed NULL devices, with a compact CPU illustration clearly marked
as such. Building the vignettes does not require browser captures. Other
interactive and static rendering examples are guarded according to their
actual backend requirements.

The active widget runtime is pinned to surfviewjs 1dfed43. Its matching
readable source and exact npm lockfile are included in surfview-source.tar.xz;
the runtime rebuild script verifies both the bundle and source archive hash.
Full original notices for surfview, Three.js, colormap, fflate and lerp are
preserved in the JavaScript and THIRD-PARTY-NOTICES.txt. Unused historical
viewer bundles, build dependencies, and font binaries are excluded.

Template/data copyright, modification records, exact upstream URLs and
SHA256 hashes are in inst/COPYRIGHTS and inst/extdata/PROVENANCE.*. The
complete FreeSurfer redistribution terms and CBIG MIT license are included.
The fsaverage5 files and Schaefer NIfTI contents were compared to pinned
upstream copies.

## Test fixtures

The old optional rscan downloads are retired: their redistribution
provenance could not be established. neurosurf_generate_testdata() writes
small synthetic GIFTI/NIML fixtures locally, under GPL version 2 or later.
The old neurosurf_download_testdata() name remains a compatibility wrapper.
No examples or fixture tests use network resources. Defaults use a
versioned package cache; check examples and tests use temporary directories.
The installed package directory is never a default output destination.

## Check environments and results

* Local: macOS Sonoma arm64, R 4.5.1, CRAN neuroim2 0.19.1;
  `R CMD check --as-cran` including PDF manual: 0 errors, 0 warnings,
  3 notes. Tests and all four vignette rebuilds passed.
* The release-branch GitHub Actions workflow checks macOS and Windows
  R-release, plus Linux R-release, R-devel and oldrel-1. Its Linux release
  job checks the audited tarball and retains that source artifact and its
  hash inventory under a name containing the exact commit SHA.
  Candidate results are available at
  https://github.com/bbuchsbaum/neurosurf/actions/workflows/R-CMD-check.yaml?query=branch%3Acran%2F0.1.0

## Notes

* New submission: expected for the first submission.
* Unable to verify current time: local check environment limitation.
* HTML Tidy is outdated locally, so HTML manual validation was skipped;
  PDF manual generation passed. The R-generated HTML manual must still
  be validated where a current HTML Tidy is available.
