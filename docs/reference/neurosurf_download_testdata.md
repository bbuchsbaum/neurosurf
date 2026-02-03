# Download optional test data for neurosurf

Downloads larger test data files that are not included in the CRAN
package due to size constraints. These files are hosted on GitHub
releases and are useful for running the full test suite or exploring
additional file formats.

## Usage

``` r
neurosurf_download_testdata(
  files = "all",
  destdir = NULL,
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- files:

  Character vector of files to download. Use `"all"` to download all
  available test files. Available files:

  - `"rscan01_lh.gii"` - GIFTI surface file (30 MB)

  - `"rscan01_lh.niml.dset"` - NIML surface dataset (22 MB)

  - `"rscan01_rh.niml.dset"` - NIML surface dataset (22 MB)

- destdir:

  Destination directory. Defaults to `extdata` within the installed
  package location. If the package location is not writable, defaults to
  `rappdirs::user_cache_dir("neurosurf")`.

- overwrite:

  Logical; if `TRUE`, re-download files even if they already exist.
  Default is `FALSE`.

- quiet:

  Logical; if `TRUE`, suppress progress messages.

## Value

Invisibly returns the paths to downloaded files.

## Details

The test data files are hosted on GitHub releases for the neurosurf
package. This function requires an internet connection to download the
files.

If you are running tests locally and want the full test suite, run
`neurosurf_download_testdata("all")` once after installing the package.

## Examples

``` r
if (FALSE) { # \dontrun{
# Download all test data
neurosurf_download_testdata("all")

# Download only the GIFTI file
neurosurf_download_testdata("rscan01_lh.gii")
} # }
```
