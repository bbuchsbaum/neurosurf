# Generate synthetic surface test data

Creates small, deterministic GIFTI and NIML fixtures locally. These
files contain 16 vertex rows and four synthetic time points, with no
subject data or external downloads. They are licensed under GPL version
2 or later, like the generation code in neurosurf.

## Usage

``` r
neurosurf_generate_testdata(
  files = "all",
  destdir = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

neurosurf_download_testdata(
  files = "all",
  destdir = NULL,
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- files:

  Character vector of fixture names, or `"all"`. Supported names are
  `"rscan01_lh.gii"`, `"rscan01_lh.niml.dset"`, and
  `"rscan01_rh.niml.dset"`. The historical names are retained for
  compatibility; the contents are synthetic, not the former large
  datasets.

- destdir:

  Existing destination directory. By default, a versioned directory in
  `tools::R_user_dir("neurosurf", "cache")` is created. The installed
  package directory is never a default destination.

- overwrite:

  Replace existing files? Defaults to `FALSE`.

- quiet:

  Suppress progress messages? Defaults to `FALSE`.

## Value

Invisibly returns the paths to the fixtures.

## Details

For the left hemisphere, value \\(i,j)\\ is \\\sin(i/3) + \cos(j/5)\\,
for vertices \\i=1,\ldots,16\\ and time points \\j=1,\ldots,4\\.
Right-hemisphere values are the negative of the left values. Node
indices in the NIML files are zero-based.

`neurosurf_download_testdata()` is a compatibility wrapper for this
generator. It no longer downloads the former external test files, whose
redistribution provenance could not be established. Existing files in an
explicitly supplied destination are left unchanged unless
`overwrite = TRUE`; their original licenses still apply.

## Examples

``` r
local({
  dest <- tempfile("neurosurf-fixtures-")
  dir.create(dest)
  on.exit(unlink(dest, recursive = TRUE))
  paths <- neurosurf_generate_testdata(destdir = dest, quiet = TRUE)
  basename(paths)
  gifti::readgii(paths[1])$data[[1]][1:3, , drop = FALSE]
})
#>          [,1]     [,2]     [,3]     [,4]
#> [1,] 1.307261 1.248256 1.152530 1.023901
#> [2,] 1.598436 1.539431 1.443705 1.315077
#> [3,] 1.821538 1.762532 1.666807 1.538178
```
