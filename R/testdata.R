#' Download optional test data for neurosurf
#'
#' Downloads larger test data files that are not included in the CRAN package
#' due to size constraints. These files are hosted on GitHub releases and are
#' useful for running the full test suite or exploring additional file formats.
#'
#' @param files Character vector of files to download. Use \code{"all"} to
#'   download all available test files. Available files:
#'   \itemize{
#'     \item \code{"rscan01_lh.gii"} - GIFTI surface file (30 MB)
#'     \item \code{"rscan01_lh.niml.dset"} - NIML surface dataset (22 MB)
#'     \item \code{"rscan01_rh.niml.dset"} - NIML surface dataset (22 MB)
#'   }
#' @param destdir Destination directory. Defaults to \code{extdata} within the
#'   installed package location. If the package location is not writable,
#'   defaults to \code{rappdirs::user_cache_dir("neurosurf")}.
#' @param overwrite Logical; if \code{TRUE}, re-download files even if they
#'   already exist. Default is \code{FALSE}.
#' @param quiet Logical; if \code{TRUE}, suppress progress messages.
#'
#' @return Invisibly returns the paths to downloaded files.
#'
#' @details
#' The test data files are hosted on GitHub releases for the neurosurf package.
#' This function requires an internet connection to download the files.
#'
#' If you are running tests locally and want the full test suite, run
#' \code{neurosurf_download_testdata("all")} once after installing the package.
#'
#' @examples
#' \donttest{
#' # Download all test data
#' neurosurf_download_testdata("all")
#'
#' # Download only the GIFTI file
#' neurosurf_download_testdata("rscan01_lh.gii")
#' }
#'
#' @export
neurosurf_download_testdata <- function(
    files = "all",
    destdir = NULL,
    overwrite = FALSE,
    quiet = FALSE
) {
  available_files <- c(
    "rscan01_lh.gii",
    "rscan01_lh.niml.dset",
    "rscan01_rh.niml.dset"
  )


  if (identical(files, "all")) {
    files <- available_files
  } else {
    invalid <- setdiff(files, available_files)
    if (length(invalid) > 0) {
      stop("Unknown files: ", paste(invalid, collapse = ", "),
           "\nAvailable: ", paste(available_files, collapse = ", "))
    }
  }

  # Determine destination directory

if (is.null(destdir)) {
    pkg_extdata <- system.file("extdata", package = "neurosurf")
    if (pkg_extdata != "" && file.access(pkg_extdata, 2) == 0) {
      destdir <- pkg_extdata
    } else {
      destdir <- file.path(neurosurf_cache_dir(), "extdata")
      if (!dir.exists(destdir)) {
        dir.create(destdir, recursive = TRUE)
      }
    }
  }

  if (!dir.exists(destdir)) {
    stop("Destination directory does not exist: ", destdir)
  }

  base_url <- "https://github.com/bbuchsbaum/neurosurf/releases/download/testdata-v1/"

  downloaded <- character()
  for (f in files) {
    dest_path <- file.path(destdir, f)

    if (file.exists(dest_path) && !overwrite) {
      if (!quiet) message("File already exists: ", f)
      downloaded <- c(downloaded, dest_path)
      next
    }

    url <- paste0(base_url, f)
    if (!quiet) message("Downloading ", f, "...")

    tryCatch({
      utils::download.file(url, dest_path, mode = "wb", quiet = quiet)
      downloaded <- c(downloaded, dest_path)
      if (!quiet) message("  -> ", dest_path)
    }, error = function(e) {
      warning("Failed to download ", f, ": ", conditionMessage(e))
    })
  }

  invisible(downloaded)
}


#' Get the neurosurf cache directory
#'
#' @return Path to the cache directory
#' @keywords internal
neurosurf_cache_dir <- function() {
  if (requireNamespace("rappdirs", quietly = TRUE)) {
    rappdirs::user_cache_dir("neurosurf")
  } else {
    # Fallback to a standard location
    file.path(tempdir(), "neurosurf_cache")
  }
}


#' Check if optional test data is available
#'
#' @param file Name of the test data file to check.
#' @return Logical indicating whether the file is available.
#' @keywords internal
neurosurf_has_testdata <- function(file) {
  # Check package extdata first
  pkg_path <- system.file("extdata", file, package = "neurosurf")
  if (pkg_path != "" && file.exists(pkg_path)) {
    return(TRUE)
  }

  # Check user cache
  cache_path <- file.path(neurosurf_cache_dir(), "extdata", file)
  file.exists(cache_path)
}


#' Get path to test data file
#'
#' Returns the path to a test data file, checking both the package extdata
#' directory and the user cache directory.
#'
#' @param file Name of the test data file.
#' @return Path to the file, or empty string if not found.
#' @keywords internal
neurosurf_testdata_path <- function(file) {
  # Check package extdata first
  pkg_path <- system.file("extdata", file, package = "neurosurf")
  if (pkg_path != "" && file.exists(pkg_path)) {
    return(pkg_path)
  }

  # Check user cache
  cache_path <- file.path(neurosurf_cache_dir(), "extdata", file)
  if (file.exists(cache_path)) {
    return(cache_path)
  }

  ""
}
