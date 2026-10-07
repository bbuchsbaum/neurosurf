#' Generate synthetic surface test data
#'
#' Creates small, deterministic GIFTI and NIML fixtures locally. These files
#' contain 16 vertex rows and four synthetic time points, with no subject data
#' or external downloads. They are licensed under GPL version 2 or later,
#' like the generation code in neurosurf.
#'
#' @param files Character vector of fixture names, or \code{"all"}. Supported
#'   names are \code{"rscan01_lh.gii"}, \code{"rscan01_lh.niml.dset"}, and
#'   \code{"rscan01_rh.niml.dset"}. The historical names are retained for
#'   compatibility; the contents are synthetic, not the former large datasets.
#' @param destdir Existing destination directory. By default, a versioned
#'   directory in \code{tools::R_user_dir("neurosurf", "cache")} is created.
#'   The installed package directory is never a default destination.
#' @param overwrite Replace existing files? Defaults to \code{FALSE}.
#' @param quiet Suppress progress messages? Defaults to \code{FALSE}.
#' @return Invisibly returns the paths to the fixtures.
#' @details For the left hemisphere, value \eqn{(i,j)} is
#'   \eqn{\sin(i/3) + \cos(j/5)}, for vertices \eqn{i=1,\ldots,16} and
#'   time points \eqn{j=1,\ldots,4}. Right-hemisphere values are the negative
#'   of the left values. Node indices in the NIML files are zero-based.
#'
#'   \code{neurosurf_download_testdata()} is a compatibility wrapper for this
#'   generator. It no longer downloads the former external test files, whose
#'   redistribution provenance could not be established. Existing files in
#'   an explicitly supplied destination are left unchanged unless
#'   \code{overwrite = TRUE}; their original licenses still apply.
#' @examples
#' local({
#'   dest <- tempfile("neurosurf-fixtures-")
#'   dir.create(dest)
#'   on.exit(unlink(dest, recursive = TRUE))
#'   paths <- neurosurf_generate_testdata(destdir = dest, quiet = TRUE)
#'   basename(paths)
#'   gifti::readgii(paths[1])$data[[1]][1:3, , drop = FALSE]
#' })
#' @export
neurosurf_generate_testdata <- function(files = "all", destdir = NULL,
                                       overwrite = FALSE, quiet = FALSE) {
  available_files <- c("rscan01_lh.gii", "rscan01_lh.niml.dset",
                       "rscan01_rh.niml.dset")
  if (identical(files, "all")) {
    files <- available_files
  } else {
    invalid <- setdiff(files, available_files)
    if (length(invalid)) {
      stop("Unknown files: ", paste(invalid, collapse = ", "),
           "\nAvailable: ", paste(available_files, collapse = ", "))
    }
  }
  if (is.null(destdir)) {
    destdir <- file.path(neurosurf_cache_dir(), "extdata")
    if (!dir.exists(destdir)) {
      dir.create(destdir, recursive = TRUE, showWarnings = FALSE)
    }
  }
  if (!dir.exists(destdir)) {
    stop("Destination directory does not exist: ", destdir)
  }
  values <- outer(seq_len(16), seq_len(4),
                  function(i, j) sin(i / 3) + cos(j / 5))
  paths <- character()
  for (f in files) {
    dest_path <- file.path(destdir, f)
    if (file.exists(dest_path) && !overwrite) {
      if (!quiet) message("File already exists: ", f)
      paths <- c(paths, dest_path)
      next
    }
    staged <- tempfile(".neurosurf-fixture-", tmpdir = destdir)
    if (!quiet) message("Generating synthetic fixture: ", f)
    fixture_values <- if (grepl("_rh", f, fixed = TRUE)) -values else values
    tryCatch({
      .ns_write_testdata(staged, f, fixture_values)
      if (!file.copy(staged, dest_path, overwrite = TRUE)) {
        stop("Could not install fixture: ", dest_path)
      }
    }, finally = unlink(staged))
    paths <- c(paths, dest_path)
  }
  invisible(paths)
}

#' @rdname neurosurf_generate_testdata
#' @export
neurosurf_download_testdata <- function(files = "all", destdir = NULL,
                                       overwrite = FALSE, quiet = FALSE) {
  neurosurf_generate_testdata(files, destdir, overwrite, quiet)
}

.ns_write_testdata <- function(path, name, values) {
  if (grepl("[.]gii$", name)) {
    text <- c(
      '<?xml version="1.0" encoding="UTF-8"?>',
      '<GIFTI Version="1.0" NumberOfDataArrays="1">',
      '<MetaData><MD><Name>Description</Name>',
      '<Value>Synthetic neurosurf fixture; GPL version 2 or later</Value>',
      '</MD></MetaData><LabelTable/>',
      '<DataArray Intent="NIFTI_INTENT_TIME_SERIES"',
      ' DataType="NIFTI_TYPE_FLOAT32" ArrayIndexingOrder="RowMajorOrder"',
      ' Dimensionality="2" Dim0="16" Dim1="4" Encoding="ASCII"',
      ' Endian="LittleEndian" ExternalFileName="" ExternalFileOffset="0">',
      '<MetaData/><Data>',
      paste(sprintf("%.9g", as.vector(t(values))), collapse = " "),
      '</Data></DataArray></GIFTI>'
    )
    writeLines(text, path, useBytes = TRUE)
  } else {
    con <- file(path, "wb")
    on.exit(close(con), add = TRUE)
    writeChar(paste0('<AFNI_dataset ni_form="ni_group" ',
                     'label="Synthetic_neurosurf_GPL_2_or_later">\n'),
              con, eos = NULL)
    writeChar(paste0('<INDEX_LIST ni_type="int" ni_dimen="16" ',
                     'ni_form="binary.lsbfirst">'), con, eos = NULL)
    writeBin(as.integer(0:15), con, size = 4, endian = "little")
    writeChar(paste0('</INDEX_LIST>\n<SPARSE_DATA ',
                     'ni_type="4*float" ni_dimen="16" ',
                     'ni_form="binary.lsbfirst">'), con, eos = NULL)
    writeBin(as.vector(t(values)), con, size = 4, endian = "little")
    writeChar('</SPARSE_DATA>\n</AFNI_dataset>\n', con, eos = NULL)
  }
  invisible(path)
}

#' Get the synthetic fixture cache directory
#' @return Path to the versioned cache directory.
#' @keywords internal
neurosurf_cache_dir <- function() {
  file.path(tools::R_user_dir("neurosurf", which = "cache"), "synthetic-v1")
}

#' Check if test data is available
#' @param file File name.
#' @return Logical indicating availability.
#' @keywords internal
neurosurf_has_testdata <- function(file) {
  nzchar(neurosurf_testdata_path(file))
}

#' Locate bundled data or synthetic fixtures
#' @param file File name.
#' @return File path, or an empty string if it does not exist.
#' @keywords internal
neurosurf_testdata_path <- function(file) {
  # The old rscan files in developer checkouts have unknown provenance.
  # Never select them as fixtures; use only the versioned synthetic cache.
  if (!startsWith(file, "rscan01_")) {
    path <- system.file("extdata", file, package = "neurosurf")
    if (nzchar(path) && file.exists(path)) return(path)
  }
  path <- file.path(neurosurf_cache_dir(), "extdata", file)
  if (file.exists(path)) path else ""
}
