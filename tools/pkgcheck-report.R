normalize_markdown <- function(x) {
  if (is.null(x)) return(character())
  if (is.character(x)) return(unname(x))
  if (is.list(x)) {
    return(as.character(unlist(lapply(x, normalize_markdown),
                               use.names = FALSE)))
  }
  stop("pkgcheck markdown contains a non-character value")
}

write_pkgcheck_markdown <- function(markdown, path) {
  writeLines(normalize_markdown(markdown), path, useBytes = TRUE)
}

pkgcheck_summary <- function(markdown) {
  lines <- normalize_markdown(markdown)
  separator <- which(trimws(lines) == "---")
  if (length(separator)) lines <- head(lines, separator[[1L]] - 1L)
  lines
}

run_pkgcheck_report <- function() {
  if (dir.exists("pkgcheck-report") &&
      length(list.files("pkgcheck-report", all.files = TRUE, no.. = TRUE))) {
    stop("pkgcheck-report must be empty to avoid stale report artifacts")
  }
  dir.create("pkgcheck-report", showWarnings = FALSE)
  on.exit(writeLines(capture.output(sessionInfo()),
                     "pkgcheck-report/session-info.txt"), add = TRUE)
  checks <- pkgcheck::pkgcheck(path = ".", use_cache = FALSE)
  saveRDS(checks, "pkgcheck-report/pkgcheck.rds")
  markdown <- pkgcheck::checks_to_markdown(checks, render = FALSE)
  write_pkgcheck_markdown(markdown, "pkgcheck-report/report.md")
  write_pkgcheck_markdown(pkgcheck_summary(markdown),
                         "pkgcheck-report/summary.md")
}

if (sys.nframe() == 0L) run_pkgcheck_report()
