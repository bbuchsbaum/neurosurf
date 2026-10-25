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

run_pkgcheck_report <- function() {
  dir.create("pkgcheck-report", showWarnings = FALSE)
  on.exit(writeLines(capture.output(sessionInfo()),
                     "pkgcheck-report/session-info.txt"), add = TRUE)
  checks <- pkgcheck::pkgcheck(path = ".", use_cache = FALSE)
  saveRDS(checks, "pkgcheck-report/pkgcheck.rds")
  writeLines(capture.output(summary(checks)), "pkgcheck-report/summary.txt")
  markdown <- pkgcheck::checks_to_markdown(checks, render = FALSE)
  write_pkgcheck_markdown(markdown, "pkgcheck-report/report.md")
}

if (sys.nframe() == 0L) run_pkgcheck_report()
