source("tools/pkgcheck-report.R")

check_report <- function(input, expected) {
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  write_pkgcheck_markdown(input, path)
  stopifnot(identical(readLines(path, warn = FALSE), expected))
}

check_report(c("heading", "---", "details"),
             c("heading", "---", "details"))
check_report(list("heading", list("first", c("second", "third"))),
             c("heading", "first", "second", "third"))
check_report(list(), character())
check_report(c("heading", "details"), c("heading", "details"))
stopifnot(identical(pkgcheck_summary(c("heading", "---", "details")),
                    "heading"))
stopifnot(identical(pkgcheck_summary(list("heading", list("details"))),
                    c("heading", "details")))
stopifnot(identical(pkgcheck_summary(character()), character()))
stopifnot(inherits(try(normalize_markdown(list(1)), silent = TRUE),
                   "try-error"))
cat("pkgcheck report serialization probes passed\n")
