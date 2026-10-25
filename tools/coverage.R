args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else "coverage"
if (dir.exists(output) &&
    length(list.files(output, all.files = TRUE, no.. = TRUE))) {
  stop("Coverage output directory must be empty: ", output)
}
dir.create(output, recursive = TRUE, showWarnings = FALSE)
artifact <- function(name) file.path(output, name)

# Keep the original object so the report can be inspected independently.
coverage <- covr::package_coverage(type = "tests", quiet = FALSE)
saveRDS(coverage, artifact("coverage.rds"))
covr::to_cobertura(coverage, artifact("cobertura.xml"))
writeLines(capture.output(sessionInfo()), artifact("session-info.txt"))

lines <- covr::tally_coverage(coverage)
if (nrow(lines) == 0L) {
  stop("Coverage produced no instrumented lines")
}
files <- split(lines$value, lines$filename)
per_file <- data.frame(
  file = names(files),
  covered = vapply(files, function(x) sum(x > 0), integer(1)),
  total = vapply(files, length, integer(1)),
  row.names = NULL
)
per_file$percent <- 100 * per_file$covered / per_file$total
write.csv(per_file, artifact("per-file.csv"), row.names = FALSE)

overall <- covr::percent_coverage(coverage)
summary <- c(
  "## Test coverage",
  "",
  sprintf("Overall: %.2f%% (%d of %d instrumented lines covered).",
          overall, sum(lines$value > 0), nrow(lines)),
  "",
  paste("Measured with covr::package_coverage(type = 'tests');",
        "NOT_CRAN=false uses standard headless CRAN skips and RGL_USE_NULL=TRUE."),
  "",
  "| File | Covered | Total | Coverage |",
  "| --- | ---: | ---: | ---: |",
  sprintf("| %s | %d | %d | %.2f%% |", per_file$file,
          per_file$covered, per_file$total, per_file$percent)
)
writeLines(summary, artifact("summary.md"))
cat(paste(summary, collapse = "\n"), "\n")
step_summary <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(step_summary)) {
  cat(paste(summary, collapse = "\n"), "\n", file = step_summary,
      append = TRUE)
}
