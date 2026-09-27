# Reproduce the GH #88 fsaverage5 / Schaefer-200 qualification.
# From the package root:
# RGL_USE_NULL=TRUE /usr/bin/time -l Rscript --vanilla \
#   tools/check-parcel-partition.R /path/to/lh.annot /tmp/parcel-check
# Download the pinned annotation URL recorded below before running. No external
# resources are fetched by this script. Peak RSS is reported by /usr/bin/time.

Sys.setenv(RGL_USE_NULL = "TRUE")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Supply annotation path and output directory")
annotation <- normalizePath(args[1], mustWork = TRUE)
output <- args[2]
dir.create(output, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(quiet = TRUE)
internal <- function(name) get(paste0(".ns_", name), asNamespace("neurosurf"))

source_sha <- "35b5664bec8822e2f77da5e090e96f91d0095be6"
source_url <- paste0(
  "https://raw.githubusercontent.com/ThomasYeoLab/CBIG/", source_sha,
  "/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/",
  "Parcellations/FreeSurfer5.3/fsaverage5/label/",
  "lh.Schaefer2018_200Parcels_7Networks_order.annot")
annotation_hash <- digest::digest(file = annotation, algo = "sha256")
stopifnot(annotation_hash ==
  "6c2d0e6e85841f9031f4193a9ceaca34023857c9aaf68538801d78b1ec367b5f")
surface_path <- "inst/extdata/fsaverage5/infl_left.gii.gz"
surface_hash <- digest::digest(file = surface_path, algo = "sha256")
stopifnot(surface_hash ==
  "1a571c9d598a476a202c7295c235a709de63360047b92c0e7b75ed18b0f37e74")

# Map packed RGB codes through the annotation's explicit colour-table IDs.
# Its background has ID 0 but a nonzero RGB code. No resampling occurs.
read_codes <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  n <- readBin(con, integer(), n = 1L, size = 4L, endian = "big")
  records <- matrix(readBin(con, integer(), n = 2L * n, size = 4L,
                            endian = "big"), ncol = 2L, byrow = TRUE)
  stopifnot(nrow(records) == n, identical(sort(records[, 1]), 0:(n - 1L)))
  header <- readBin(con, integer(), n = 4L, size = 4L, endian = "big")
  stopifnot(header[1] == 1L, header[2] == -2L)
  readBin(con, "raw", n = header[4])
  count <- readBin(con, integer(), n = 1L, size = 4L, endian = "big")
  codes <- ids <- integer(count)
  for (i in seq_len(count)) {
    entry <- readBin(con, integer(), n = 2L, size = 4L, endian = "big")
    readBin(con, "raw", n = entry[2])
    rgba <- readBin(con, integer(), n = 4L, size = 4L, endian = "big")
    ids[i] <- entry[1]
    codes[i] <- sum(rgba[1:3] * c(1L, 256L, 65536L))
  }
  labels <- ids[match(records[order(records[, 1]), 2], codes)]
  stopifnot(!anyNA(labels), any(labels == 0L))
  labels
}
labels <- read_codes(annotation)
geometry <- load_fsaverage("fsaverage5", "inflated")$lh
vertices <- t(geometry@mesh$vb[1:3, , drop = FALSE])
faces <- t(geometry@mesh$it)
stopifnot(length(labels) == nrow(vertices), any(labels == 0L))
ids <- sort(unique(labels[labels != 0L]))
colors <- stats::setNames(grDevices::hcl.colors(length(ids), "Set3"), ids)

measurements <- list()
for (iterations in c(0L, 3L)) {
  timing <- system.time({
    prep <- prepare_surface_parcels(geometry, labels,
                                    boundary_method = "smooth",
                                    boundary_smooth = iterations)
  })
  p <- prep$partition
  b <- p$boundaries
  face <- rep(seq_len(nrow(faces)), each = 3L)
  weights <- diag(3)[rep(1:3, nrow(faces)), , drop = FALSE]
  classified <- internal("classify_parcel_partition")(p, face, weights)
  stopifnot(identical(classified, as.integer(labels[t(faces)])))
  # Pair adjacency comes directly from the original mesh, independently of
  # path tracing. Verify every segment was emitted exactly once as well.
  e <- rbind(faces[, c(1, 2)], faces[, c(2, 3)], faces[, c(3, 1)])
  a <- labels[e[, 1]]; z <- labels[e[, 2]]
  pair_key <- function(a, z) paste(pmin(a, z), pmax(a, z), sep = "/")
  expected_pairs <- sort(unique(pair_key(a[a != z], z[a != z])))
  observed_pairs <- sort(unique(pair_key(b$label_pair[, 1], b$label_pair[, 2])))
  stopifnot(identical(expected_pairs, observed_pairs))
  emitted <- unlist(lapply(b$path_nodes, function(path) {
    pair_key(path[-length(path)], path[-1])
  }))
  stopifnot(!anyDuplicated(emitted),
            identical(sort(emitted), sort(pair_key(p$segments[, 1],
                                                    p$segments[, 2]))))
  render_times <- list()
  for (camera in c("lateral", "medial")) {
    render_time <- system.time({
      image <- render_surface_parcels(prep, colors = colors, camera = camera,
                                      width = 480L, height = 360L,
                                      antialias = 2L)
    })
    rgba <- array(as.integer(image$rgba) / 255, dim(image$rgba))
    png::writePNG(rgba, file.path(output, paste0(camera, "-", iterations, ".png")))
    render_times[[camera]] <- unname(render_time["elapsed"])
  }
  measurements[[as.character(iterations)]] <- list(
    iterations = iterations, prepare_seconds = unname(timing["elapsed"]),
    render_seconds = render_times,
    prepared_bytes = as.numeric(object.size(prep)),
    crossings = nrow(b$crossings), junctions = nrow(b$junctions),
    paths = length(b$boundary), closed_paths = sum(b$closed),
    projected_junctions = sum(b$junctions$projected),
    clamped_crossings = sum(b$crossings$clamped),
    vertex_checks = length(classified), adjacency_pairs = length(expected_pairs),
    segment_coverage = length(emitted), provenance = b$provenance
  )
}
receipt <- list(
  source = list(annotation_url = source_url, annotation_sha256 = annotation_hash,
                surface_path = surface_path, surface_sha256 = surface_hash,
                surface_nilearn_sha = "e5faab66dc2fe72d2cb50a8aaf2f44813aba4a51"),
  environment = list(R = R.version.string, platform = R.version$platform,
                     system = as.list(Sys.info()[c("sysname", "release", "machine")]),
                     source_sha256 = vapply(c("R/parcel_partition.R",
                                               "R/render_surface_parcels.R"),
                                            function(x) digest::digest(file = x,
                                              algo = "sha256"), character(1))),
  vertices = nrow(vertices), faces = nrow(faces), parcels = length(ids),
  medial_vertices = sum(labels == 0L),
  smallest_parcel_vertices = min(table(labels[labels != 0L])),
  measurements = measurements,
  limitation = paste("Single-process measurements, no speedup claim.",
                      "Peak RSS includes both preparation and four renders.",
                      "Visual inspection and peak RSS are recorded separately;",
                      "no cortex-studio parity has been run.")
)
jsonlite::write_json(receipt, file.path(output, "receipt.json"),
                     pretty = TRUE, auto_unbox = TRUE, digits = 16)
cat(jsonlite::toJSON(receipt, pretty = TRUE, auto_unbox = TRUE, digits = 16), "\n")
