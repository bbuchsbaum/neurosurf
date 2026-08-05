#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
data_dir <- if (length(args) >= 1L) args[[1]] else {
  "/tmp/neurosurf-rfe77/nilearn-data/fsaverage6"
}
output_dir <- if (length(args) >= 2L) args[[2]] else {
  "/tmp/neurosurf-rfe77/fsaverage6"
}

required <- c(
  "infl_left.gii.gz", "infl_right.gii.gz",
  "curv_left.gii.gz", "curv_right.gii.gz"
)
missing <- required[!file.exists(file.path(data_dir, required))]
if (length(missing)) {
  stop(
    "Missing fsaverage6 inputs: ", paste(missing, collapse = ", "),
    ". Fetch them with nilearn.datasets.fetch_surf_fsaverage('fsaverage6')."
  )
}

devtools::load_all(quiet = TRUE)

read_geometry <- function(path, hemi) {
  source <- gifti::readgii(path)
  SurfaceGeometry(source$data$pointset, source$data$triangle, hemi = hemi)
}

read_scalar <- function(path) {
  as.numeric(gifti::readgii(path)$data[[1]])
}

left <- read_geometry(file.path(data_dir, "infl_left.gii.gz"), "lh")
right <- read_geometry(file.path(data_dir, "infl_right.gii.gz"), "rh")
curvature <- list(
  left = read_scalar(file.path(data_dir, "curv_left.gii.gz")),
  right = read_scalar(file.path(data_dir, "curv_right.gii.gz"))
)

deterministic_map <- function(geometry, phase) {
  xyz <- coords(geometry)
  scale(cos(xyz[, 1] / 24 + phase) + sin(xyz[, 3] / 31 - phase))[, 1]
}

maps <- list(
  effect = list(
    left = deterministic_map(left, 0),
    right = deterministic_map(right, 0)
  ),
  reliability = list(
    left = deterministic_map(left, pi / 3),
    right = deterministic_map(right, pi / 3)
  )
)

scene <- surface_scene(
  left = left,
  right = right,
  layers = list(
    surface_layer(
      "effect", maps$effect, limits = c(-3.5, 3.5), units = "z",
      legend = list(title = "Task effect", units = "z", visible = TRUE),
      provenance = list(generator = "tools/rfe77_fsaverage6_scene.R")
    ),
    surface_layer(
      "reliability", maps$reliability, limits = c(-3.5, 3.5),
      units = "standardized score",
      legend = list(
        title = "Reliability", units = "standardized score", visible = TRUE
      )
    )
  ),
  curvature = curvature,
  metadata = list(template = "fsaverage6", vertices_per_hemisphere = 40962L),
  provenance = list(
    source = "nilearn.datasets.fetch_surf_fsaverage('fsaverage6')"
  ),
  fallback = paste(
    "Bilateral fsaverage6 cortical surfaces with task-effect and reliability",
    "maps. The interactive view is unavailable."
  ),
  alt_text = paste(
    "Coordinated lateral views of left and right fsaverage6 cortical surfaces",
    "with selectable task-effect and reliability maps."
  )
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_surface_scene(scene, output_dir, self_contained = FALSE,
                    title = "RFE 77 fsaverage6 performance fixture")
manifest <- surface_scene_manifest(scene, "directory", output_dir)
manifest_json <- jsonlite::toJSON(
  unclass(manifest), auto_unbox = TRUE, null = "null", na = "null", digits = NA
)

assets <- manifest$assets
bytes_by_role <- vapply(assets, `[[`, numeric(1), "byteLength")
roles <- vapply(assets, `[[`, character(1), "role")
metric <- list(
  template = "fsaverage6",
  vertices_per_hemisphere = 40962L,
  faces_per_hemisphere = 81920L,
  raw_geometry_bytes = sum(bytes_by_role[roles %in% c("vertices", "faces")]),
  curvature_bytes = sum(bytes_by_role[roles == "curvature"]),
  layer_bytes = as.list(stats::setNames(
    vapply(manifest$layers, function(layer) {
      sum(vapply(layer$values, function(ref) assets[[ref$values]]$byteLength,
                 numeric(1)))
    }, numeric(1)),
    names(manifest$layers)
  )),
  manifest_bytes = nchar(manifest_json, type = "bytes"),
  html_bytes = file.info(file.path(output_dir, "index.html"))$size,
  value_sha256 = lapply(manifest$layers, function(layer) {
    lapply(layer$values, function(ref) assets[[ref$values]]$sha256)
  }),
  generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)

jsonlite::write_json(
  metric, file.path(output_dir, "r-metrics.json"),
  auto_unbox = TRUE, pretty = TRUE, digits = NA
)
cat(file.path(output_dir, "index.html"), "\n")
