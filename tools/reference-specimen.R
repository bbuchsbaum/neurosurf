#!/usr/bin/env Rscript
# Synthetic, disconnected quads exercise none/reference/effect/overlap regions.
# RGL_USE_NULL=TRUE Rscript tools/reference-specimen.R /tmp/neurosurf-reference
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1]] else "/tmp/neurosurf-reference"
devtools::load_all(quiet = TRUE)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
vertices <- NULL
triangles <- NULL
values <- membership <- numeric()
for (j in 0:1) for (i in 0:1) {
  n <- if (is.null(vertices)) 0L else nrow(vertices)
  vertices <- rbind(vertices, cbind(0, c(0, 1, 1, 0) + i * 1.2,
                                   c(0, 0, 1, 1) + j * 1.2))
  triangles <- rbind(triangles, c(0L, 1L, 2L) + n, c(0L, 2L, 3L) + n)
  values <- c(values, rep(i * 2, 4))
  membership <- c(membership, if (j == 1 && i == 0) c(0, 1, 1, 0) else rep(j, 4))
}
vertices <- vertices * 20
left <- SurfaceGeometry(vertices, triangles, hemi = "lh")
right <- SurfaceGeometry(vertices, triangles, hemi = "rh")
provenance <- list(source = "synthetic four-region fixture", mesh = "four-quads",
                   projection = "none; authored binary vertex membership")
scene <- surface_scene(
  left = left, right = right,
  layers = list(
    surface_layer("psl", list(left = membership, right = membership),
                   role = "reference", color = "#0000FF", opacity = 0.4,
                   legend = list(title = "Synthetic PSL reference"),
                   provenance = provenance),
    surface_layer("effect", list(left = values, right = values),
                   colormap = c("#FF0000", "#FF0000"), limits = c(0, 2),
                   threshold = c(-1, 1), units = "effect units"),
    surface_layer("graded", list(left = values, right = values),
                   colormap = c("#FF0000", "#FF0000"), limits = c(0, 2),
                   threshold = c(-1, 1), opacity = 0.5, units = "effect units")
  ), fallback = "Synthetic reference below quantitative maps.",
  alt_text = "Four regions: empty, effect only, reference only and overlapping."
)
for (inline in c(TRUE, FALSE)) {
  dir <- file.path(out, if (inline) "inline" else "bundled")
  write_surface_scene(scene, dir, self_contained = inline)
}
jsonlite::write_json(unclass(surface_scene_manifest(scene)),
                     file.path(out, "manifest.json"), auto_unbox = TRUE,
                     null = "null", na = "null", digits = NA, pretty = TRUE)
static <- render_surface_rgba(
  left, values, reference = list(id = "psl", label = "Synthetic PSL reference",
                                 mask = membership, color = "#0000FF",
                                 opacity = 0.4, provenance = provenance),
  camera = "lateral", width = 500, height = 500, threshold = 1,
  limits = c(0, 2), palette = c("#FF0000", "#FF0000"),
  lighting = FALSE, subdivide = FALSE, outer_contour = FALSE
)
write_surface_rgba(static, file.path(out, "static.png"))
# The categorical key is independent of the quantitative color scale.
svg <- paste0('<svg xmlns="http://www.w3.org/2000/svg" width="560" height="560">',
              '<rect width="100%" height="100%" fill="white"/>',
              '<image href="static.png" x="30" y="0" width="500" height="500"/>',
              '<rect x="40" y="520" width="18" height="18" fill="blue" fill-opacity=".4"/>',
              '<text x="68" y="535" font-family="sans-serif" font-size="16">Synthetic PSL reference (opacity 0.4)</text></svg>')
writeLines(svg, file.path(out, "static-with-key.svg"))
cat(out, "\n")
