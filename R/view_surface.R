


# plotlySurface <- function(surfgeom, vals, col=brewer_pal(palette="RdBu")(9),
#                           alpha=1,
#                           add_normals=FALSE,
#                           threshold=NULL,
#                           irange=NULL,
#                           bgcol="#D3D3D3") {
#
#
#
#   curv <- curvature(surfgeom)
#
#   cmat <- rbind(vals[surfgeom@mesh$it[1,]],vals[surfgeom@mesh$it[2,]],vals[surfgeom@mesh$it[3,]] )
#   fvals <- colMeans(cmat)
#
#   fg_layer <- IntensityColorPlane(fvals, col,alpha=1)
#   fg_clrs <- map_colors(fg_layer, alpha=alpha, threshold=threshold, irange=irange)
#
#   #bg_layer <- IntensityColorPlane(curv, c("#D3D3D3FF", "#A9A9A9FF", "#A9A9A9FF"),alpha=1)
#   #bg_clrs <- map_colors(bg_layer, alpha=1)
#
#   #browser()
#
#
#   #combined <- blend_colors(bg_clrs, fg_clrs, alpha=alpha)
#   #face_cols <- as_hexcol(combined)
#   face_cols <- as_hexcol(fg_clrs)
#
#   cds <- coords(surfgeom)
#   p <- plot_ly(
#     x = cds[,1], y=cds[,2], z = cds[,3],
#     i = surfgeom@mesh$it[1,]-1, j =surfgeom@mesh$it[2,]-1, k = surfgeom@mesh$it[3,]-1,
#     facecolor=face_cols,
#     #vertexcolor=face_cols,
#     #intensity=vals,
#     type = "mesh3d",
#     flatshading=TRUE
#   )
#   p
#
#
# }
#
#
#

#' Convert Curvature Values to Binary Colors for Visualization
#'
#' @description
#' This function maps a vector of surface curvature values (e.g., mean curvature)
#' to a binary color scheme, typically used to distinguish gyri (outward folds)
#' from sulci (inward folds) on a brain surface visualization.
#'
#' @param vals A numeric vector containing curvature values for each vertex
#'   on the surface.
#' @param incol A character string specifying the hex color code to represent
#'   vertices with curvature values *greater than* the median curvature.
#'   Default is "#B3B3B3" (light gray).
#' @param outcol A character string specifying the hex color code to represent
#'   vertices with curvature values *less than or equal to* the median curvature.
#'   Default is "#404040" (dark gray).
#'
#' @return A character vector of the same length as `vals`, containing hex color
#'   codes based on the binary classification of curvature values relative to the median.
#'
#' @details
#' Surface curvature provides information about the local shape of the surface.
#' Mean curvature is often used, where positive values typically indicate outward
#' curvature (gyri) and negative values indicate inward curvature (sulci).
#' This function simplifies the curvature map into two colors based on whether
#' the value is above or below the median curvature, providing a quick visual
#' distinction between these features. Note the default coloring assigns `incol`
#' to values *above* the median and `outcol` to values *at or below* the median.
#' You might need to adjust `incol` and `outcol` depending on the specific
#' interpretation of curvature values in your data (e.g., if positive values
#' represent sulci).
#'
#' @examples
#' # Generate some example curvature values
#' set.seed(123)
#' curvature_values <- rnorm(100, mean = 0, sd = 0.1)
#'
#' # Get binary colors using default light/dark gray
#' gray_colors <- curv_cols(curvature_values)
#' table(gray_colors)
#'
#' # Use different colors (e.g., red for above median, blue for below)
#' red_blue_colors <- curv_cols(curvature_values, incol = "#FF0000", outcol = "#0000FF")
#' table(red_blue_colors)
#'
#' @seealso \code{\link{curvature}}, \code{\link{view_surface}}
#'
#' @export
curv_cols <- function(vals, incol="#B3B3B3", outcol="#404040") {
  ifelse(vals > stats::median(vals), incol, outcol)
}


#' @noRd
#' @keywords internal
surface_views <- list(
  left_lateral   = rbind(c(0,-1,0,0), c(0,0,1,0), c(-1,0,0,0), c(0,0,0,1)),
  left_medial    = rbind(c(0, 1,0,0), c(0,0,1,0), c( 1,0,0,0), c(0,0,0,1)),
  left_ventral   = rbind(c(-1,0,0,0), c(0,1,0,0), c( 0,0,-1,0), c(0,0,0,1)),
  left_dorsal    = rbind(c(1,0,0,0),  c(0,1,0,0), c( 0,0,1,0),  c(0,0,0,1)),
  left_anterior  = rbind(c(0,0,1,0),  c(0,1,0,0), c(-1,0,0,0),  c(0,0,0,1)),
  left_posterior = rbind(c(1,0,0,0),  c(0,0,1,0), c( 0,-1,0,0), c(0,0,0,1)),

  right_lateral   = rbind(c(0,1,0,0),  c(0,0,1,0), c( 1,0,0,0),  c(0,0,0,1)),
  right_medial    = rbind(c(0,-1,0,0), c(0,0,1,0), c(-1,0,0,0), c(0,0,0,1)),
  right_ventral   = rbind(c(-1,0,0,0), c(0,1,0,0), c( 0,0,-1,0), c(0,0,0,1)),
  right_dorsal    = rbind(c(1,0,0,0),  c(0,1,0,0), c( 0,0,1,0),  c(0,0,0,1)),
  right_anterior  = rbind(c(0,0,1,0),  c(0,1,0,0), c( 1,0,0,0),  c(0,0,0,1)),
  right_posterior = rbind(c(1,0,0,0),  c(0,0,1,0), c( 0,-1,0,0), c(0,0,0,1))
)


#' Display a 3D Brain Surface using RGL
#'
#' @description
#' Renders a 3D brain surface mesh using the `rgl` package. This function provides
#' flexible options for coloring the surface based on data values or predefined
#' colors, adjusting transparency, controlling lighting, setting viewpoints, and
#' overlaying spherical markers.
#'
#' @param surfgeom A \code{\linkS4class{SurfaceGeometry}} object representing the
#'   3D brain surface mesh to be displayed, or a \code{\linkS4class{SurfaceSet}}
#'   containing multiple variants.
#' @param vals An optional numeric vector containing data values for each vertex
#'   on the surface. If provided and `vert_clrs` is NULL, these values are mapped
#'   to colors using `cmap` and `irange`.
#' @param cmap A vector of colors (e.g., hex codes) defining the color map used
#'   when `vals` is provided and `vert_clrs` is NULL. Defaults to `rainbow(256)`.
#' @param vert_clrs An optional character vector of hex color codes for each vertex.
#'   If provided, these colors directly override any coloring derived from `vals` and `cmap`.
#'   The length should match the number of vertices in `surfgeom`.
#' @param bgcol A single hex color code or a vector of hex color codes used as the
#'   base color for the surface. If `vals` or `vert_clrs` are provided, this color
#'   is blended with the data/vertex colors. Defaults to "lightgray".
#' @param alpha A numeric value between 0 (fully transparent) and 1 (fully opaque)
#'   controlling the overall transparency of the surface. Defaults to 1.
#' @param add_normals Logical. If TRUE (default), surface normals are calculated
#'   and added to the mesh, which improves the appearance of lighting effects.
#' @param thresh An optional numeric vector of length 2, `c(lower, upper)`.
#'   Vertices with `vals` *outside* this range (i.e., `< lower` or `> upper`)
#'   are made fully transparent. This is applied *after* the general `alpha`.
#'   Defaults to NULL (no thresholding).
#' @param irange An optional numeric vector of length 2, `c(min, max)`. Specifies
#'   the range of `vals` to map onto the `cmap`. Values outside this range will be
#'   clamped to the min/max colors. Defaults to the full range of `vals`.
#' @param vals_vertices Optional integer vector of 1-based vertex ids corresponding
#'   to `vals` when `length(vals) < n_vertices`. Enables sparse data inputs.
#' @param vals_smoothing One of `"auto"` (default) or `"nearest"`. When using
#'   sparse data, `"auto"` diffuses values with neighbor averaging after nearest
#'   fill; `"nearest"` performs nearest-neighbour fill only.
#' @param vals_smoothing_steps Integer number of smoothing iterations applied
#'   when `vals_smoothing = "auto"`. Ignored otherwise.
#' @param specular The color of specular highlights on the surface, affecting its
#'   shininess. Can be a color name (e.g., "white") or hex code. Defaults to "black"
#'   for a matte look. Set to a brighter colour for a glossier appearance.
#' @param lit Logical. If \code{TRUE}, enables lighting effects on the surface. If
#'   \code{FALSE}, disables lighting for a flat appearance. If \code{NULL} (default),
#'   automatically sets to \code{TRUE} for interactive sessions and \code{FALSE} when
#'   knitting (when \code{rgl.useNULL} is \code{TRUE}).
#' @param viewpoint A character string specifying a predefined view (e.g.,
#'   "lateral", "medial", "ventral", "dorsal", "anterior", "posterior"). The
#'   actual view depends on the hemisphere (`surfgeom@hemi`, e.g.,
#'   "left_lateral"). Alternatively, a 4x4 transformation matrix defining a
#'   custom view. Defaults to "lateral".
#' @param new_window Logical. If TRUE (default), opens a new `rgl` window for the plot.
#'   If FALSE, attempts to plot in the currently active `rgl` window (useful for
#'   updates or within Shiny apps).
#' @param offset A numeric vector of length 3 specifying a translation offset
#'   `c(x, y, z)` applied to the surface coordinates before rendering. Defaults to `c(0, 0, 0)`.
#' @param zoom A numeric value controlling the camera zoom level. Defaults to 1 (no zoom).
#'   Values > 1 zoom in, < 1 zoom out.
#' @param spheres An optional data frame to draw spheres at specific locations on
#'   or near the surface. Must contain columns `x`, `y`, `z` (coordinates), and
#'   `radius`. Can optionally include a `color` column (hex codes or color names)
#'   for individual sphere colors (defaults to black). Alternatively, supply a
#'   `vertex` column (1-based vertex ids) and set \code{spheres_as_vertices = TRUE}
#'   to position foci by vertex.
#' @param spheres_map_surface Optional \code{SurfaceGeometry}, \code{SurfaceSet},
#'   or file path used to map sphere coordinates to the nearest vertex on that
#'   surface before snapping to \code{surfgeom}. Assumes both surfaces share the
#'   same vertex ordering (e.g., white -> inflated).
#' @param spheres_map_label Optional surface label to use when
#'   \code{spheres_map_surface} is a \code{SurfaceSet}.
#' @param spheres_as_vertices Logical; if \code{TRUE}, interpret the `vertex`
#'   column of \code{spheres} as 1-based vertex ids on \code{surfgeom} rather
#'   than raw coordinates.
#' @param vectors Optional matrix (n x 3) of XYZ vectors to draw as line glyphs.
#' @param vector_vertices Optional vertex ids matching rows of \code{vectors}
#'   when they are defined on a subset of vertices.
#' @param vector_scale Optional numeric scale factor for vectors. If \code{NULL},
#'   a heuristic scale based on mesh extent and vector magnitudes is used.
#' @param vector_color Colour for the vectors (single value or vector).
#' @param vector_alpha Opacity for the vectors (0–1).
#' @param vector_lwd Numeric line width for vector glyphs.
#' @param label Optional surface label to select when `surfgeom` is a
#'   \code{SurfaceSet}. Defaults to the set's `default_label`.
#' @param ... Additional arguments passed directly to `rgl::shade3d` for fine-grained
#'   control over rendering (e.g., `lit`, `smooth`).
#'
#' @return Invisibly returns the object ID(s) of the shape(s) added to the RGL scene
#'   by `rgl::shade3d`. This can be useful for modifying the scene later.
#'
#' @details
#' **Coloring:** Surface vertex colors are determined by the following priority:
#'   1. `vert_clrs`: If provided, these specific hex colors are used.
#'   2. `vals` & `cmap`: If `vals` is provided and `vert_clrs` is NULL, `vals` are mapped to `cmap` based on `irange`.
#'   3. `bgcol`: If neither `vert_clrs` nor `vals` are used for coloring, `bgcol` is applied uniformly.
#'   If `bgcol` is specified alongside `vert_clrs` or `vals`, the colors are blended based on the `alpha` parameter.
#'
#' **Transparency:** Overall transparency is set by `alpha`. Additional threshold-based
#'   transparency can be applied using `thresh` when `vals` are provided. Vertices
#'   with values outside the `thresh` range become fully transparent.
#'
#' **Lighting:** `add_normals=TRUE` is recommended for realistic lighting. The `specular`
#'   parameter controls the shininess.
#'
#' **Viewpoint:** Predefined viewpoints (`"lateral"`, `"medial"`, etc.) are automatically
#'   adjusted based on the hemisphere specified in `surfgeom@hemi` (e.g., "lh" results
#'   in "left_lateral"). If `hemi` is unknown, the current `rgl` view is used unless
#'   a custom 4x4 matrix is provided.
#'
#' **Performance:** Rendering very large surfaces or surfaces with complex coloring/transparency
#'   can be computationally intensive.
#'
#' @importFrom gplots col2hex
#' @importFrom rgl open3d clear3d shade3d spheres3d view3d par3d addNormals rgl.useNULL
#' @importFrom colorplane IntensityColorPlane HexColorPlane map_colors blend_colors as_hexcol
#' @importFrom stats median
#' @importFrom grDevices rainbow
#'
#' @examples
#' \donttest{
#' # Load a surface geometry
#' surf_geom <- example_surface_geometry()
#'
#' # Simple display with default background color
#' view_surface(surf_geom, viewpoint = "lateral")
#'
#' # Display with curvature coloring (assuming you have curvature data)
#' curv_vals <- curvature(surf_geom)
#' view_surface(surf_geom, vals = curv_vals, cmap = gray.colors(256), viewpoint = "medial")
#'
#' # Display with spheres marking specific coordinates
#' sphere_coords <- data.frame(
#'   x = c(10, -15, 5),
#'   y = c(20, 0, -10),
#'   z = c(-5, 25, 15),
#'   radius = c(3, 4, 2.5),
#'   color = c("yellow", "cyan", "magenta")
#' )
#' view_surface(surf_geom, viewpoint = "lateral", spheres = sphere_coords)
#' }
#'
#' @seealso \code{\link[rgl]{shade3d}}, \code{\link[rgl]{spheres3d}}, \code{\link[rgl]{view3d}}, \code{\link{SurfaceGeometry}}
#' @export
view_surface <- function(surfgeom, vals=NA,
                         cmap=grDevices::rainbow(256, alpha = 1),
                         vert_clrs=NULL,
                         bgcol = "lightgray",
                         alpha=1,
                         add_normals=TRUE,
                         thresh=NULL,
                         irange=NULL,
                         specular="black",  # Matte by default
                         lit=NULL,           # auto: TRUE interactive, FALSE when knitting (useNULL)
                         viewpoint=c("lateral","medial", "ventral", "dorsal", "anterior", "posterior"),
                         new_window=TRUE,  # New argument to control RGL window
                         offset=c(0,0,0),
                         zoom=1,
                         spheres=NULL,  # New argument for spheres
                         spheres_map_surface = NULL,
                         spheres_map_label = NULL,
                         spheres_as_vertices = FALSE,
                         vectors = NULL,
                         vector_vertices = NULL,
                         vector_scale = NULL,
                         vector_color = "red",
                         vector_alpha = 0.8,
                         vector_lwd = 1.5,
                         vals_vertices = NULL,
                         vals_smoothing = c("auto", "nearest"),
                         vals_smoothing_steps = 20,
                         label=NULL,    # optional SurfaceSet label
                         ...) {

  # If a SurfaceSet was supplied, pick the requested surface (or default)
  if (is(surfgeom, "SurfaceSet")) {
    surfgeom <- get_surface(surfgeom, label)
  }

  # Open a new rgl window only if not in Shiny
  if (new_window && !rgl::rgl.useNULL()) {
    rgl::open3d()
  } else if (new_window) {
    # When knitting (rgl.useNULL=TRUE), make sure we start from a clean scene
    rgl::clear3d(type = "all")
  }



  mesh <- surfgeom@mesh
  if (add_normals) {
    mesh <- rgl::addNormals(mesh)
  }

  if (!is.null(offset) && length(offset) == 3 && any(offset != 0)) {
    mesh$vb[1:3, ] <- sweep(mesh$vb[1:3, , drop = FALSE], 1, offset, "+")
  }

  viewpoint <- match.arg(viewpoint)

  umat <- if (is.matrix(viewpoint)) {
    stopifnot(nrow(viewpoint) == 4 && ncol(viewpoint) == 4)
    viewpoint
  } else if (surfgeom@hemi == "lh") {
    viewpoint <- paste0("left_", viewpoint)
    surface_views[[viewpoint]]
  } else if (surfgeom@hemi == "rh") {
    viewpoint <- paste0("right_", viewpoint)
    surface_views[[viewpoint]]
  } else {
    warning("unknown hemisphere, default using viewpoint")
    rgl::par3d()$userMatrix
  }

  n_vert <- nrow(coords(surfgeom))

  vals_smoothing <- match.arg(vals_smoothing)

  vals_use <- vals
  if (length(vals_use) == 1L && is.na(vals_use)) {
    vals_use <- rep(NA_real_, n_vert)
  }

  if (is.numeric(vals_use) && length(vals_use) > 1L &&
      length(vals_use) != n_vert) {
    if (is.null(vals_vertices)) {
      stop("vals length (", length(vals_use),
           ") does not match number of vertices (", n_vert,
           "); supply vals_vertices to indicate which vertices the data map to.")
    }
    vals_use <- .ns_fill_sparse_data(
      surf = surfgeom,
      values = vals_use,
      vertices = vals_vertices,
      smoothing = vals_smoothing,
      smoothing_steps = vals_smoothing_steps
    )
  }

  if (is.null(irange) || any(!is.finite(irange))) {
    irange <- range(vals_use, na.rm = TRUE)
    if (!all(is.finite(irange))) {
      irange <- c(0, 1)
    }
  }

  if (length(bgcol) == 1 && is.na(bgcol)) {
    bg_layer <- NULL
  } else {
    if (is.character(bgcol)) {
      bgcol <- gplots::col2hex(bgcol)
    }

    if (length(bgcol) == 1) {
      bg_layer <- colorplane::HexColorPlane(rep(bgcol, ncol(mesh$vb)))
    } else {
      bg_layer <- colorplane::HexColorPlane(bgcol)
    }
  }

  if (is.numeric(vals_use) && any(!is.na(vals_use)) && is.null(vert_clrs)) {
    fg_layer <- colorplane::IntensityColorPlane(vals_use, cmap, alpha=1)
    fg_clrs <- colorplane::map_colors(fg_layer, alpha=alpha, threshold=thresh, irange=irange)
    combined <- colorplane::blend_colors(bg_layer, fg_clrs, alpha=alpha)
    vertex_cols <- colorplane::as_hexcol(combined)
  } else if (!is.null(vert_clrs)) {
    if (is.null(bg_layer)) {
      vertex_cols <- vert_clrs
    } else {
      fg_layer <- colorplane::HexColorPlane(vert_clrs)
      combined <- colorplane::blend_colors(bg_layer, fg_layer, alpha=alpha)
      vertex_cols <- colorplane::as_hexcol(combined)
    }
  } else {
    vertex_cols <- if (!is.null(bg_layer)) {
      colorplane::as_hexcol(bg_layer)
    } else {
      rep("#FFFFFF", ncol(mesh$vb))
    }
  }

  rgl::par3d(mouseMode="trackball")
  lit_arg <- lit
  # Default lighting: interactive sessions lit; knitting (rgl.useNULL) unlit unless user explicitly requests
  if (is.null(lit)) {
    lit <- !rgl::rgl.useNULL()
  }

  # If users asked for a shiny surface explicitly keep it, otherwise soften
  if (identical(specular, "white") && is.null(lit_arg)) {
    specular <- "#333333"
  }

  # For knit/NULL scenes with lighting turned on, pad lights to match shader expectations (defaults to 8)
  if (rgl::rgl.useNULL() && isTRUE(lit)) {
    rgl::clear3d(type = "lights")
    # two visible lights
    rgl::light3d(theta = 45,   phi = 45, diffuse = "#E0E0E0",  specular = specular, ambient = "#d0d0d0")
    rgl::light3d(theta = -45, phi = -20, diffuse = "#888888", specular = "black", ambient = "#c0c0c0")
    # pad with zero-intensity lights so doLighting gets full-length uniform arrays
    for (i in seq_len(6)) {
      rgl::light3d(theta = 0, phi = 0,
                   diffuse = "#000000", specular = "#000000", ambient = "#000000")
    }
  }

  shade_args <- list(col = vertex_cols,
                     specular = specular,
                     polygon_offset = 1,
                     meshColor = "vertices",
                     lit = lit,
                     ...)

  ret <- do.call(rgl::shade3d, c(list(mesh), shade_args))
  rgl::view3d(fov=0, userMatrix=umat, zoom=zoom)

  # Add better lights for interactive scenes (studio setup)
  if (isTRUE(lit) && !rgl::rgl.useNULL()) {
    rgl::clear3d(type = "lights")
    rgl::light3d(theta = 45,  phi = 45, diffuse = "#E0E0E0", specular = specular)
    rgl::light3d(theta = -45, phi = 0,  diffuse = "#B0B0B0", specular = "black")
    rgl::light3d(theta = 0,   phi = -45, diffuse = "#606060", specular = "black")
  }

  if (!is.null(vectors)) {
    .ns_draw_vector_overlay(
      surf = surfgeom,
      vectors = vectors,
      vertices = vector_vertices,
      scale = vector_scale,
      color = vector_color,
      alpha = vector_alpha,
      lwd = vector_lwd
    )
  }

  # Add spheres if specified
  if (!is.null(spheres)) {
    spheres_df <- as.data.frame(spheres)
    use_vertices <- isTRUE(spheres_as_vertices) ||
      (!all(c("x", "y", "z") %in% names(spheres_df)) &&
         "vertex" %in% names(spheres_df))

    if (use_vertices) {
      if (!"vertex" %in% names(spheres_df)) {
        stop("spheres must include a 'vertex' column when ",
             "spheres_as_vertices = TRUE.")
      }
    } else {
      if (!all(c("x", "y", "z") %in% names(spheres_df))) {
        stop("spheres data frame must contain columns 'x', 'y', 'z' when ",
             "spheres_as_vertices = FALSE.")
      }
    }
    if (!"radius" %in% names(spheres_df)) {
      stop("spheres data frame must contain a 'radius' column.")
    }

    mapped <- .ns_map_foci_to_surface(
      spheres = spheres_df,
      target_surf = surfgeom,
      map_surface = spheres_map_surface,
      coords_as_vertices = use_vertices,
      map_label = spheres_map_label
    )

    foci_coords <- mapped$coords

    for (i in seq_len(nrow(spheres_df))) {
      # Use provided color or default to black
      sphere_color <- if ("color" %in% names(spheres_df)) spheres_df$color[i] else "black"
      rgl::spheres3d(
        x = foci_coords[i, 1],
        y = foci_coords[i, 2],
        z = foci_coords[i, 3],
        radius = spheres_df$radius[i],
        color = sphere_color
      )
    }
  }

  ret
}

#' Map foci coordinates to a target surface
#'
#' @keywords internal
#' @noRd
.ns_map_foci_to_surface <- function(spheres,
                                    target_surf,
                                    map_surface = NULL,
                                    coords_as_vertices = FALSE,
                                    map_label = NULL) {
  stopifnot(inherits(target_surf, "SurfaceGeometry"))

  if (coords_as_vertices) {
    v_idx <- as.integer(spheres$vertex)
    n_vert <- nrow(coords(target_surf))
    if (any(v_idx < 1L | v_idx > n_vert)) {
      stop("vertex ids must be within [1, ", n_vert, "].")
    }
    mapped_coords <- coords(target_surf)[v_idx, , drop = FALSE]
    return(list(coords = mapped_coords, vertex = v_idx))
  }

  if (is.null(map_surface)) {
    return(list(coords = as.matrix(spheres[, c("x", "y", "z")]), vertex = NULL))
  }

  map_geom <- .ns_resolve_surface_geom(map_surface, map_label)
  map_coords <- coords(map_geom)
  target_coords <- coords(target_surf)

  if (nrow(map_coords) != nrow(target_coords)) {
    warning("map_surface vertex count (", nrow(map_coords),
            ") differs from target (", nrow(target_coords),
            "); mapped vertices may be misaligned.")
  }

  knn <- FNN::get.knnx(map_coords,
                       query = as.matrix(spheres[, c("x", "y", "z")]),
                       k = 1L)
  v_idx <- as.integer(knn$nn.index[, 1])
  mapped_coords <- target_coords[v_idx, , drop = FALSE]
  list(coords = mapped_coords, vertex = v_idx)
}

#' Normalize surface input for mapping
#'
#' @keywords internal
#' @noRd
.ns_resolve_surface_geom <- function(x, label = NULL) {
  if (inherits(x, "SurfaceGeometry")) {
    return(x)
  }
  if (is(x, "SurfaceSet")) {
    return(get_surface(x, label))
  }
  if (is.character(x) && length(x) == 1L) {
    return(read_surf_geometry(x))
  }
  stop("Unsupported surface specification for spheres_map_surface. ",
       "Provide a SurfaceGeometry, SurfaceSet, or file path.")
}

#' plot a surface
#'
#' @rdname plot-methods
#' @param x the surface to plot
#' @param ... extra args to send to \code{view_surface}
#' @export
#' @importFrom graphics plot
#' @importFrom grDevices gray
#' @inheritParams view_surface
setMethod("plot", signature=signature(x="SurfaceGeometry", y="missing"),
          def=function(x,vals=NA, cmap=grDevices::gray(seq(0,1,length.out=255)),
                       vert_clrs=NULL,
                       irange=range(vals),
                       thresh=c(0,0),
                       alpha=1,
                       specular="black",
                       bgcol="lightgray", ...) {

            view_surface(x,vals,cmap=cmap,vert_clrs=vert_clrs, irange=irange,thresh=thresh,alpha=alpha,bgcol=bgcol,specular=specular,...)

          })

# S3 fallback so namespaced calls like graphics::plot() dispatch to neurosurf rendering
#' Plot method for SurfaceGeometry objects
#'
#' @param x A \code{\linkS4class{SurfaceGeometry}} object.
#' @param y Ignored (for S3 method compatibility).
#' @param ... Additional arguments passed to \code{\link{view_surface}}.
#'
#' @return Invisibly returns the object ID(s) from the RGL scene.
#' @method plot SurfaceGeometry
#' @export
plot.SurfaceGeometry <- function(x, y, ...) {
  view_surface(x, ...)
}

#' Plot method for SurfaceSet objects
#'
#' @param x A \code{\linkS4class{SurfaceSet}}.
#' @param y Ignored (for S3 compatibility).
#' @param label Optional surface label to display; defaults to the set's default.
#' @param ... Additional arguments passed to \code{\link{view_surface}}.
#' @return Invisibly returns the object ID(s) from the RGL scene.
#' @method plot SurfaceSet
#' @export
plot.SurfaceSet <- function(x, y, label = NULL, ...) {
  view_surface(x, label = label, ...)
}


#' @export
#' @rdname plot-methods
setMethod("plot", signature=signature(x="NeuroSurface", y="missing"),
          def=function(x,cmap=grDevices::gray(seq(0,1,length.out=255)),
                       vert_clrs=NULL,
                       irange=range(x@data, na.rm=TRUE),
                       thresh=c(0,0),
                       alpha=1,
                       specular="black",
                       bgcol="lightgray", ...) {

            ind <- x@indices
            vals <- rep(NA, length(nodes(x)))
            vals[ind] <- x@data

            view_surface(x@geometry,vals,cmap=cmap,vert_clrs=vert_clrs, irange=irange,thresh=thresh,alpha=alpha,bgcol=bgcol,specular=specular,...)

          })

#' @export
#' @importFrom graphics plot
#' @rdname plot-methods
setMethod("plot", signature=signature(x="LabeledNeuroSurface", y="missing"),
          def=function(x,cmap=x@cols,
                       vert_clrs=NULL,
                       irange=range(x@data, na.rm=TRUE),
                       thresh=c(0,0),
                       alpha=1,
                       specular="black",
                       bgcol="lightgray", ...) {

            ind <- x@indices
            vals <- rep(NA, length(nodes(x)))
            vals[ind] <- x@data

            view_surface(x@geometry,vals,cmap=cmap,vert_clrs=vert_clrs, irange=irange,thresh=thresh,alpha=alpha,bgcol=bgcol,specular=specular,...)

          })


#' @export
#' @rdname plot-methods
setMethod("plot", signature=signature(x="ColorMappedNeuroSurface", y="missing"),
          def=function(x,
                       vert_clrs=NULL,
                       alpha=1,
                       specular="black",
                       bgcol="lightgray", ...) {

            ind <- x@indices
            vals <- rep(NA, length(nodes(x)))
            vals[ind] <- x@data

            view_surface(x@geometry,
                         vals,
                         cmap=x@cmap,
                         vert_clrs=vert_clrs,
                         irange=x@irange,
                         thresh=x@thresh,
                         alpha=alpha,
                         bgcol=bgcol,
                         specular=specular,
                         ...)

          })


#' @export
#' @rdname plot-methods
setMethod("plot", signature=signature(x="VertexColoredNeuroSurface", y="missing"),
          def=function(x,
                       alpha=1,
                       specular="black",
                       bgcol="lightgray", ...) {

            ind <- x@indices
            colors <- rep(NA, length(nodes(x@geometry)))
            colors[ind] <- x@colors

            view_surface(x@geometry,
                         vals=NULL,
                         cmap=NULL,
                         vert_clrs=colors,
                         irange=NULL,
                         thresh=NULL,
                         alpha=alpha,
                         bgcol=bgcol,
                         specular=specular,
                         ...)

          })



#' viewShiny <- function(surfgeom, vals=1:length(nodes(surfgeom)), col=rainbow(255, alpha = 1)) {
#'   options(rgl.useNULL = TRUE)
#'
#'   app <- shinyApp(
#'     ui = fluidPage(
#'       rgl::registerSceneChange(),
#'       sidebarLayout(
#'         sidebarPanel(
#'           sliderInput(inputId="threshold",
#'                       "Intensity Threshold:",
#'                       min = min(vals),
#'                       max = max(vals),
#'                       value = c(.45*max(vals), .55*max(vals))),
#'           sliderInput(inputId="range",
#'                       "Intensity Range:",
#'                        min = min(vals),
#'                        max = max(vals),
#'                        value = c(.02*min(vals), .98*max(vals)))
#'         ),
#'         mainPanel(h3("Surface View"),
#'                   rgl::rglwidgetOutput("surface_widget", width = "100%", height = 512))
#'       )
#'     ),
#'
#'     server = function(input, output, session) {
#'       options(rgl.useNULL=TRUE)
#'       rgl::open3d()
#'       dev <- rgl::rgl.cur()
#'       save <- options(rgl.inShiny = TRUE)
#'       on.exit(options(save))
#'
#'       session$onSessionEnded(function() {
#'         rgl::rgl.set(dev)
#'         rgl::rgl.close()
#'       })
#'
#'       #surf <- viewSurface(surfgeom, vals, col, add_normals=TRUE)
#'
#'       #start_surf <- reactiveValues(my_mesh=NULL)
#'
#'       #viewSurface(surfgeom, vals,  col)
#'       #scene1 <- scene3d()
#'
#'
#'        # observeEvent(input$threshold, {
#'        #   cat("got event \n")
#'        #   cat("thresh = ", input$threshold)
#'        #   rgl.set(dev)
#'        #   newsurf <- viewSurface(surfgeom, vals, col, add_normals=TRUE, threshold=input$threshold, irange=input$range)
#'        #   cat("newsurf id:", as.integer(newsurf))
#'        #   cat("old surf id: ", as.integer(start_surf$my_mesh))
#'        #   session$sendCustomMessage("sceneChange",
#'        #                             sceneChange("surface_widget", replace=start_surf$my_mesh, skipRedraw=TRUE))
#'        #
#'        #   start_surf$my_mesh <- newsurf
#'        #   session$onFlushed(function()
#'        #     session$sendCustomMessage("sceneChange",
#'        #                               sceneChange("surface_widget", skipRedraw = FALSE)))
#'        # })
#'
#'
#'       output$surface_widget <- rgl::renderRglwidget({
#'         rgl.set(dev)
#'
#'         cat("new thresh ", input$threshold)
#'         #viewSurface(surfgeom, vals, col, add_normals=TRUE, threshold=input$threshold, irange=input$range)
#'         #start_surf()
#'         viewSurface(surfgeom, vals, col, threshold=input$threshold, irange=input$range)
#'         scene1 <- scene3d()
#'         rglwidget(scene1, width=500, height=500)
#'         #rglwidget(scene1)
#'       })
#'     }
#'   )
#'
#'   runApp(app, launch.browser=TRUE)
#' }
#'
#'
