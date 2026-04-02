#' High-level surface plotting helpers
#'
#' These functions provide an idiomatic R interface for building complex
#' multi-view, multi-hemisphere surface plots on top of neurosurf's core
#' classes and plotting utilities.
#'
#' @name surface_plot_helpers
#' @return NULL (documentation block only)
#' @keywords internal
NULL


#' Show a surface plot in one step
#'
#' This is a convenience wrapper around \code{\link{surface_plot}},
#' \code{\link{add_surface_layer}}, and \code{\link{plot.neurosurf_plot}}.
#' It is intended for quick inspection and simple publication-style plots.
#'
#' @param lh,rh Either \code{SurfaceGeometry} objects or file paths that can be
#'   read by \code{\link{read_surf_geometry}}. At least one must be provided.
#' @param data Optional numeric vector or list of vectors containing
#'   vertex-wise data to plot. If a single numeric vector is supplied, it is
#'   split across hemispheres in left-to-right order based on the vertex
#'   counts of the surfaces. If \code{NULL}, a plain surface is shown.
#' @param views Character vector of named views to display for each hemisphere.
#'   See \code{\link{surface_plot}} for valid values.
#' @param layout One of \code{"grid"}, \code{"row"}, or \code{"column"}
#'   controlling how views and hemispheres are arranged.
#' @param cmap Character string naming a colour map for the data layer.
#' @param color_range Optional numeric vector of length 2 giving the minimum
#'   and maximum values for the colour scale.
#' @param show_colorbar Logical; if \code{TRUE}, draw a colour bar for the data
#'   layer.
#' @param outline Logical; if \code{TRUE}, the supplied \code{data} are treated
#'   as ROI labels and boundaries are drawn instead of a filled map.
#' @param ... Additional arguments passed through to \code{\link{add_surface_layer}}
#'   (for example \code{alpha}, \code{outline_col}, \code{outline_lwd}).
#'
#' @return Invisibly returns the underlying \code{"neurosurf_plot"} object.
#'   The plot is drawn as a side-effect.
#'
#' @examples
#' \donttest{
#' geom <- example_surface_geometry()
#' show_surface_plot(geom, data = rnorm(nrow(coords(geom))))
#' }
#'
#' @export
show_surface_plot <- function(lh,
                              rh = NULL,
                              data = NULL,
                              views = c("lateral", "medial"),
                              layout = c("grid", "row", "column"),
                              cmap = "viridis",
                              color_range = NULL,
                              show_colorbar = TRUE,
                              outline = FALSE,
                              ...) {
  p <- surface_plot(lh = lh,
                    rh = rh,
                    views = views,
                    layout = layout)

  if (!is.null(data)) {
    p <- add_surface_layer(
      p,
      data = data,
      cmap = cmap,
      color_range = color_range,
      show_colorbar = show_colorbar && !outline,
      as_outline = outline,
      ...
    )
  }

  plot(p)
  invisible(p)
}


#' Add an atlas outline layer to a surface plot
#'
#' This helper adds an outline-only layer to an existing \code{"neurosurf_plot"}
#' object using ROI labels. It configures sensible defaults for boundary
#' aesthetics (including a light halo and a small depth offset) so that atlas
#' outlines remain legible over filled statistical maps.
#'
#' @param x A \code{"neurosurf_plot"} object created by
#'   \code{\link{surface_plot}}.
#' @param labels Numeric vector or list of vectors containing ROI labels for
#'   each vertex. If a single vector is supplied, it is split across
#'   hemispheres based on vertex counts.
#' @param rois Optional numeric vector of ROI ids to outline. If \code{NULL},
#'   all ROIs present in \code{labels} are outlined.
#' @param label Optional character label for this outline layer.
#' @param outline_col Colour to use for ROI boundaries. Defaults to \code{"black"}.
#' @param outline_lwd Numeric line width for boundaries. Defaults to \code{1.5}.
#' @param outline_offset Numeric depth offset along surface normals to avoid
#'   z-fighting. Defaults to \code{0.5}.
#' @param outline_halo Logical; if \code{TRUE}, draws a thicker halo under the
#'   main line for better visibility. Defaults to \code{TRUE}.
#' @param outline_halo_col Colour for the halo. Defaults to \code{"white"}.
#' @param outline_halo_lwd Numeric line width for the halo. If \code{NULL},
#'   slightly larger than \code{outline_lwd}.
#' @param outline_lty Line type for boundaries: \code{"solid"} or \code{"dashed"}.
#' @param ... Additional arguments passed through to
#'   \code{\link{add_surface_layer}}, allowing fine control over line colour,
#'   width, offset, and halo appearance.
#'
#' @return A modified \code{"neurosurf_plot"} object.
#'
#' @examples
#' \donttest{
#' fs <- load_fsaverage_std8("inflated")
#' p  <- surface_plot(fs$lh, fs$rh)
#' # roi_labels <- ... # per-vertex ROI ids
#' # p <- add_atlas_outline(p, roi_labels)
#' }
#'
#' @export
add_atlas_outline <- function(x,
                              labels,
                              rois = NULL,
                              label = "atlas",
                              outline_col = "black",
                              outline_lwd = 1.5,
                              outline_offset = 0.5,
                              outline_halo = TRUE,
                              outline_halo_col = "white",
                              outline_halo_lwd = NULL,
                              outline_lty = c("solid", "dashed"),
                              ...) {
  stopifnot(inherits(x, "neurosurf_plot"))

  outline_lty <- match.arg(outline_lty)

  # Build args so any user-supplied values in ... override defaults cleanly.
  args <- list(
    data           = labels,
    as_outline     = TRUE,
    show_colorbar  = FALSE,
    outline_col    = outline_col,
    outline_lwd    = outline_lwd,
    outline_offset = outline_offset,
    outline_halo   = outline_halo,
    outline_halo_col = outline_halo_col,
    outline_halo_lwd = outline_halo_lwd,
    outline_rois   = rois,
    outline_lty    = outline_lty,
    label          = label
  )

  dot_args <- list(...)
  args <- utils::modifyList(args, dot_args, keep.null = TRUE)

  # Ensure no duplicated names (can happen if users supply arguments that
  # overlap with defaults); keep the last occurrence (user-specified).
  if (length(args)) {
    dup <- duplicated(names(args), fromLast = TRUE)
    if (any(dup)) {
      args <- args[!dup]
    }
  }

  do.call(add_surface_layer, c(list(x = x), args))
}


#' Create a surface plot specification
#'
#' @param lh,rh Either \code{SurfaceGeometry} objects or file paths that can be
#'   read by \code{\link{read_surf_geometry}}. At least one must be provided.
#' @param views Character vector of named views to display for each hemisphere.
#'   Valid values include \code{"lateral"}, \code{"medial"},
#'   \code{"ventral"}, \code{"dorsal"}, \code{"anterior"}, and
#'   \code{"posterior"}. Defaults to \code{c("lateral","medial")}.
#' @param layout One of \code{"grid"}, \code{"row"}, or \code{"column"}
#'   controlling how views and hemispheres are arranged.
#' @param mirror_views Logical; if \code{TRUE}, reverse the order of the right
#'   hemisphere views for \code{"row"} and \code{"column"} layouts so that they
#'   mirror the left hemisphere.
#' @param flip Logical; if \code{TRUE} and both hemispheres are present, flip
#'   the left/right ordering in the layout (useful for anterior views).
#' @param zoom Numeric zoom factor passed through to the underlying
#'   \code{\link{view_surface}} calls.
#' @param background Background colour for the rgl scene.
#' @param brightness Baseline brightness for a plain surface when no layers are
#'   added. Value in \eqn{[0,1]}.
#'
#' @return An object of class \code{"neurosurf_plot"} that can be further
#'   modified with \code{add_surface_layer()} and rendered with
#'   \code{render_surface_plot()} or \code{draw_surface_plot()}.
#'
#' @examples
#' \donttest{
#' geom <- example_surface_geometry()
#' p <- surface_plot(geom)
#' p <- add_surface_layer(p, data = rnorm(nrow(coords(geom))))
#' }
#'
#' @export
surface_plot <- function(lh,
                         rh = NULL,
                         views = c("lateral", "medial"),
                         layout = c("grid", "row", "column"),
                         mirror_views = FALSE,
                         flip = FALSE,
                         zoom = 2,
                         background = "white",
                         brightness = 0.5) {

  layout <- match.arg(layout)

  surf_lh <- .ns_normalize_surface(lh, hemi = "lh")
  surf_rh <- if (!is.null(rh)) .ns_normalize_surface(rh, hemi = "rh") else NULL

  if (is.null(surf_lh) && is.null(surf_rh)) {
    stop("At least one of 'lh' or 'rh' must be provided.")
  }

  if (is.null(views)) {
    views <- c("lateral", "medial")
  }
  .ns_check_views(views)

  layout_spec <- .ns_set_layout(surf_lh, surf_rh, layout, views,
                                mirror_views = mirror_views, flip = flip)

  obj <- list(
    surfaces = list(left = surf_lh, right = surf_rh),
    layout = layout_spec,
    zoom = zoom,
    background = background,
    brightness = brightness,
    layers = list(),
    vector_layers = list()
  )
  class(obj) <- "neurosurf_plot"
  obj
}


#' Add a data layer to a surface plot
#'
#' @param x A \code{"neurosurf_plot"} object created by
#'   \code{\link{surface_plot}}.
#' @param data Numeric vector or list specifying vertex-wise data. If a vector,
#'   it should have length equal to the total number of vertices across
#'   hemispheres and is split left-to-right. If a list, it may contain elements
#'   named \code{"left"} and/or \code{"right"}.
#' @param cmap Character string naming a colour map. The value is passed
#'   through to \code{\link{colorRampPalette}} to generate colours.
#' @param alpha Numeric in \eqn{[0,1]} controlling layer opacity.
#' @param color_range Optional numeric vector of length 2 giving the minimum
#'   and maximum data values to map to the colour scale. If \code{NULL}, the
#'   range of \code{data} (ignoring \code{NA}) is used.
#' @param vertices Optional vector or list of vertex ids corresponding to the
#'   supplied \code{data} when it is defined on a subset of vertices. Use a list
#'   with elements \code{left}/\code{right} for hemisphere-specific subsets.
#' @param smoothing One of \code{"auto"} (default) or \code{"nearest"} when
#'   using sparse data. \code{"auto"} fills missing vertices by nearest
#'   neighbour then applies smoothing iterations; \code{"nearest"} performs only
#'   nearest-neighbour fill.
#' @param smoothing_steps Integer number of smoothing iterations applied when
#'   \code{smoothing = "auto"}. Ignored otherwise.
#' @param as_outline Logical; if \code{TRUE}, the data are treated as labels
#'   and are intended to be visualised as region outlines rather than a filled
#'   map. When \code{as_outline = TRUE}, the layer does not contribute to the
#'   filled vertex colours; instead its ROI boundaries are rendered as line
#'   overlays using \code{\link{findBoundaries}}.
#' @param zero_transparent Logical; if \code{TRUE}, zeros are turned into
#'   \code{NA} so they render as transparent.
#' @param show_colorbar Logical; if \code{TRUE}, this layer will contribute a
#'   colour bar when using a figure-level drawing helper.
#' @param label Optional character label identifying the layer (for legends and
#'   colour bars).
#' @param outline_col Colour to use for ROI boundaries when
#'   \code{as_outline = TRUE}. May be a single colour name/hex code or the
#'   special value \code{"auto"}, in which case boundaries are coloured by ROI
#'   using the layer's \code{cmap}. Ignored for non-outline layers.
#' @param outline_lwd Numeric line width to use when drawing ROI boundaries for
#'   outline layers. Ignored for non-outline layers.
#' @param outline_offset Numeric scalar giving a small depth offset applied to
#'   boundary coordinates along the surface normals. This helps avoid
#'   z-fighting with the underlying mesh. A value around \code{0.5}–\code{1}
#'   is often sufficient for standardised cortical meshes.
#' @param outline_halo Logical; if \code{TRUE}, draws a two-pass boundary with
#'   a thicker halo under a thinner main line to improve legibility.
#' @param outline_halo_col Colour used for the halo when
#'   \code{outline_halo = TRUE}. Defaults to a light colour if \code{NULL}.
#' @param outline_halo_lwd Numeric line width for the halo. If \code{NULL},
#'   a slightly larger width than \code{outline_lwd} is used.
#' @param outline_rois Optional numeric vector of ROI ids to plot boundaries
#'   for when \code{as_outline = TRUE}. If \code{NULL}, boundaries are drawn
#'   for all ROIs present in the data.
#' @param outline_lty Line type for boundaries, one of \code{"solid"} or
#'   \code{"dashed"}. Dashed lines are approximated by drawing alternating
#'   short segments along the boundary polyline.
#' @param hemi One of \code{"both"}, \code{"left"}, or \code{"right"},
#'   indicating which hemispheres the supplied \code{data} apply to when a
#'   single numeric vector is given.
#'
#' @return A modified \code{"neurosurf_plot"} object.
#'
#' @examples
#' \donttest{
#' # Requires FreeSurfer-like surface files
#' # sp <- surface_plot(left = "lh.pial", right = "rh.pial")
#' # sp <- add_surface_layer(sp, data = rnorm(163842))
#' }
#'
#' @export
add_surface_layer <- function(x,
                              data,
                              cmap = "viridis",
                              alpha = 1,
                              color_range = NULL,
                              vertices = NULL,
                              smoothing = c("auto", "nearest"),
                              smoothing_steps = 20,
                              as_outline = FALSE,
                              zero_transparent = TRUE,
                              show_colorbar = TRUE,
                              label = NULL,
                              outline_col = "black",
                              outline_lwd = 1.5,
                              outline_offset = 0,
                              outline_halo = FALSE,
                              outline_halo_col = NULL,
                              outline_halo_lwd = NULL,
                              outline_rois = NULL,
                              outline_lty = c("solid", "dashed"),
                              hemi = c("both", "left", "right")) {
  stopifnot(inherits(x, "neurosurf_plot"))
  hemi <- match.arg(hemi)
  smoothing <- match.arg(smoothing)
  outline_lty <- match.arg(outline_lty)

  split_data <- .ns_split_layer_data(
    surfaces = x$surfaces,
    data = data,
    hemi = hemi,
    allow_partial = !is.null(vertices)
  )
  split_vertices <- .ns_split_vertices(
    x$surfaces,
    vertices,
    hemi,
    allow_partial = !is.null(vertices)
  )

  filled_data <- .ns_prepare_layer_data(
    surfaces = x$surfaces,
    data = split_data,
    vertices = split_vertices,
    smoothing = smoothing,
    smoothing_steps = smoothing_steps
  )

  if (is.null(color_range)) {
    vals <- unlist(filled_data, use.names = FALSE)
    color_range <- range(vals, na.rm = TRUE)
    if (!all(is.finite(color_range))) {
      color_range <- c(0, 1)
    }
  }

  layer <- list(
    data = filled_data,
    cmap = cmap,
    alpha = alpha,
    color_range = color_range,
    vertices = split_vertices,
    smoothing = smoothing,
    smoothing_steps = smoothing_steps,
    as_outline = as_outline,
    zero_transparent = zero_transparent,
    show_colorbar = show_colorbar,
    label = label %||% paste0("layer_", length(x$layers) + 1L),
    outline_col = outline_col,
    outline_lwd = outline_lwd,
    outline_offset = outline_offset,
    outline_halo = outline_halo,
    outline_halo_col = outline_halo_col,
    outline_halo_lwd = outline_halo_lwd,
    outline_rois = outline_rois,
    outline_lty = outline_lty
  )

  x$layers <- c(x$layers, list(layer))
  x
}

#' Add a vector field overlay
#'
#' @param x A \code{"neurosurf_plot"} object.
#' @param vectors Matrix (n x 3) of XYZ vectors or a list with \code{left}/\code{right}
#'   matrices. When supplying a single matrix for both hemispheres, rows are split
#'   left-to-right to match the vertex ordering.
#' @param vertices Optional vector or list of vertex ids matching the rows of
#'   \code{vectors} when defined on a subset of vertices. For both hemispheres,
#'   supply a list to avoid ambiguity.
#' @param scale Optional numeric scalar. If \code{NULL}, a heuristic scale is
#'   derived from the mesh extent and vector magnitudes.
#' @param color Colour for the vectors (single value or vector).
#' @param alpha Numeric in \eqn{[0,1]} for vector opacity.
#' @param lwd Numeric line width for the glyphs.
#' @param hemi One of \code{"both"}, \code{"left"}, or \code{"right"} when a
#'   single \code{vectors} matrix is provided.
#'
#' @return A modified \code{"neurosurf_plot"} object.
#'
#' @examples
#' \donttest{
#' # Requires FreeSurfer-like surface files
#' # sp <- surface_plot(left = "lh.pial", right = "rh.pial")
#' # vectors <- matrix(rnorm(163842 * 3), ncol = 3)
#' # sp <- add_vector_layer(sp, vectors = vectors)
#' }
#'
#' @export
add_vector_layer <- function(x,
                             vectors,
                             vertices = NULL,
                             scale = NULL,
                             color = "red",
                             alpha = 0.8,
                             lwd = 1.5,
                             hemi = c("both", "left", "right")) {
  stopifnot(inherits(x, "neurosurf_plot"))
  hemi <- match.arg(hemi)

  vec_data <- .ns_split_vector_data(x$surfaces, vectors, hemi)
  vec_vertices <- .ns_split_vertices(
    x$surfaces,
    vertices,
    hemi,
    allow_partial = !is.null(vertices)
  )

  hemis <- c("left", "right")
  for (h in hemis) {
    vmat <- vec_data[[h]]
    if (is.null(vmat)) {
      next
    }
    surf <- x$surfaces[[h]]
    if (is.null(surf)) {
      next
    }
    if (!is.matrix(vmat) || ncol(vmat) != 3) {
      stop("vectors for ", h, " must be a matrix with 3 columns (XYZ).")
    }
    n_vert <- nrow(coords(surf))
    v_idx <- vec_vertices[[h]]
    if (is.null(v_idx)) {
      if (nrow(vmat) != n_vert) {
        stop("vectors for ", h, " must have one row per vertex or supply vertices.")
      }
    } else if (length(v_idx) != nrow(vmat)) {
      stop("Length of vertices for ", h, " does not match number of vectors.")
    }
  }

  layer <- list(
    data = vec_data,
    vertices = vec_vertices,
    scale = scale,
    color = color,
    alpha = alpha,
    lwd = lwd
  )

  x$vector_layers <- c(x$vector_layers, list(layer))
  x
}


#' Render a neurosurf plot using rgl
#'
#' @param x A \code{"neurosurf_plot"} object.
#' @param offscreen Logical; if \code{TRUE}, rendering is performed with
#'   \code{rgl.useNULL = TRUE} so that plots can be captured as images.
#'   A real GL context is attempted first for better antialiasing.
#' @param scale Numeric vector of length 2 giving a supersampling factor for
#'   the offscreen snapshot. Values above 1 render at higher resolution before
#'   downscaling for smoother edges. Defaults to \code{c(2, 2)}.
#' @param crop Logical; if \code{TRUE}, automatically crops away white/empty
#'   margins from each snapshot to avoid the "tiny brain" effect in grids.
#'
#' @return A list containing rendered panel images (with aspect ratios) and
#'   layout information. This is a low-level helper intended to be wrapped by
#'   higher-level figure drawing utilities.
#'
#' @examples
#' \donttest{
#' geom <- example_surface_geometry()
#' p <- surface_plot(geom)
#' rendered <- render_surface_plot(p)
#' }
#'
#' @seealso \code{\link{surface_plot}}, \code{\link{add_surface_layer}},
#'   \code{\link{view_surface}}
#' @export
render_surface_plot <- function(x,
                                offscreen = TRUE,
                                scale = c(2, 2),
                                crop = TRUE) {
  stopifnot(inherits(x, "neurosurf_plot"))

  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("The 'rgl' package is required for render_surface_plot().")
  }

  old_useNULL <- getOption("rgl.useNULL")
  on.exit(options(rgl.useNULL = old_useNULL), add = TRUE)

  # For snapshot-based offscreen renders, prefer a real context when available;
  # rgl.snapshot on rgl.useNULL devices can return empty/black images on some builds.
  if (offscreen) {
    options(rgl.useNULL = FALSE)
  }

  base_res <- 800
  w <- base_res * scale[1L]
  h <- base_res * scale[2L]

  view_layout <- x$layout$views
  hemi_layout <- x$layout$hemis

  panels <- list()

  for (i in seq_along(view_layout)) {
    v_spec <- view_layout[[i]]
    h_spec <- hemi_layout[[i]]

    surf <- x$surfaces[[h_spec]]
    if (is.null(surf)) {
      next
    }

    # Combine layers for this hemisphere into per-vertex colours.
    vertex_cols <- .ns_compute_vertex_colors(
      layers = x$layers,
      surf = surf,
      hemi = h_spec,
      brightness = x$brightness
    )

    if (!offscreen && !rgl::rgl.useNULL()) {
      rgl::open3d()
    } else if (offscreen) {
      # Prefer a real GL context; fall back to NULL if unavailable (e.g., headless build)
      ok <- FALSE
      try({
        rgl::open3d(windowRect = c(0, 0, w, h))
        ok <- TRUE
      }, silent = TRUE)
      if (!ok) {
        options(rgl.useNULL = TRUE)
        rgl::open3d()
        rgl::par3d(windowRect = c(0, 0, w, h))
      }
    }

    # Basic background support
    try(rgl::bg3d(col = x$background), silent = TRUE)

    view_surface(
      surfgeom = surf,
      vals = NA,
      vert_clrs = vertex_cols,
      bgcol = NA,
      alpha = 1,
      viewpoint = v_spec,
      zoom = x$zoom,
      new_window = FALSE,
      specular = "#101010"
    )

    # Draw ROI outlines for any layers marked as_outline = TRUE
    outline_layers <- Filter(function(layer) isTRUE(layer$as_outline), x$layers)
    if (length(outline_layers)) {
      vertex_normals <- .ns_vertex_normals(surf)
      for (layer in outline_layers) {
        roi_vals <- layer$data[[h_spec]]
        if (is.null(roi_vals)) {
          next
        }
        if (all(is.na(roi_vals))) {
          next
        }

        # Construct a temporary NeuroSurface to reuse findBoundaries()
        n_vert <- nrow(coords(surf))
        if (length(roi_vals) != n_vert) {
          warning("Outline layer data length does not match number of vertices; skipping outlines for this layer.")
          next
        }

        # Build an auto palette if requested
        roi_ids <- sort(unique(roi_vals[!is.na(roi_vals)]))
        if (identical(layer$outline_col, "auto")) {
          pal <- .ns_cmap_to_colors(layer$cmap, n = max(length(roi_ids), 3L))
        } else {
          pal <- NULL
        }

        ns_obj <- NeuroSurface(
          geometry = surf,
          indices = seq_len(n_vert),
          data = roi_vals
        )

        b <- tryCatch(
          findBoundaries(ns_obj, method = "edge_vertices"),
          error = function(e) {
            warning("findBoundaries() failed for outline layer: ", conditionMessage(e))
            NULL
          }
        )

        if (!is.null(b) && length(b$boundary)) {
          for (k in seq_along(b$boundary)) {
            coords_k <- b$boundary[[k]]
            if (is.null(coords_k) || nrow(coords_k) < 2L) {
              next
            }

            # Optional ROI subset selection
            if (!is.null(layer$outline_rois) &&
                !is.null(b$boundary_roi_id) &&
                length(b$boundary_roi_id) >= k) {
              roi_k <- b$boundary_roi_id[k]
              if (!(roi_k %in% layer$outline_rois)) {
                next
              }
            } else {
              roi_k <- if (!is.null(b$boundary_roi_id) && length(b$boundary_roi_id) >= k)
                b$boundary_roi_id[k] else NA_integer_
            }

            # Optional depth offset along vertex normals
            if (!is.null(layer$outline_offset) &&
                is.numeric(layer$outline_offset) &&
                layer$outline_offset != 0 &&
                !is.null(b$boundary_verts) &&
                length(b$boundary_verts) >= k &&
                !is.null(vertex_normals)) {
              v_ids <- b$boundary_verts[[k]]
              if (length(v_ids) == nrow(coords_k) &&
                  all(v_ids >= 1L) &&
                  all(v_ids <= nrow(vertex_normals))) {
                nrm <- vertex_normals[v_ids, , drop = FALSE]
                coords_k <- coords_k + layer$outline_offset * nrm
              }
            }

            # Resolve main line colour for this boundary
             # Choose per-boundary colour
            col_k <- layer$outline_col
            if (identical(layer$outline_col, "auto") && !is.null(pal) &&
                !is.na(roi_k)) {
              idx_k <- match(roi_k, roi_ids)
              if (!is.na(idx_k) && idx_k >= 1L && idx_k <= length(pal)) {
                col_k <- pal[idx_k]
              } else {
                col_k <- "black"
              }
            }

            # Halo (drawn first, underneath)
            if (isTRUE(layer$outline_halo)) {
              halo_col <- if (is.null(layer$outline_halo_col)) {
                # Default to a lightened version of the main colour
                utils::tail(.ns_cmap_to_colors(c("white", col_k), n = 3L), 1L)
              } else {
                layer$outline_halo_col
              }
              halo_lwd <- if (is.null(layer$outline_halo_lwd)) {
                layer$outline_lwd + 1
              } else {
                layer$outline_halo_lwd
              }
              .ns_draw_polyline(coords_k,
                                col = halo_col,
                                lwd = halo_lwd,
                                lty = layer$outline_lty)
            }

            # Main outline
            .ns_draw_polyline(coords_k,
                              col = col_k,
                              lwd = layer$outline_lwd,
                              lty = layer$outline_lty)
          }
        }
      }
    }

    if (length(x$vector_layers)) {
      for (v_layer in x$vector_layers) {
        vecs <- v_layer$data[[h_spec]]
        if (is.null(vecs)) {
          next
        }
        verts <- v_layer$vertices[[h_spec]]
        tryCatch(
          .ns_draw_vector_overlay(
            surf = surf,
            vectors = vecs,
            vertices = verts,
            scale = v_layer$scale,
            color = v_layer$color,
            alpha = v_layer$alpha,
            lwd = v_layer$lwd
          ),
          error = function(e) warning("Skipping vector layer for ", h_spec, ": ", e$message)
        )
      }
    }

    if (offscreen) {
      tmpfile <- tempfile(fileext = ".png")

      # force scene update before snapshot for some drivers
      if (exists("rgl.bringtotop", where = asNamespace("rgl"), mode = "function")) {
        try(rgl::rgl.bringtotop(), silent = TRUE)
      }

      rgl::rgl.snapshot(filename = tmpfile)
      img <- png::readPNG(tmpfile)
      unlink(tmpfile)

      if (isTRUE(crop)) {
        img <- .ns_autocrop(img, border = 10)
      }

      panels[[length(panels) + 1L]] <- list(
        image = img,
        view = v_spec,
        hemi = h_spec,
        aspect = ncol(img) / nrow(img)
      )
      rgl::close3d()
    }
  }

  list(
    panels = panels,
    layout = x$layout
  )
}


#' Draw a static multi-panel surface figure
#'
#' This is a convenience wrapper around \code{\link{render_surface_plot}} that
#' arranges rendered panels into a single static figure using the \pkg{grid}
#' graphics system. It supersamples, crops whitespace, and preserves per-panel
#' aspect ratios to avoid tiny or distorted brains when assembled.
#'
#' @param x A \code{"neurosurf_plot"} object.
#' @param colorbar Logical; if \code{TRUE}, draws one or more colour bars for
#'   non-outline layers that have \code{show_colorbar = TRUE}.
#' @param cbar_location Location of colour bars relative to the panel layout.
#'   Currently supports \code{"bottom"} (default) or \code{"right"}.
#' @param cbar_kws Optional list of graphical parameters for colour bars
#'   (e.g., \code{bar_height}, \code{title_cex}, \code{label_cex}, \code{n_ticks}).
#'
#' @return A \code{grob} object that can be drawn with \code{grid::grid.draw()}.
#'
#' @examples
#' \donttest{
#' # Requires FreeSurfer-like surface files
#' # sp <- surface_plot(left = "lh.pial", right = "rh.pial")
#' # g <- draw_surface_plot(sp)
#' # grid::grid.draw(g)
#' }
#'
#' @export
draw_surface_plot <- function(x,
                              colorbar = TRUE,
                              cbar_location = c("bottom", "right"),
                              cbar_kws = list()) {
  stopifnot(inherits(x, "neurosurf_plot"))

  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("The 'grid' package is required for draw_surface_plot().")
  }

  cbar_location <- match.arg(cbar_location)
  bg_fill <- x$background %||% "white"
  with_background <- function(grob) {
    grid::grobTree(
      grid::rectGrob(gp = grid::gpar(fill = bg_fill, col = NA)),
      grob
    )
  }

  rendered <- render_surface_plot(x, offscreen = TRUE, crop = TRUE)
  panels <- rendered$panels
  dims <- rendered$layout$dims

  if (is.null(dims) || length(dims) != 2L) {
    stop("Layout information is missing; cannot arrange panels.")
  }

  nrow <- dims[1L]
  ncol <- dims[2L]

  if (!length(panels)) {
    stop("No panels were rendered; check that surfaces and layers are defined.")
  }

  if (length(panels) != nrow * ncol) {
    warning("Number of rendered panels does not match layout dimensions; ",
            "figure layout may be incorrect.")
  }

  panel_grobs <- lapply(panels, function(p) {
    img <- p$image
    aspect <- p$aspect %||% (ncol(img) / nrow(img))
    if (!is.finite(aspect) || aspect <= 0) {
      aspect <- 1
    }

    # Preserve aspect by shrinking the limiting dimension inside the cell
    if (aspect >= 1) {
      vp <- grid::viewport(width = grid::unit(1, "npc"),
                           height = grid::unit(1 / aspect, "npc"),
                           x = 0.5, y = 0.5,
                           just = c("center", "center"))
    } else {
      vp <- grid::viewport(width = grid::unit(aspect, "npc"),
                           height = grid::unit(1, "npc"),
                           x = 0.5, y = 0.5,
                           just = c("center", "center"))
    }

    grid::grobTree(
      grid::rasterGrob(img, interpolate = TRUE),
      vp = vp
    )
  })

  layout <- grid::grid.layout(nrow = nrow, ncol = ncol)
  frame <- grid::frameGrob(layout = layout)

  for (k in seq_along(panel_grobs)) {
    row <- ((k - 1L) %% nrow) + 1L
    col <- ((k - 1L) %/% nrow) + 1L
    frame <- grid::placeGrob(frame, panel_grobs[[k]], row = row, col = col)
  }

  # Add colour bars if requested and available
  if (!isTRUE(colorbar)) {
    return(with_background(frame))
  }

  color_layers <- Filter(
    function(layer) !isTRUE(layer$as_outline) && isTRUE(layer$show_colorbar),
    x$layers
  )

  if (!length(color_layers)) {
    return(with_background(frame))
  }

  default_cb <- list(
    bar_height = grid::unit(2, "mm"),
    title_cex = 0.8,
    label_cex = 0.6,
    n_ticks = 3L
  )
  cb_kws <- utils::modifyList(default_cb, cbar_kws, keep.null = TRUE)

  cb_grob <- .ns_build_colorbars(color_layers,
                                 location = cbar_location,
                                 cbar_kws = cb_kws)

  if (cbar_location == "bottom") {
    outer <- grid::frameGrob(layout = grid::grid.layout(nrow = 2L, ncol = 1L,
                                                       heights = grid::unit.c(grid::unit(1, "null"),
                                                                               grid::unit(0.18, "npc"))))
    outer <- grid::placeGrob(outer, frame, row = 1L, col = 1L)
    outer <- grid::placeGrob(outer, cb_grob, row = 2L, col = 1L)
  } else {
    outer <- grid::frameGrob(layout = grid::grid.layout(nrow = 1L, ncol = 2L,
                                                       widths = grid::unit.c(grid::unit(1, "null"),
                                                                              grid::unit(0.18, "npc"))))
    outer <- grid::placeGrob(outer, frame, row = 1L, col = 1L)
    outer <- grid::placeGrob(outer, cb_grob, row = 1L, col = 2L)
  }

  with_background(outer)
}

#' Plot method for neurosurf_plot objects
#'
#' This is a convenience wrapper that renders a multi-panel surface layout and
#' draws it to a new grid device.
#'
#' @param x A \code{"neurosurf_plot"} object.
#' @param ... Additional arguments passed to \code{\link{draw_surface_plot}}.
#'
#' @return Invisibly returns the input \code{neurosurf_plot} object.
#'
#' @method plot neurosurf_plot
#' @export
plot.neurosurf_plot <- function(x, ...) {
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("The 'grid' package is required to plot a neurosurf_plot object.")
  }

  g <- draw_surface_plot(x, ...)
  grid::grid.newpage()
  grid::grid.draw(g)
  invisible(x)
}


# Internal utilities ---------------------------------------------------------

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Auto-crop a raster image (removes whitespace)
#' @param img A raster image array
#' @param border Number of pixels to preserve as border
#' @return The cropped image array with whitespace removed
#' @keywords internal
.ns_autocrop <- function(img, border = 0) {
  if (length(dim(img)) < 3L) {
    return(img)
  }

  if (dim(img)[3L] == 4L) {
    is_content <- img[, , 4L] > 0.01
  } else {
    is_content <- (img[, , 1L] < 0.99) | (img[, , 2L] < 0.99) | (img[, , 3L] < 0.99)
  }

  rows <- which(rowSums(is_content) > 0)
  cols <- which(colSums(is_content) > 0)

  if (!length(rows) || !length(cols)) {
    return(img)
  }

  r1 <- max(1L, min(rows) - border)
  r2 <- min(nrow(img), max(rows) + border)
  c1 <- max(1L, min(cols) - border)
  c2 <- min(ncol(img), max(cols) + border)

  img[r1:r2, c1:c2, , drop = FALSE]
}

#' Supersample/Resize image for antialiasing (placeholder)
#' @param img A raster image array
#' @param scale_factor Scale factor for resizing (default 0.5)
#' @return The resized image array
#' @keywords internal
.ns_resize_img <- function(img, scale_factor = 0.5) {
  # Placeholder: for now we rely on grid::rasterGrob interpolation.
  # Kept for future extension with magick/EBImage if desired.
  img
}

.ns_normalize_surface <- function(x, hemi) {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "SurfaceGeometry")) {
    return(x)
  }
  if (is.character(x) && length(x) == 1L) {
    sg <- read_surf_geometry(x)
    sg@hemi <- hemi
    return(sg)
  }
  stop("Unsupported surface specification for hemi = ", hemi,
       ". Expected a SurfaceGeometry or a file path.")
}

.ns_check_views <- function(views) {
  # Currently we support the subset of views implemented by view_surface().
  valid <- c("medial", "lateral", "ventral", "dorsal", "anterior", "posterior")
  bad <- setdiff(unique(views), valid)
  if (length(bad)) {
    stop("Invalid view(s): ", paste(bad, collapse = ", "),
         ". Valid views are: ", paste(valid, collapse = ", "), ".")
  }
  invisible(views)
}

.ns_set_layout <- function(lh, rh, layout, views, mirror_views, flip) {
  # Determine hemispheres present
  hemis <- c(if (!is.null(lh)) "left", if (!is.null(rh)) "right")
  n_hemi <- length(hemis)
  n_views <- length(views)

  # Build parallel vectors of view and hemisphere labels
  v <- character(0)
  h <- character(0)

  if (!is.null(lh)) {
    v <- c(v, views)
    h <- c(h, rep("left", n_views))
  }
  if (!is.null(rh)) {
    rh_views <- if (mirror_views && layout != "grid") {
      rev(views)
    } else {
      views
    }
    v <- c(v, rh_views)
    h <- c(h, rep("right", n_views))
  }

  # Reshape for grid/column layouts
  if (layout == "grid") {
    v_mat <- matrix(v, nrow = n_views, ncol = n_hemi, byrow = FALSE)
    h_mat <- matrix(h, nrow = n_views, ncol = n_hemi, byrow = FALSE)
  } else if (layout == "column") {
    v_mat <- matrix(v, ncol = 1L)
    h_mat <- matrix(h, ncol = 1L)
  } else { # row
    v_mat <- matrix(v, nrow = 1L)
    h_mat <- matrix(h, nrow = 1L)
  }

  if (flip && n_hemi == 2L) {
    v_mat <- v_mat[, ncol(v_mat):1L, drop = FALSE]
    h_mat <- h_mat[, ncol(h_mat):1L, drop = FALSE]
  }

  list(
    views = as.vector(v_mat),
    hemis = as.vector(h_mat),
    layout = layout,
    dims = dim(v_mat)
  )
}

.ns_split_layer_data <- function(surfaces, data, hemi, allow_partial = FALSE) {
  if (is.list(data)) {
    out <- list(left = data$left, right = data$right)
    return(out)
  }

  stopifnot(is.numeric(data))

  n_left <- if (!is.null(surfaces$left)) nrow(coords(surfaces$left)) else 0L
  n_right <- if (!is.null(surfaces$right)) nrow(coords(surfaces$right)) else 0L

  total <- n_left + n_right

  if (length(data) != total) {
    if (!allow_partial) {
      stop("Length of 'data' (", length(data),
           ") does not match total number of vertices (",
           total, ").")
    }
    if (hemi == "both") {
      stop("Provide hemisphere-specific data or set hemi to 'left'/'right' ",
           "when using sparse data.")
    }
  }

  if (hemi == "both") {
    left_vals <- if (n_left) data[seq_len(n_left)] else NULL
    right_vals <- if (n_right) data[n_left + seq_len(n_right)] else NULL
  } else if (hemi == "left") {
    left_vals <- if (n_left) data[seq_len(n_left)] else NULL
    right_vals <- NULL
  } else {
    left_vals <- NULL
    right_vals <- if (n_right) data[seq_len(n_right)] else NULL
  }

  list(left = left_vals, right = right_vals)
}

.ns_split_vertices <- function(surfaces, vertices, hemi, allow_partial = FALSE) {
  if (is.null(vertices)) {
    return(list(left = NULL, right = NULL))
  }

  if (is.list(vertices)) {
    return(list(left = vertices$left, right = vertices$right))
  }

  stopifnot(is.numeric(vertices))

  n_left <- if (!is.null(surfaces$left)) nrow(coords(surfaces$left)) else 0L
  n_right <- if (!is.null(surfaces$right)) nrow(coords(surfaces$right)) else 0L

  if (hemi == "left") {
    return(list(left = vertices, right = NULL))
  }
  if (hemi == "right") {
    return(list(left = NULL, right = vertices))
  }

  total <- n_left + n_right
  if (length(vertices) != total) {
    if (!allow_partial) {
      stop("Length of 'vertices' (", length(vertices),
           ") does not match total number of vertices (",
           total, "). Supply a list with left/right indices for sparse data.")
    }
    stop("Provide hemisphere-specific vertices (list) when using sparse data with both hemispheres.")
  }

  left_vals <- if (n_left) vertices[seq_len(n_left)] else NULL
  right_vals <- if (n_right) vertices[n_left + seq_len(n_right)] else NULL
  list(left = left_vals, right = right_vals)
}

.ns_split_vector_data <- function(surfaces, vectors, hemi) {
  if (is.list(vectors)) {
    out <- list(left = if (!is.null(vectors$left)) as.matrix(vectors$left) else NULL,
                right = if (!is.null(vectors$right)) as.matrix(vectors$right) else NULL)
    return(out)
  }

  vec_mat <- as.matrix(vectors)
  if (ncol(vec_mat) != 3) {
    stop("vectors must have 3 columns (XYZ).")
  }

  n_left <- if (!is.null(surfaces$left)) nrow(coords(surfaces$left)) else 0L
  n_right <- if (!is.null(surfaces$right)) nrow(coords(surfaces$right)) else 0L

  if (hemi == "left") {
    return(list(left = vec_mat, right = NULL))
  }
  if (hemi == "right") {
    return(list(left = NULL, right = vec_mat))
  }

  if (nrow(vec_mat) != (n_left + n_right)) {
    stop("When supplying a single vectors matrix for both hemispheres, rows must ",
         "match total vertices or provide a left/right list.")
  }

  left_vals <- if (n_left) vec_mat[seq_len(n_left), , drop = FALSE] else NULL
  right_vals <- if (n_right) vec_mat[n_left + seq_len(n_right), , drop = FALSE] else NULL
  list(left = left_vals, right = right_vals)
}

.ns_prepare_layer_data <- function(surfaces, data, vertices, smoothing, smoothing_steps) {
  out <- data
  hemis <- c("left", "right")
  for (h in hemis) {
    vals <- data[[h]]
    surf <- surfaces[[h]]
    if (is.null(vals) || is.null(surf)) {
      next
    }
    n_vert <- nrow(coords(surf))
    if (length(vals) == n_vert) {
      out[[h]] <- vals
      next
    }
    verts <- vertices[[h]]
    if (is.null(verts)) {
      stop("Layer data length for ", h, " (", length(vals),
           ") does not match number of vertices (", n_vert,
           "); supply vertices for sparse data.")
    }
    if (length(verts) != length(vals)) {
      stop("Length of vertices for ", h, " (", length(verts),
           ") does not match length of data (", length(vals), ").")
    }
    out[[h]] <- .ns_fill_sparse_data(
      surf = surf,
      values = vals,
      vertices = verts,
      smoothing = smoothing,
      smoothing_steps = smoothing_steps
    )
  }
  out
}

.ns_fill_sparse_data <- function(surf, values, vertices,
                                 smoothing = c("auto", "nearest"),
                                 smoothing_steps = 20) {
  smoothing <- match.arg(smoothing)
  n_vert <- nrow(coords(surf))

  if (length(values) == n_vert) {
    return(values)
  }

  v_idx <- as.integer(vertices)
  if (any(v_idx < 1L | v_idx > n_vert)) {
    stop("vertex ids must be within [1, ", n_vert, "].")
  }
  if (length(values) != length(v_idx)) {
    stop("values length (", length(values), ") must match length of vertices (", length(v_idx), ").")
  }

  coords_all <- coords(surf)
  seeds <- rep(NA_real_, n_vert)
  seeds[v_idx] <- values

  knn <- FNN::get.knnx(data = coords_all[v_idx, , drop = FALSE],
                       query = coords_all,
                       k = 1L)
  filled <- seeds
  filled[is.na(filled)] <- values[knn$nn.index[is.na(filled), 1L]]

  if (identical(smoothing, "nearest") || smoothing_steps <= 0) {
    return(filled)
  }

  adj <- igraph::adjacent_vertices(graph(surf), seq_len(n_vert))
  seed_mask <- rep(FALSE, n_vert)
  seed_mask[v_idx] <- TRUE

  current <- filled
  for (i in seq_len(smoothing_steps)) {
    updated <- current
    for (v in seq_len(n_vert)) {
      neigh <- adj[[v]]
      vals <- current[c(v, neigh)]
      vals <- vals[!is.na(vals)]
      if (length(vals)) {
        updated[v] <- mean(vals)
      }
    }
    updated[seed_mask] <- filled[seed_mask]
    current <- updated
  }

  current
}

.ns_draw_vector_overlay <- function(surf, vectors, vertices = NULL,
                                    scale = NULL, color = "red",
                                    alpha = 0.8, lwd = 1.5) {
  surf_coords <- coords(surf)
  vecs <- as.matrix(vectors)
  if (ncol(vecs) != 3) {
    stop("vectors must have 3 columns (XYZ).")
  }

  if (is.null(vertices)) {
    if (nrow(vecs) != nrow(surf_coords)) {
      stop("vectors must have one row per vertex or supply vertices.")
    }
    verts <- seq_len(nrow(surf_coords))
  } else {
    verts <- as.integer(vertices)
    if (length(verts) != nrow(vecs)) {
      stop("Length of vertices does not match number of vectors.")
    }
  }

  norms <- sqrt(rowSums(vecs^2))
  max_range <- max(apply(surf_coords, 2, function(col) diff(range(col))))
  if (!is.finite(max_range) || max_range == 0) {
    max_range <- 1
  }
  if (is.null(scale)) {
    max_norm <- max(norms, na.rm = TRUE)
    if (!is.finite(max_norm) || max_norm == 0) {
      max_norm <- 1
    }
    scale <- 0.05 * max_range / max_norm
  }

  starts <- surf_coords[verts, , drop = FALSE]
  ends <- starts + scale * vecs

  col_vec <- color
  if (length(col_vec) == 1L) {
    col_vec <- rep(col_vec, nrow(starts))
  }
  col_vec <- rep(col_vec, each = 2L)

  rgl::segments3d(
    x = as.vector(rbind(starts[, 1], ends[, 1])),
    y = as.vector(rbind(starts[, 2], ends[, 2])),
    z = as.vector(rbind(starts[, 3], ends[, 3])),
    col = col_vec,
    alpha = alpha,
    lwd = lwd
  )
}

.ns_cmap_to_colors <- function(cmap, n = 256L) {
  if (length(cmap) > 1L) {
    return(grDevices::colorRampPalette(cmap)(n))
  }

  if (identical(cmap, "viridis")) {
    # Hard-coded viridis-like palette to avoid extra dependencies
    base_cols <- c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF")
    return(grDevices::colorRampPalette(base_cols)(n))
  }

  # Fallback: simple blue-white-red palette
  grDevices::colorRampPalette(c("blue", "white", "red"))(n)
}

.ns_compute_vertex_colors <- function(layers, surf, hemi, brightness) {
  n_vert <- nrow(coords(surf))

  base_hex <- grDevices::gray(brightness)
  base_layer <- colorplane::HexColorPlane(rep(base_hex, n_vert))

  if (!length(layers)) {
    return(colorplane::as_hexcol(base_layer))
  }

  cur <- base_layer

  for (i in seq_along(layers)) {
    layer <- layers[[i]]
    # Outline-only layers do not contribute to filled vertex colours.
    if (isTRUE(layer$as_outline)) {
      next
    }
    vals <- layer$data[[hemi]]
    if (is.null(vals)) {
      next
    }

    v <- vals
    if (layer$zero_transparent) {
      v[v == 0] <- NA_real_
    }
    if (all(is.na(v))) {
      next
    }

    cmap_cols <- .ns_cmap_to_colors(layer$cmap)
    fg_plane <- colorplane::IntensityColorPlane(v, cmap_cols, alpha = 1)
    fg_clrs <- colorplane::map_colors(
      fg_plane,
      alpha = layer$alpha,
      threshold = NULL,
      irange = layer$color_range
    )

    cur <- colorplane::blend_colors(cur, fg_clrs, alpha = layer$alpha)
  }

  colorplane::as_hexcol(cur)
}

.ns_vertex_normals <- function(surf) {
  mesh <- surf@mesh
  if (is.null(mesh$normals)) {
    mesh <- rgl::addNormals(mesh)
    surf@mesh <- mesh
  }
  if (is.null(mesh$normals)) {
    return(NULL)
  }
  t(mesh$normals[1:3, , drop = FALSE])
}

.ns_draw_polyline <- function(coords, col, lwd, lty = c("solid", "dashed")) {
  lty <- match.arg(lty)
  n <- nrow(coords)
  if (n < 2L) {
    return(invisible(NULL))
  }

  if (lty == "solid") {
    rgl::lines3d(
      x = coords[, 1L],
      y = coords[, 2L],
      z = coords[, 3L],
      col = col,
      lwd = lwd
    )
    return(invisible(NULL))
  }

  # Simple dashed approximation: draw every other segment.
  seg_idx <- seq_len(n - 1L)
  draw_mask <- (seg_idx %% 2L) == 1L
  for (s in seg_idx[draw_mask]) {
    rgl::lines3d(
      x = coords[s:(s + 1L), 1L],
      y = coords[s:(s + 1L), 2L],
      z = coords[s:(s + 1L), 3L],
      col = col,
      lwd = lwd
    )
  }
  invisible(NULL)
}

.ns_build_colorbars <- function(layers,
                                location = c("bottom", "right"),
                                cbar_kws = list()) {
  location <- match.arg(location)
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("The 'grid' package is required for colour bar rendering.")
  }

  n_layers <- length(layers)
  if (!n_layers) {
    return(grid::nullGrob())
  }

  n_ticks <- cbar_kws$n_ticks %||% 3L
  digits  <- cbar_kws$digits  %||% 2L
  label_cex <- cbar_kws$label_cex %||% 0.7
  title_cex <- cbar_kws$title_cex %||% 0.8
  bar_height <- cbar_kws$bar_height %||% grid::unit(0.8, "lines")
  bar_spacing <- cbar_kws$bar_spacing %||% grid::unit(0.4, "lines")

  bar_grobs <- vector("list", n_layers)

  for (i in seq_len(n_layers)) {
    layer <- layers[[i]]
    cols  <- .ns_cmap_to_colors(layer$cmap, n = 256L)
    img   <- matrix(cols, nrow = 1L)

    rng <- layer$color_range
    if (any(!is.finite(rng))) {
      rng <- c(0, 1)
    }

    ticks <- if (n_ticks > 1L) {
      seq(rng[1], rng[2], length.out = n_ticks)
    } else {
      rng[2]
    }
    span <- diff(range(rng))
    tick_pos <- if (span > 0) {
      (ticks - rng[1]) / span
    } else {
      rep(0, length(ticks))
    }

    tick_lab <- if (digits > 0L) {
      formatC(ticks, digits = digits, format = "f")
    } else {
      as.character(round(ticks))
    }

    bar <- grid::rasterGrob(img, interpolate = TRUE,
                            height = bar_height)

    title_lab <- layer$label %||% ""
    title <- if (nzchar(title_lab)) {
      grid::textGrob(
        label = title_lab,
        x = grid::unit(0, "npc"),
        y = grid::unit(1, "npc"),
        just = c("left", "top"),
        gp = grid::gpar(cex = title_cex, fontface = "bold")
      )
    } else {
      grid::nullGrob()
    }
    axis <- grid::grobTree(
      grid::segmentsGrob(
        x0 = grid::unit(tick_pos, "npc"),
        x1 = grid::unit(tick_pos, "npc"),
        y0 = grid::unit(0, "npc"),
        y1 = grid::unit(0.3, "npc"),
        gp = grid::gpar(col = "black", lwd = 0.5)
      ),
      grid::textGrob(
        label = tick_lab,
        x = grid::unit(tick_pos, "npc"),
        y = grid::unit(0.9, "npc"),
        gp = grid::gpar(cex = label_cex)
      )
    )

    lay <- grid::grid.layout(
      nrow = 3L,
      ncol = 1L,
      heights = grid::unit.c(
        bar_spacing,
        bar_height,
        grid::unit(1, "lines")
      )
    )
    frame <- grid::frameGrob(layout = lay)
    frame <- grid::placeGrob(frame, title, row = 1L, col = 1L)
    frame <- grid::placeGrob(frame, bar,   row = 2L, col = 1L)
    frame <- grid::placeGrob(frame, axis,  row = 3L, col = 1L)

    bar_grobs[[i]] <- frame
  }

  # Stack bars vertically; location is handled at the caller level
  lay_outer <- grid::grid.layout(nrow = n_layers, ncol = 1L)
  outer <- grid::frameGrob(layout = lay_outer)
  for (i in seq_len(n_layers)) {
    outer <- grid::placeGrob(outer, bar_grobs[[i]], row = i, col = 1L)
  }

  outer
}
