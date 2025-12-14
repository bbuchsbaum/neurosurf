#' Find boundaries of ROIs on a surface mesh
#'
#' This function identifies boundaries between regions of interest (ROIs)
#' defined on a triangular surface mesh. It is a translation of the core
#' parts of Stuart Oldham's \code{findROIboundaries} MATLAB function, adapted
#' for use with neurosurf objects.
#'
#' Two boundary representations are currently supported:
#' \itemize{
#'   \item \code{"faces"}: returns a logical vector indicating which faces lie
#'     on a boundary between ROIs.
#'   \item \code{"edge_vertices"}: returns boundary polygons traced through
#'     mesh vertices, suitable for drawing ROI outlines.
#' }
#'
#' @param vertices Numeric matrix of vertex coordinates (\eqn{n \times 3}).
#' @param faces Integer matrix of face indices (\eqn{m \times 3}, 1-based).
#' @param vertex_id Integer vector of ROI labels for each vertex (length \eqn{n}).
#' @param boundary_method One of \code{"faces"} or \code{"edge_vertices"}.
#' @param verbose Logical; if \code{TRUE}, print progress messages.
#' @param use_cpp Logical; if \code{TRUE}, use optimized C++ implementation.
#'
#' @return A list with elements:
#' \describe{
#'   \item{\code{boundary}}{For \code{"faces"}: logical vector of length
#'     \code{nrow(faces)} indicating boundary faces.
#'     For \code{"edge_vertices"}: list of coordinate matrices (\eqn{k \times 3})
#'     giving boundary polygons for each connected ROI component.}
#'   \item{\code{boundary_roi_id}}{Integer vector giving the ROI id associated
#'     with each boundary polygon (empty for \code{"faces"}).}
#'   \item{\code{roi_components}}{Integer vector giving the number of connected
#'     components for each ROI (indexed parallel to \code{sort(unique(vertex_id))}).}
#'   \item{\code{boundary_verts}}{For \code{"edge_vertices"}: list of integer
#'     vectors giving the vertex ids used for each boundary polygon; \code{NULL}
#'     for \code{"faces"}.}
#' }
#'
#' @examples
#' \donttest{
#' # Simple cube mesh with two ROIs (bottom vs top)
#' vertices <- matrix(c(
#'   0,0,0, 1,0,0, 1,1,0, 0,1,0,
#'   0,0,1, 1,0,1, 1,1,1, 0,1,1
#' ), ncol = 3, byrow = TRUE)
#'
#' faces <- matrix(c(
#'   1,2,3, 1,3,4,
#'   5,6,7, 5,7,8,
#'   1,2,6, 1,6,5,
#'   3,4,8, 3,8,7,
#'   1,4,8, 1,8,5,
#'   2,3,7, 2,7,6
#' ), ncol = 3, byrow = TRUE)
#'
#' roi <- c(1,1,1,1, 2,2,2,2)
#'
#' b <- find_roi_boundaries(vertices, faces, roi, boundary_method = "edge_vertices")
#' str(b$boundary)
#' }
#'
#' @importFrom igraph graph_from_edgelist components degree neighbors delete_edges
#'   get.edge.ids shortest_paths V
#' @export
find_roi_boundaries <- function(vertices,
                                faces,
                                vertex_id,
                                boundary_method = c("faces", "edge_vertices"),
                                verbose = FALSE,
                                use_cpp = TRUE) {
  boundary_method <- match.arg(boundary_method)

  vertices <- as.matrix(vertices)
  faces <- as.matrix(faces)
  vertex_id <- as.vector(vertex_id)

  if (ncol(vertices) != 3L) {
    stop("vertices must be an n x 3 numeric matrix.")
  }
  if (ncol(faces) != 3L) {
    stop("faces must be an m x 3 integer matrix of vertex indices.")
  }
  if (length(vertex_id) != nrow(vertices)) {
    stop("length(vertex_id) must equal nrow(vertices).")
  }
  if (any(faces < 1L) || any(faces > nrow(vertices))) {
    stop("faces must reference vertex indices between 1 and nrow(vertices).")
  }

  # ROI ids per face vertex
  if (verbose) message("Preparing face ROI ids...")
  faces_roi_ids <- matrix(vertex_id[faces], ncol = 3L)

  if (boundary_method == "faces") {
    # A face is on a boundary if any pair of its vertices differ in ROI id.
    is_boundary <- faces_roi_ids[, 1L] != faces_roi_ids[, 2L] |
                   faces_roi_ids[, 2L] != faces_roi_ids[, 3L] |
                   faces_roi_ids[, 1L] != faces_roi_ids[, 3L]
    return(list(
      boundary = is_boundary,
      boundary_roi_id = NULL,
      roi_components = NULL,
      boundary_verts = NULL
    ))
  }

  # edge_vertices method ----------------------------------------------------

  if (use_cpp) {
    if (verbose) message("Computing ROI boundary loops via C++ ...")
    res <- roi_boundary_loops_cpp(vertices, faces, as.integer(vertex_id))
    # Drop roi_ids if you don't want it in the public API
    res$roi_ids <- NULL
    return(res)
  } else {
    # Face "colour" is the ROI id of the first vertex of each face
    face_color <- faces_roi_ids[, 1L]

    # Faces that touch more than one ROI
    n_rois_per_face <- apply(faces_roi_ids, 1L, function(x) length(unique(x)))
    boundary_faces_mask <- n_rois_per_face > 1L

    # Construct edges (three per face)
    e1 <- cbind(faces[, 1L], faces[, 2L])
    e2 <- cbind(faces[, 2L], faces[, 3L])
    e3 <- cbind(faces[, 3L], faces[, 1L])
    all_edges <- rbind(e1, e2, e3)

    # Edge metadata replicated per face
    edge_face_color <- rep(face_color, each = 3L)
    edge_face_id <- rep(seq_len(nrow(faces)), each = 3L)
    edge_is_boundary_face <- rep(boundary_faces_mask, each = 3L)

    # Sort within edges so (u,v) == (v,u)
    all_edges_sorted <- t(apply(all_edges, 1L, sort))

    if (verbose) message("Building edge table...")
    edges_df <- data.frame(
      v1 = all_edges_sorted[, 1L],
      v2 = all_edges_sorted[, 2L],
      roi_v1 = vertex_id[all_edges_sorted[, 1L]],
      roi_v2 = vertex_id[all_edges_sorted[, 2L]],
      face_color = edge_face_color,
      face_id = edge_face_id,
      boundary_face = as.integer(edge_is_boundary_face),
      stringsAsFactors = FALSE
    )

    # Sort by vertices so duplicate edges become adjacent
    ord <- order(edges_df$v1, edges_df$v2)
    edges_df_sorted <- edges_df[ord, ]

    if (nrow(edges_df_sorted) %% 2L != 0L) {
      warning("Mesh edges are not consistently paired; results may be unreliable for open meshes.")
      edges_df_sorted <- edges_df_sorted[seq_len(nrow(edges_df_sorted) - 1L), ]
    }

    odd_idx <- seq(1L, nrow(edges_df_sorted), by = 2L)
    even_idx <- seq(2L, nrow(edges_df_sorted), by = 2L)

    e_odd <- edges_df_sorted[odd_idx, ]
    e_even <- edges_df_sorted[even_idx, ]

    # Unique edges and their meta-data
    unique_edges <- data.frame(
      v1 = e_odd$v1,
      v2 = e_odd$v2,
      roi_v1 = e_odd$roi_v1,
      roi_v2 = e_odd$roi_v2,
      diff_color = as.integer(e_odd$face_color != e_even$face_color),
      face_id1 = e_odd$face_id,
      face_id2 = e_even$face_id,
      boundary_face = pmax(e_odd$boundary_face, e_even$boundary_face),
      stringsAsFactors = FALSE
    )
  }

  roi_ids <- sort(unique(vertex_id))
  nrois <- length(roi_ids)
  roi_components <- numeric(nrois)

  # Pre-split edge rows by ROI to avoid repeated which() scans
  if (nrow(unique_edges) == 0L) {
    return(list(
      boundary = list(),
      boundary_roi_id = integer(0L),
      roi_components = integer(length(roi_ids)),
      boundary_verts = list()
    ))
  }

  edges_by_roi <- split(seq_len(nrow(unique_edges)), unique_edges$roi_v1)

  boundaries <- list()
  boundary_roi_id <- integer(0L)
  boundary_verts <- list()
  n_bounds <- 1L

  for (i in seq_len(nrois)) {
    current_roi <- roi_ids[i]
    if (verbose && i %% 25 == 1) message("Processing ROI ", i, "/", nrois, " (id=", current_roi, ")")

    # Edges that connect two vertices of this ROI and belong to a boundary face
    idx <- edges_by_roi[[as.character(current_roi)]]
    if (!length(idx)) {
      roi_components[i] <- 0L
      next
    }

    idx <- idx[unique_edges$roi_v2[idx] == current_roi & unique_edges$boundary_face[idx] == 1L]

    if (!length(idx)) {
      roi_components[i] <- 0L
      next
    }

    subset_edges <- unique_edges[idx, ]

    edge_list <- cbind(subset_edges$v1, subset_edges$v2)
    g <- igraph::graph_from_edgelist(as.matrix(edge_list), directed = FALSE)
    deg <- igraph::degree(g)

    comps <- igraph::components(g)
    roi_components[i] <- comps$no

    for (j in seq_len(comps$no)) {
      comp_nodes <- igraph::V(g)[comps$membership == j]
      deg2_nodes <- comp_nodes[deg[comp_nodes] == 2L]

      if (!length(deg2_nodes)) {
        next
      }

      comp_ids <- as.integer(comp_nodes)

      # Fast path: simple cycle (every vertex degree==2)
      if (all(deg[comp_nodes] == 2L)) {
        start_id <- comp_ids[1]
        path <- integer(0L)
        current <- start_id
        prev <- NA_integer_
        max_steps <- length(comp_ids) + 2L
        step <- 0L
        repeat {
          if (is.na(current)) break
          step <- step + 1L
          path <- c(path, current)
          nbrs <- as.integer(igraph::neighbors(g, current))
          nbrs <- nbrs[!is.na(nbrs)]
          next_candidates <- if (is.na(prev)) nbrs else nbrs[nbrs != prev]
          if (!length(next_candidates) || step >= max_steps) break
          next_node <- next_candidates[1]
          prev <- current
          current <- next_node
          if (!is.na(current) && current == start_id) break
        }
        if (length(path) && path[length(path)] != start_id) {
          v_seq <- c(path, start_id)
        } else {
          v_seq <- path
        }
      } else {
        start_node <- deg2_nodes[1]
        start_id <- as.integer(start_node)
        neigh <- igraph::neighbors(g, start_node)
        if (!length(neigh)) {
          next
        }
        end_node <- neigh[1]
        end_id <- as.integer(end_node)

        # Remove one edge to break the cycle
        edge_id <- igraph::get.edge.ids(g, c(start_id, end_id))
        g_temp <- igraph::delete_edges(g, edge_id)

        sp <- suppressWarnings(
          igraph::shortest_paths(g_temp,
                                 from = start_id,
                                 to   = end_id,
                                 output = "vpath")
        )
        vpath <- sp$vpath[[1]]
        if (!length(vpath)) {
          next
        }

        # Close loop by returning to the start node
        v_seq <- c(as.integer(vpath), as.integer(start_node))
      }

      coords <- vertices[v_seq, , drop = FALSE]

      boundaries[[n_bounds]] <- coords
      boundary_roi_id[n_bounds] <- current_roi
      boundary_verts[[n_bounds]] <- v_seq
      n_bounds <- n_bounds + 1L
    }
  }

  # Recompute component counts directly from detected loops to ensure consistency
  if (length(boundary_roi_id)) {
    roi_components <- tabulate(match(boundary_roi_id, roi_ids),
                               nbins = length(roi_ids))
  } else {
    roi_components <- integer(length(roi_ids))
  }

  list(
    boundary = boundaries,
    boundary_roi_id = boundary_roi_id,
    roi_components = roi_components,
    boundary_verts = boundary_verts
  )
}


#' Find boundaries for a \code{NeuroSurface} object
#'
#' @param x A \code{\linkS4class{NeuroSurface}} object whose \code{data} slot
#'   contains integer ROI labels for each vertex.
#' @param method Boundary method passed to \code{\link{find_roi_boundaries}}.
#'   Currently one of \code{"faces"} or \code{"edge_vertices"}.
#' @param ... Additional arguments passed to \code{\link{find_roi_boundaries}}.
#'
#' @return A list as returned by \code{\link{find_roi_boundaries}}.
#'
#' @seealso \code{\link{find_roi_boundaries}}
#' @rdname findBoundaries-methods
#' @export
setMethod(
  f = "findBoundaries",
  signature = "NeuroSurface",
  definition = function(x, method = c("edge_vertices", "faces"), ...) {
    method <- match.arg(method)

    geom <- x@geometry
    if (is.null(geom) || !inherits(geom, "SurfaceGeometry")) {
      stop("Invalid or missing geometry in NeuroSurface object.")
    }

    mesh <- geom@mesh
    if (is.null(mesh)) {
      stop("Missing mesh in SurfaceGeometry object.")
    }

    verts <- t(mesh$vb[1:3, , drop = FALSE])
    faces <- t(mesh$it)

    region_ids <- x@data
    if (length(region_ids) != nrow(verts)) {
      stop("Length of x@data must match number of vertices in geometry.")
    }

    if (!all(region_ids == as.integer(region_ids))) {
      region_ids <- as.integer(region_ids)
    }

    if (length(unique(region_ids)) < 2L) {
      warning("Only one region present; no boundaries to detect.")
      return(list(
        boundary = list(),
        boundary_roi_id = integer(0L),
        roi_components = integer(0L),
        boundary_verts = list()
      ))
    }

    find_roi_boundaries(
      vertices = verts,
      faces = faces,
      vertex_id = region_ids,
      boundary_method = method,
      ...
    )
  }
)
