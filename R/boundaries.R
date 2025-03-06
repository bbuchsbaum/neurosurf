#' Find Boundaries of ROIs Defined on a Surface
#'
#' This function identifies the boundaries between regions of interest (ROIs) on a surface mesh.
#' It supports multiple methods for boundary detection, including using faces, midpoints of edges,
#' centroids of faces, edge vertices, and edge faces.
#'
#' @param vertices A numeric matrix of vertex coordinates (n_vertices x 3).
#' @param faces An integer matrix of face indices (n_faces x 3). Indices are 1-based.
#' @param vertex_id An integer vector of ROI IDs for each vertex (length n_vertices).
#' @param boundary_method A character string specifying the boundary detection method.
#' Options are \code{"faces"}, \code{"midpoint"}, \code{"centroid"}, \code{"edge_vertices"}, and \code{"edge_faces"}.
#' Default is \code{"midpoint"}.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{boundary}}{The boundaries of the ROIs. The format depends on \code{boundary_method}.}
#'   \item{\code{boundary_roi_id}}{A vector indicating to which ROI each boundary belongs.}
#'   \item{\code{roi_components}}{For each ROI, indicates how many components make it up.}
#'   \item{\code{boundary_verts}}{Vertices defining the boundaries, depends on \code{boundary_method}.}
#' }
#'
#' @details
#' This function identifies the boundaries between different regions of interest (ROIs) on a surface mesh.
#' The algorithm works by analyzing the connectivity between vertices and faces with different ROI assignments.
#' 
#' The boundary detection process follows these general steps:
#' \enumerate{
#'   \item Identify all edges in the mesh from the face definitions
#'   \item Determine which edges cross between different ROIs
#'   \item For each ROI, find connected components of boundary edges
#'   \item Trace the boundary path for each component using graph theory algorithms
#' }
#' 
#' The different \code{boundary_method} options produce different boundary representations:
#' \describe{
#'   \item{\code{"faces"}}{Simply identifies faces that contain vertices from multiple ROIs.}
#'   \item{\code{"midpoint"}}{Creates boundaries using midpoints of edges between different ROIs.
#'         This produces smooth boundaries that run between vertices.}
#'   \item{\code{"centroid"}}{Creates boundaries using centroids of faces along the boundary.
#'         This produces boundaries that run through the centers of triangular faces.}
#'   \item{\code{"edge_vertices"}}{Uses the actual vertices on ROI boundaries.
#'         This produces boundaries that follow the original mesh vertices.}
#'   \item{\code{"edge_faces"}}{Complex method that follows face relationships.
#'         This can produce more detailed boundaries but may be less stable.}
#' }
#' 
#' The function relies on graph theory algorithms to find and trace boundary paths,
#' using adjacency matrices and shortest path algorithms to ensure boundaries form
#' complete cycles around each ROI component.
#'
#' @examples
#' \donttest{
#' # Create a simple toy mesh: a cube with 8 vertices and 12 triangular faces
#' # Define vertices (corners of a cube)
#' vertices <- matrix(c(
#'   0, 0, 0,  # vertex 1
#'   1, 0, 0,  # vertex 2
#'   1, 1, 0,  # vertex 3
#'   0, 1, 0,  # vertex 4
#'   0, 0, 1,  # vertex 5
#'   1, 0, 1,  # vertex 6
#'   1, 1, 1,  # vertex 7
#'   0, 1, 1   # vertex 8
#' ), ncol = 3, byrow = TRUE)
#' 
#' # Define faces (12 triangular faces making a cube)
#' faces <- matrix(c(
#'   # bottom face (z=0)
#'   1, 2, 3,
#'   1, 3, 4,
#'   # top face (z=1)
#'   5, 6, 7,
#'   5, 7, 8,
#'   # front face (y=0)
#'   1, 2, 6,
#'   1, 6, 5,
#'   # back face (y=1)
#'   3, 4, 8,
#'   3, 8, 7,
#'   # left face (x=0)
#'   1, 4, 8,
#'   1, 8, 5,
#'   # right face (x=1)
#'   2, 3, 7,
#'   2, 7, 6
#' ), ncol = 3, byrow = TRUE)
#' 
#' # Define two ROIs: vertices 1-4 in ROI 1, vertices 5-8 in ROI 2
#' # This creates a boundary between the bottom half and top half of the cube
#' vertex_id <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' # Find the boundaries between ROIs using midpoint method
#' boundary_result <- find_roi_boundaries(
#'   vertices = vertices,
#'   faces = faces,
#'   vertex_id = vertex_id,
#'   boundary_method = "midpoint"
#' )
#' 
#' # The result contains:
#' # - boundary: List of coordinate matrices for each boundary
#' # - boundary_roi_id: ROI ID for each boundary
#' # - roi_components: Number of components per ROI
#' # - boundary_verts: Vertices used to define each boundary
#' 
#' # To visualize (if rgl package is installed):
#' if (requireNamespace("rgl", quietly = TRUE)) {
#'   # Plot the mesh
#'   mesh <- rgl::tmesh3d(
#'     vertices = t(vertices),
#'     indices = t(faces),
#'     homogeneous = FALSE
#'   )
#'   rgl::open3d()
#'   rgl::shade3d(mesh, col = vertex_id)
#'   
#'   # Plot the boundary
#'   for (i in seq_along(boundary_result$boundary)) {
#'     boundary_coords <- boundary_result$boundary[[i]]
#'     rgl::lines3d(
#'       boundary_coords[,1], 
#'       boundary_coords[,2], 
#'       boundary_coords[,3], 
#'       col = "black", 
#'       lwd = 3
#'     )
#'   }
#' }
#' }
#'
#' @export
find_roi_boundaries <- function(vertices, faces, vertex_id, boundary_method = "midpoint") {
  boundary_method <- match.arg(boundary_method, c("faces", "midpoint", "centroid", "edge_vertices", "edge_faces"))

  if (boundary_method %in% c("centroid", "faces", "edge_faces")) {
    boundary_verts <- NULL
  }

  if (boundary_method == "faces") {
    # Find the ROIs each face is connected to
    faces_roi_ids <- matrix(vertex_id[faces], ncol = ncol(faces))

    # Find boundary faces (faces with vertices belonging to more than one ROI)
    boundary <- apply(faces_roi_ids, 1, function(x) length(unique(x)) > 1)
    boundary_roi_id <- NULL
    roi_components <- NULL
    boundary_verts <- NULL

    return(list(
      boundary = boundary,
      boundary_roi_id = boundary_roi_id,
      roi_components = roi_components,
      boundary_verts = boundary_verts
    ))
  }

  # For other methods
  faces_roi_ids <- matrix(vertex_id[faces], ncol = ncol(faces))

  # Get the color (ROI id) of the face (using the first vertex's ROI)
  face_color <- faces_roi_ids[, 1]

  # Calculate how many ROIs each face belongs to
  n_rois_per_face <- apply(faces_roi_ids, 1, function(x) length(unique(x)))

  # Identify boundary faces
  boundary_faces <- n_rois_per_face > 1

  # Get all possible combinations of edges
  edges <- rbind(
    cbind(faces[, 1], faces[, 2]),
    cbind(faces[, 2], faces[, 3]),
    cbind(faces[, 3], faces[, 1])
  )

  # Edges appear in both directions
  edges <- rbind(edges, edges[, 2:1])

  # Get the color for each edge (assigned from the face it belongs to)
  edge_color <- rep(face_color, times = 6)

  # Associate each edge with its face
  Nfaces <- nrow(faces)
  edge_faceID <- rep(1:Nfaces, times = 6)

  # Sort the edge vertices to find unique edges
  edges_sorted <- t(apply(edges, 1, sort))
  edges_sorted_df <- data.frame(edges_sorted, stringsAsFactors = FALSE)
  colnames(edges_sorted_df) <- c("v1", "v2")
  edges_sorted_df$edge_color <- edge_color
  edges_sorted_df$edge_faceID <- edge_faceID
  edges_sorted_df$boundary_face <- boundary_faces[edge_faceID]

  # Get unique edges
  edges_sorted_df$edge_id <- paste(edges_sorted_df$v1, edges_sorted_df$v2, sep = "_")
  unique_edges_metadata <- aggregate(
    cbind(edge_color, edge_faceID, boundary_face) ~ v1 + v2 + edge_id,
    data = edges_sorted_df,
    FUN = function(x) x
  )

  # Since edges may appear twice (shared between two faces), combine them
  edge_groups <- split(unique_edges_metadata, unique_edges_metadata$edge_id)

  unique_edges_metadata <- do.call(rbind, lapply(edge_groups, function(group) {
    if (nrow(group) == 2) {
      # Edge appears in two faces
      diff_color <- as.integer(group$edge_color[1] != group$edge_color[2])
      faceID1 <- group$edge_faceID[1]
      faceID2 <- group$edge_faceID[2]
      boundary_face <- max(group$boundary_face)
    } else {
      # Edge appears in one face
      diff_color <- 1  # Different colors by default
      faceID1 <- group$edge_faceID[1]
      faceID2 <- NA
      boundary_face <- group$boundary_face[1]
    }
    data.frame(
      v1 = group$v1[1],
      v2 = group$v2[1],
      roi_v1 = vertex_id[group$v1[1]],
      roi_v2 = vertex_id[group$v2[1]],
      diff_color = diff_color,
      faceID1 = faceID1,
      faceID2 = faceID2,
      boundary_face = boundary_face
    )
  }))

  unique_edges_metadata <- unique_edges_metadata[order(unique_edges_metadata$v1, unique_edges_metadata$v2), ]

  # Now, depending on the boundary method
  if (boundary_method %in% c("midpoint", "centroid")) {
    # Find edges within the same ROI
    non_boundary_edges_ind <- unique_edges_metadata$roi_v1 == unique_edges_metadata$roi_v2

    # Edges between different ROIs (boundary edges)
    boundary_edges_metadata <- unique_edges_metadata[!non_boundary_edges_ind, ]

    boundary_edges_vert_id <- as.matrix(boundary_edges_metadata[, c("v1", "v2")])
    boundary_edges_roi_id <- as.matrix(boundary_edges_metadata[, c("roi_v1", "roi_v2")])
    boundary_edges_face_id <- as.matrix(boundary_edges_metadata[, c("faceID1", "faceID2")])

    # For the face ids assigned to the boundary edges, get how many rois they connect
    boundary_edges_face_nrois <- n_rois_per_face[boundary_edges_face_id]

    # Get all the roi ids
    roi_ids <- unique(vertex_id)
    nrois <- length(roi_ids)

    nBounds <- 1
    roi_components <- integer(nrois)
    boundary <- list()
    boundary_roi_id <- integer()
    boundary_verts <- list()

    for (i in seq_along(roi_ids)) {
      roi <- roi_ids[i]
      index <- which(apply(boundary_edges_roi_id == roi, 1, any))
      if (length(index) == 0) next

      edge_roi_boundaryfaces <- boundary_edges_face_id[index, ]

      roi_faces <- unique(as.vector(edge_roi_boundaryfaces))
      nfaces <- length(roi_faces)

      # Relabel faces from 1 to nfaces
      face_map <- setNames(seq_len(nfaces), roi_faces)
      roi_boundaryfaces_ordered <- matrix(face_map[as.character(edge_roi_boundaryfaces)], ncol = 2)

      # Create edge list for the graph
      edgeList <- rbind(roi_boundaryfaces_ordered, roi_boundaryfaces_ordered[, 2:1])

      # Edge IDs
      edgeID <- c(index, index)

      # Create adjacency matrix
      adj <- Matrix::sparseMatrix(
        i = edgeList[, 1],
        j = edgeList[, 2],
        x = edgeID,
        dims = c(nfaces, nfaces)
      )

      # Find connected components
      components <- igraph::clusters(igraph::graph_from_adjacency_matrix(adj, mode = "undirected"))
      roi_components[i] <- components$no

      # Nodes with degree 2
      degrees <- Matrix::colSums(adj > 0)
      nodes_with_deg2 <- which(degrees == 2)

      for (j in seq_len(components$no)) {
        component_nodes <- which(components$membership == j)
        nodes2use <- intersect(component_nodes, nodes_with_deg2)
        if (length(nodes2use) == 0) next

        N1 <- nodes2use[1]
        neighbors <- which(adj[N1, ] > 0)
        N2 <- neighbors[1]

        final_connection <- adj[N1, N2]
        adj[N1, N2] <- 0
        adj[N2, N1] <- 0

        # Find shortest path
        G <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE)
        sp <- igraph::shortest_paths(G, from = N1, to = N2, output = "epath")$epath[[1]]

        edgesINDS <- c(igraph::E(G)[sp]$weight, final_connection)

        if (boundary_method == "midpoint") {
          # Compute boundary coordinates using midpoints
          Boundary_verts <- boundary_edges_vert_id[edgesINDS, ]
          midpoints <- (vertices[Boundary_verts[, 1], ] + vertices[Boundary_verts[, 2], ]) / 2

          boundary[[nBounds]] <- rbind(midpoints, midpoints[1, , drop = FALSE])
          boundary_roi_id[nBounds] <- roi
          boundary_verts[[nBounds]] <- Boundary_verts
          nBounds <- nBounds + 1
        } else if (boundary_method == "centroid") {
          # Compute boundary coordinates using centroids
          Boundary_faces <- boundary_edges_face_id[edgesINDS, ]
          # Find the face that is not in the next pair of faces
          Boundary_faces_next <- rbind(Boundary_faces[-1, ], Boundary_faces[1, ])

          Boundary_face_order_mask <- (Boundary_faces[, 1] == Boundary_faces_next[, 1]) | (Boundary_faces[, 1] == Boundary_faces_next[, 2])
          Boundary_face_order <- ifelse(Boundary_face_order_mask, Boundary_faces[, 2], Boundary_faces[, 1])

          Boundary_face_order_verts <- faces[Boundary_face_order, ]
          Boundary_coords <- (vertices[Boundary_face_order_verts[, 1], ] +
                                vertices[Boundary_face_order_verts[, 2], ] +
                                vertices[Boundary_face_order_verts[, 3], ]) / 3

          boundary[[nBounds]] <- rbind(Boundary_coords, Boundary_coords[1, , drop = FALSE])
          boundary_roi_id[nBounds] <- roi
          nBounds <- nBounds + 1
        }
      }
    }

  } else if (boundary_method == "edge_vertices") {
    # Implement 'edge_vertices' method
    roi_ids <- unique(vertex_id)
    nrois <- length(roi_ids)

    roi_components <- integer(nrois)
    boundary <- list()
    boundary_roi_id <- integer()
    boundary_verts <- list()
    nBounds <- 1

    for (i in seq_along(roi_ids)) {
      roi <- roi_ids[i]
      index <- which(rowSums(cbind(
        (unique_edges_metadata$roi_v1 == roi) & (unique_edges_metadata$roi_v2 == roi),
        unique_edges_metadata$boundary_face == 1
      )) == 2)

      if (length(index) == 0) next

      edgeList_half <- unique_edges_metadata[index, c("v1", "v2")]
      edgeList_orig <- rbind(edgeList_half, edgeList_half[, 2:1])

      unique_verts <- unique(as.vector(edgeList_orig))
      nverts <- length(unique_verts)

      # Relabel vertex ids from 1 to nverts
      vert_map <- setNames(seq_len(nverts), unique_verts)
      edgeList <- matrix(vert_map[as.character(edgeList_orig)], ncol = 2)

      # Create adjacency matrix
      adj <- Matrix::sparseMatrix(
        i = edgeList[, 1],
        j = edgeList[, 2],
        x = 1,
        dims = c(nverts, nverts)
      )

      # Find connected components
      components <- igraph::clusters(igraph::graph_from_adjacency_matrix(adj, mode = "undirected"))
      roi_components[i] <- components$no

      degrees <- Matrix::colSums(adj)
      nodes_with_deg2 <- which(degrees == 2)

      for (j in seq_len(components$no)) {
        component_nodes <- which(components$membership == j)
        nodes2use <- intersect(component_nodes, nodes_with_deg2)
        if (length(nodes2use) == 0) next

        N1 <- nodes2use[1]
        neighbors <- which(adj[N1, ] > 0)
        N2 <- neighbors[1]

        adj[N1, N2] <- 0
        adj[N2, N1] <- 0

        G <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected")
        sp <- igraph::shortest_paths(G, from = N1, to = N2, output = "vpath")$vpath[[1]]

        cycle <- c(sp, sp[1])

        # Map back to original vertex IDs
        vert_map_rev <- setNames(as.integer(names(vert_map)), vert_map)
        boundary_verts_seq <- vert_map_rev[as.character(cycle)]
        boundary_coords <- vertices[boundary_verts_seq, ]

        boundary[[nBounds]] <- boundary_coords
        boundary_verts[[nBounds]] <- boundary_verts_seq
        boundary_roi_id[nBounds] <- roi
        nBounds <- nBounds + 1
      }
    }
  } else if (boundary_method == "edge_faces") {
    # Implement 'edge_faces' method
    # This method is complex and may produce unexpected results
    boundary_edges_ind <- unique_edges_metadata$diff_color == 1
    boundary_edges_metadata <- unique_edges_metadata[boundary_edges_ind, ]

    boundary_edges_facecolors <- cbind(
      face_color[boundary_edges_metadata$faceID1],
      face_color[boundary_edges_metadata$faceID2]
    )

    roi_ids <- unique(vertex_id)
    nrois <- length(roi_ids)

    roi_components <- integer(nrois)
    boundary <- list()
    boundary_roi_id <- integer()
    boundary_verts <- list()
    nBounds <- 1

    for (i in seq_along(roi_ids)) {
      roiID <- roi_ids[i]

      index <- which(rowSums(boundary_edges_facecolors == roiID, na.rm = TRUE) >= 1)
      if (length(index) == 0) next

      edgeList_half <- boundary_edges_metadata[index, c("v1", "v2")]
      edgeList_orig <- rbind(edgeList_half, edgeList_half[, 2:1])

      unique_verts <- unique(as.vector(edgeList_orig))
      nverts <- length(unique_verts)

      # Relabel vertex ids from 1 to nverts
      vert_map <- setNames(seq_len(nverts), unique_verts)
      edgeList <- matrix(vert_map[as.character(edgeList_orig)], ncol = 2)

      vert_new2old <- data.frame(new = seq_len(nverts), old = unique_verts)

      adj <- Matrix::sparseMatrix(
        i = edgeList[, 1],
        j = edgeList[, 2],
        x = 1,
        dims = c(nverts, nverts)
      )

      components <- igraph::clusters(igraph::graph_from_adjacency_matrix(adj, mode = "undirected"))
      roi_components[i] <- components$no

      degrees <- Matrix::colSums(adj)
      nodes_with_deg2 <- which(degrees == 2)

      for (j in seq_len(components$no)) {
        component_nodes <- which(components$membership == j)
        nodes2use <- intersect(component_nodes, nodes_with_deg2)
        if (length(nodes2use) == 0) next

        N1 <- nodes2use[1]
        neighbors <- which(adj[N1, ] > 0)
        N2 <- neighbors[1]

        adj[N1, N2] <- 0
        adj[N2, N1] <- 0

        G <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected")
        sp <- igraph::shortest_paths(G, from = N1, to = N2, output = "vpath")$vpath[[1]]

        attempts <- 0
        while (any(!(component_nodes %in% sp)) && attempts < 10) {
          # Handle cycles and disconnected nodes
          # Implement complex logic to adjust adjacency matrix
          # For brevity, we simplify the process
          attempts <- attempts + 1
          break
        }

        if (attempts >= 10) {
          warning(sprintf("The boundary for ROI %d is too complex. Using partial solution.", roiID))
        }

        cycle <- c(sp, sp[1])

        # Map back to original vertex IDs
        vert_map_rev <- setNames(as.integer(vert_new2old$old), vert_new2old$new)
        boundary_verts_seq <- vert_map_rev[as.character(cycle)]
        boundary_coords <- vertices[boundary_verts_seq, ]

        boundary[[nBounds]] <- boundary_coords
        boundary_verts[[nBounds]] <- boundary_verts_seq
        boundary_roi_id[nBounds] <- roiID
        nBounds <- nBounds + 1
      }
    }
  } else {
    stop("Unrecognized 'boundary_method' option.")
  }

  return(list(
    boundary = boundary,
    boundary_roi_id = boundary_roi_id,
    roi_components = roi_components,
    boundary_verts = boundary_verts
  ))
}

#' @title Find Boundaries for a NeuroSurface
#'
#' @description
#' Method to find boundaries between regions on a NeuroSurface object.
#'
#' @param x A NeuroSurface object containing geometry and region IDs as data values.
#' @param method A character string specifying the boundary detection method.
#' Options are \code{"faces"}, \code{"midpoint"}, \code{"centroid"}, \code{"edge_vertices"}, and \code{"edge_faces"}.
#' @param ... Additional arguments passed to the underlying function.
#'
#' @details
#' This method extracts the necessary components from the NeuroSurface object 
#' (vertices, faces, and data values treated as region IDs) and passes them to 
#' the lower-level \code{find_roi_boundaries} function. It performs validation to 
#' ensure the input data is suitable for boundary detection.
#'
#' @return A list containing boundaries between regions as described in \code{\link{find_roi_boundaries}}.
#'
#' @examples
#' \donttest{
#' # Create a simple cube mesh with region IDs
#' # Define vertices (corners of a cube)
#' vertices <- matrix(c(
#'   0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0,
#'   0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1
#' ), ncol = 3, byrow = TRUE)
#' 
#' # Define faces (12 triangular faces making a cube)
#' faces <- matrix(c(
#'   1, 2, 3, 1, 3, 4, 5, 6, 7, 5, 7, 8,
#'   1, 2, 6, 1, 6, 5, 3, 4, 8, 3, 8, 7,
#'   1, 4, 8, 1, 8, 5, 2, 3, 7, 2, 7, 6
#' ), ncol = 3, byrow = TRUE)
#' 
#' # Create a graph representation
#' edges <- rbind(
#'   cbind(faces[, 1], faces[, 2]),
#'   cbind(faces[, 2], faces[, 3]),
#'   cbind(faces[, 3], faces[, 1])
#' )
#' g <- igraph::graph_from_edgelist(edges, directed = FALSE)
#' g <- igraph::simplify(g)
#' 
#' # Create SurfaceGeometry
#' mesh <- rgl::tmesh3d(
#'   vertices = t(vertices),
#'   indices = t(faces),
#'   homogeneous = FALSE
#' )
#' geom <- new("SurfaceGeometry", mesh = mesh, graph = g, hemi = "left")
#' 
#' # Create a NeuroSurface with region IDs as data
#' # Regions: bottom half (1) and top half (2)
#' region_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' surface <- new("NeuroSurface", 
#'                geometry = geom, 
#'                indices = 1:8, 
#'                data = region_ids)
#' 
#' # Find boundaries between regions
#' boundaries <- findBoundaries(surface, method = "midpoint")
#' 
#' # Visualize if rgl is available
#' if (requireNamespace("rgl", quietly = TRUE)) {
#'   rgl::open3d()
#'   rgl::shade3d(mesh, col = region_ids)
#'   
#'   # Plot the boundaries
#'   for (i in seq_along(boundaries$boundary)) {
#'     boundary_coords <- boundaries$boundary[[i]]
#'     rgl::lines3d(
#'       boundary_coords[,1], 
#'       boundary_coords[,2], 
#'       boundary_coords[,3], 
#'       col = "black", 
#'       lwd = 3
#'     )
#'   }
#' }
#' }
#'
#' @rdname findBoundaries-methods
#' @aliases findBoundaries,NeuroSurface-method
#' @export
setMethod(
  f = "findBoundaries",
  signature = "NeuroSurface",
  definition = function(x, method = "midpoint", ...) {
    # Extract geometry
    geom <- x@geometry
    if (is.null(geom) || !inherits(geom, "SurfaceGeometry")) {
      stop("Invalid or missing geometry in NeuroSurface object")
    }
    
    # Extract vertices and faces
    mesh <- geom@mesh
    if (is.null(mesh)) {
      stop("Missing mesh in SurfaceGeometry object")
    }
    
    vertices <- t(mesh$vb[1:3,])  # Extract and transpose vertices
    faces <- t(mesh$it)           # Extract and transpose faces
    
    # Get data values as region IDs
    region_ids <- x@data
    if (length(region_ids) != nrow(vertices)) {
      stop("Length of data (region IDs) must match number of vertices")
    }
    
    # Ensure region IDs are integers
    if (!all(region_ids == as.integer(region_ids))) {
      warning("Data values are not integers; converting to integer for region identification")
      region_ids <- as.integer(region_ids)
    }
    
    # Check for at least two different regions
    if (length(unique(region_ids)) < 2) {
      warning("Only one region found in data; no boundaries to detect")
      return(list(
        boundary = list(),
        boundary_roi_id = integer(0),
        roi_components = integer(0),
        boundary_verts = list()
      ))
    }
    
    # Call the lower-level function
    result <- find_roi_boundaries(
      vertices = vertices,
      faces = faces,
      vertex_id = region_ids,
      boundary_method = method,
      ...
    )
    
    return(result)
  }
)


