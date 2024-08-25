#'
#' ## experimental
#' #' @noRd
#' findROIboundaries <- function(vertices, faces, vertex_id, boundary_method = "midpoint") {
#'   BOUNDARY <- list()
#'   BOUNDARY_ROI_ID <- integer(0)
#'   ROI_COMPONENTS <- integer(0)
#'   BOUNDARY_VERTS <- list()
#'
#'   faces_roi_ids <- array(vertex_id[faces], dim(faces))
#'   face_color <- faces_roi_ids[, 1]
#'   n_rois_per_face <- apply(faces_roi_ids, 1, function(x) length(unique(x)))
#'
#'     #rowSums((faces_roi_ids[sort.list(faces_roi_ids),] != 0) * 1, 2) + 1
#'   boundary_faces <- !apply(faces_roi_ids, 1, function(x) all(diff(x) == 0))
#'
#'   #boundary_faces <- logical(apply(diff(faces_roi_ids, 2), 1, any))
#'   edges <- rbind(cbind(faces[, 1], faces[, 3]),
#'                  cbind(faces[, 1], faces[, 2]),
#'                  cbind(faces[, 2], faces[, 1]),
#'                  cbind(faces[, 2], faces[, 3]),
#'                  cbind(faces[, 3], faces[, 2]),
#'                  cbind(faces[, 3], faces[, 1]))
#'
#'
#'   # Get the color for each edge (an edge is assigned the color
#'   # of the face it is assigned to, each edge therefore has two
#'   # colors)
#'   edge_color <- rep(face_color, each = 6)
#'
#'   # Get the number of faces
#'   Nfaces <- nrow(faces)
#'
#'   # Make an index to find which edge corresponds to each face
#'   edge_faceID <- rep(1:Nfaces, each = 6)
#'
#'   # Sort the edge vertices from low to high, allows unique edges
#'   # to be found
#'   edges_sorted <- edges[order(edges[, 1], edges[, 2]), ]
#'
#'   # Extract all of the edges metadata into one matrix.
#'   # edges_metadata is a dataframe with the following columns:
#'   # 1:2 are the IDs of the vertices making up that edge
#'   # 3:4 are the vertex ROI ids for the vertices that make up the edge
#'   # 5 is the color of the face that edge is part of
#'   # 6 is the face ID of the faces the edge is part of
#'   # 7 equals 1 if that edge is part of a boundary face
#'   edges_metadata <- data.frame(edges_sorted,
#'                                vertex_id[edges_sorted[,1]],
#'                                vertex_id[edges_sorted[,2]],
#'                                edge_color[edge_faceID],
#'                                edge_faceID,
#'                                as.integer(boundary_faces[edge_faceID]))
#'
#'   # Get the unique data for each edge. Each edge is assigned to
#'   # two faces so will appear twice in this list
#'   edges_unique <- unique(edges_metadata)
#'
#'   # Because edges_unique is sorted such that the data for
#'   # each edge is a pair, we can split
#'   edges_unique1 <- edges_unique[seq(1, nrow(edges_unique), by = 2), ]
#'   edges_unique2 <- edges_unique[seq(2, nrow(edges_unique), by = 2), ]
#'
#'   # Extract all of the unique edges metadata into one matrix.
#'   # unique_edges_metadata is a dataframe with the following columns:
#'   # 1:2 are the IDs of the vertices making up that edge
#'   # 3:4 are the vertex ROI ids for the vertices that make up the edge
#'   # 5 indicates if that edge belongs to two faces of the same color
#'   # 6:7 are the face IDs of the faces the edge is part of
#'   # 8 equals 1 if that edge is part of a boundary face
#'   unique_edges_metadata <- data.frame(edges_unique1[, 1:4],
#'                                       as.integer(edges_unique1[, 5] != edges_unique2[, 5]),
#'                                       edges_unique1[, 6],
#'                                       edges_unique2[, 6],
#'                                       pmax(edges_unique1[, 7], edges_unique2[, 7]))
#'
#'
#'   # Find edges which are contained within the one roi
#'   non_boundary_edges_ind <- unique_edges_metadata[, 3] == unique_edges_metadata[, 4]
#'
#'   # Find edges which lie between two rois (boundary edges)
#'   boundary_edges_vert_id <- unique_edges_metadata[!non_boundary_edges_ind, 1:2]
#'
#'   # Get the associated roi ids for the boundary edges
#'   boundary_edges_roi_id <- unique_edges_metadata[!non_boundary_edges_ind, 3:4]
#'
#'   # Get the face ids for the boundary edges
#'   boundary_edges_face_id <- unique_edges_metadata[!non_boundary_edges_ind, 6:7]
#'
#'   # For the face ids assigned to the boundary edges, get how many rois
#'   # they connect
#'   boundary_edges_face_nrois <- n_rois_per_face[boundary_edges_face_id]
#'
#'   # Get all the roi ids and the number of rois
#'   roi_ids <- unique(vertex_id)
#'   nrois <- length(roi_ids)
#'
#'   # Potentially a roi is not spatially contiguous, so will have multiple
#'   # boundaries. The code will iteratively add in boundaries because of this
#'   nBounds <- 1
#'
#'   ROI_COMPONENTS <- numeric(nrois)
#'
#'   for (i in 1:nrois) {
#'     # Find the boundary edges for a given roi
#'     index <- which(any(boundary_edges_roi_id == roi_ids[i], 2))
#'
#'     # Get the face ids for the boundary edges for a given roi
#'     edge_roi_boundaryfaces <- boundary_edges_face_id[index, ]
#'
#'     # Get all the unique faces along the roi boundary and the number of
#'     # such faces
#'     roi_faces <- unique(edge_roi_boundaryfaces)
#'     nfaces <- length(roi_faces)
#'
#'     # Relabel the faces from 1:nfaces so the boundary of the roi can be
#'     # found easily using graph theory methods
#'     newval <- 1:nfaces
#'     oldval <- roi_faces
#'     roi_boundaryfaces_ordered <- edge_roi_boundaryfaces
#'
#'     for (k in 1:length(newval)) {
#'       roi_boundaryfaces_ordered[edge_roi_boundaryfaces == oldval[k]] <- newval[k]
#'     }
#'
#'     # Make an edge list using the relabeled faces as nodes. Each row
#'     # defines a pair of faces which are connected. The edge list needs
#'     # to be symmetric
#'     edgeList <- rbind(roi_boundaryfaces_ordered, roi_boundaryfaces_ordered[, 2:1])
#'
#'     # Get the id of each edge
#'     edgeID <- c(index, index)
#'
#'     # Make an adjacency matrix where each connection defines a pair of
#'     # faces which are adjacent, and the connection weight is the
#'     # corresponding edge id that connects those two faces
#'     adj <- Matrix(0, nfaces, nfaces, sparse = TRUE)
#'     adj[cbind(edgeList[, 1], edgeList[, 2])] <- edgeID
#'
#'     # The boundary of a ROI should form a circle. If for some reason the
#'     # roi is not spatially continuous, each of the subrois should form a
#'     # cycle. If not, something is very wrong with your parcellation
#'
#'     # Find the components of the adjacency matrix. Each component
#'     # represents a spatially continuous area for the current roi
#'     comp <- igraph::components(igraph::graph_from_adjacency_matrix(adj))
#'     ROI_COMPONENTS[i] <- comp$no
#'
#'     # Loop over each roi component (i.e., each boundary for a roi)
#'     NodesWithDeg2 <- colSums(adj > 0) == 2
#'   }
#'
#'   for (j in 1:ROI_COMPONENTS[i]) {
#'     # Find the "nodes" making up the component
#'     Nodes2Use <- which((comp$membership == j) & NodesWithDeg2)
#'
#'     N1 <- Nodes2Use[1]
#'
#'     # Find the neighbours of the "node"
#'     NodeNeighbors <- which(adj[N1, ] > 0)
#'
#'     # Pick one of those neighbours to be the end "node"
#'     N2 <- NodeNeighbors[1]
#'
#'     # Find the connection linking the start and end "nodes". Once
#'     # found, record it, then remove it from the adjacency matrix
#'     FinalConnection <- as.numeric(adj[N1, N2])
#'
#'     adj[N1, N2] <- 0
#'     adj[N2, N1] <- 0
#'
#'     # The boundary should be a cycle in the graph. If you remove
#'     # one connection between "node" N1 and N2, if you find the
#'     # shortest path between N1 and N2, you get the ordering of
#'     # faces which define the boundary. You can also get the order
#'     # of the connections traversed to make this path "E".
#'     G <- igraph::graph_from_adjacency_matrix(adj)
#'
#'     shortest_path_res <- igraph::shortest_paths(G, N1, N2, output = "epath")
#'
#'     # We know the connections traversed, so we use this index to
#'     # get the original edge ids which make up the boundary. To make
#'     # the boundary complete, we add in the one edge we originally
#'     # removed. Each edge defines a coordinate.
#'     edgesINDS <- c(sapply(shortest_path_res$epath[[1]], function(x) G$weight[x]), FinalConnection)
#'
#'   if (boundary_method == "midpoint")
#'
#'     DoSimple <- 0
#'
#'     if (DoSimple) {
#'       Boundary_verts <- boundary_edges_vert_id[edgesINDS, ]
#'       midpoints <- (vertices[Boundary_verts[, 1], ] + vertices[Boundary_verts[, 2], ]) / 2
#'       BOUNDARY[[nBounds]] <- rbind(midpoints, midpoints[1, ])
#'       nBounds <- nBounds + 1
#'     } else {
#'       # For the rois boundary edges, get the number of rois the
#'       # corresponding faces are connected to
#'       Boundary_verts_nrois <- boundary_edges_face_nrois[edgesINDS, ]
#'
#'       # Find the "corners" (i.e. where three rois intersect) of the
#'       # boundary
#'       boundary_corner_points <- which(rowMaxs(Boundary_verts_nrois) == 3)
#'
#'       # Check there are corners for this boundary
#'       if (length(boundary_corner_points) > 0) {
#'         # Prevent the boundary from starting on a corner, this messes up
#'         # code later. The corners should come in pairs in EdgeINDS and
#'         # the code expects as such. If the first edge but not second
#'         # edge in a corner edge, this means the last edge is a corner
#'         # edge. If so we just reorder to matrix to add the starting
#'         # edge to the end
#'         if (boundary_corner_points[1] == 1 && boundary_corner_points[2] != 1) {
#'           EdgeINDS_1 <- edgesINDS[1]
#'           edgesINDS <- c(edgesINDS[-1], EdgeINDS_1)
#'           Boundary_verts_nrois <- boundary_edges_face_nrois[edgesINDS, ]
#'           boundary_corner_points <- which(rowMaxs(Boundary_verts_nrois) == 3)
#'         }
#'
#'         # Find the corresponding faces of the rois boundary edge
#'         Boundary_faces <- boundary_edges_face_id[edgesINDS, ]
#'
#'         # Find the corresponding vertices of the rois boundary edge
#'         Boundary_verts <- boundary_edges_vert_id[edgesINDS, ]
#'
#'         # Calculate the midpoints of the boundary edges based on the
#'         # vertices which make up that edge
#'         midpoints <- (vertices[Boundary_verts[, 1], ] + vertices[Boundary_verts[, 2], ]) / 2
#'
#'         # Create a binary mask of which edges are corners
#'         Boundary_corner_faces_mask <- Boundary_verts_nrois == 3
#'
#'         # Find the faces which make up the corners
#'         corner_faces <- Boundary_faces
#'         corner_faces[!Boundary_corner_faces_mask] <- 0
#'         boundary_corner_faces <- rowMaxs(corner_faces[boundary_corner_points, ])
#'
#'         # Find the vertices of the corner faces
#'         boundary_corner_faces_verts <- faces[boundary_corner_faces, ]
#'
#'         # Get the centroid of the corner faces
#'         corner_coords <- (vertices[boundary_corner_faces_verts[, 3], ] + vertices[boundary_corner_faces_verts[, 2], ] + vertices[boundary_corner_faces_verts[, 1], ]) / 3
#'
#'         # Multiple corner faces may be adjacent. This detects which
#'         # are adjacent, selects the appropriate coordinates to use
#'         # and the appropriate spot to place them in. This assumes
#'         # for a pair of adjacent corners, you take the coordinates
#'         # of the second of the pair but use the first to get the
#'         # point to insert the coordinate
#'         corner_coords2use_ind <- which(diff(boundary_corner_points) == 1)
#'         corner_coords2use <- corner_coords[corner_coords2use_ind +  1, ]
#'         corner_coords_insert <- boundary_corner_points[corner_coords2use_ind]
#'
#'         # Create the matrix defining the final number of boundary
#'         # coordinates
#'         boundary_coords <- matrix(0, nrow = nrow(midpoints) + length(corner_coords2use), ncol = ncol(midpoints))
#'
#'         # Find indices for the non-corner boundary edges
#'         addRows <- is.element(1:nrow(midpoints), corner_coords_insert)
#'         oldDataInd <- (1:nrow(midpoints)) + cumsum(c(0, addRows[-length(addRows)]))
#'
#'         # Add in the non-corner boundary edges
#'         boundary_coords[oldDataInd, ] <- midpoints
#'
#'         # Find indices for the corner boundary edges
#'         newDataInd <- (1:length(corner_coords_insert)) + corner_coords_insert
#'
#'         # Add in the corner boundary edges
#'         boundary_coords[newDataInd, ] <- corner_coords2use
#'
#'       } else {
#'         # If there are no corners, just get the midpoints and use them
#'         Boundary_verts <- boundary_edges_vert_id[edgesINDS, ]
#'         boundary_coords <- (vertices[Boundary_verts[, 1], ] + vertices[Boundary_verts[, 2], ]) / 2
#'       }
#'
#'       # The edges define a set of coordinates, and the order of the
#'       # defines how these coordinates need to be connected to form
#'       # the boundary. However, the boundary needs to end where it
#'       # starts, so we make the first coordinate the final one as well
#'       BOUNDARY[[nBounds]] <- rbind(boundary_coords, boundary_coords[1, ])
#'
#'       # We don't know beforehand how many distinct ROI boundaries
#'       # there will be, so we update it as we go.
#'       BOUNDARY_ROI_ID[nBounds] <- roi_ids[i]
#'       nBounds <- nBounds + 1
#'     } else if (boundary_method == "centroid") {
#'       ## centroid
#'       # Find the pairs of faces making up the boundary
#'       Boundary_faces <- boundary_edges_face_id[edgesINDS, ]
#'
#'       # Find the face that is not in the next pair of faces. This
#'       # is the face we need to calculate the coordinates at that
#'       # step
#'       Boundary_faces_next <- rbind(Boundary_faces[-1, ], Boundary_faces[1, ])
#'
#'       Boundary_face_order_mask <- apply(Boundary_faces == Boundary_faces_next, 1, any)
#'
#'       Boundary_face_order_masked <- Boundary_faces
#'       Boundary_face_order_masked[cbind(1:nrow(Boundary_faces), c(Boundary_face_order_mask, !Boundary_face_order_mask))] <- 0
#'
#'       # Get the order of faces
#'       Boundary_face_order <- apply(Boundary_face_order_masked, 1, max)
#'
#'       # Get the vertex ids of those order faces
#'       Boundary_face_order_verts <- faces[Boundary_face_order, ]
#'
#'       # Get the face centroid to use as the coordinates for the
#'       # boundary
#'       Boundary_coords <- (vertices[Boundary_face_order_verts[, 3], ] + vertices[Boundary_face_order_verts[, 2], ] + vertices[Boundary_face_order_verts[, 1], ]) / 3
#'
#'       BOUNDARY[[nBounds]] <- rbind(Boundary_coords, Boundary_coords[1, ])
#'       BOUNDARY_ROI_ID[nBounds] <- i
#'
#'       nBounds <- nBounds + 1
#'     }
#'   }
#' }


