
#' SurfaceGeometrySource
#'
#' Constructor for SurfaceGeometrySource
#'
#' @param surface_name the name of the file containing the surface geometry.
#' @rdname SurfaceGeometrySource-class
#' @noRd
SurfaceGeometrySource <- function(surface_name) {
  stopifnot(is.character(surface_name))
  stopifnot(file.exists(surface_name))
  meta_info <- .readHeader(surface_name)
  new("SurfaceGeometrySource", meta_info=meta_info)
}

#' Create a SurfaceGeometry Object
#'
#' This function creates a new SurfaceGeometry object from vertex coordinates and face indices.
#'
#' @param vert A numeric matrix with 3 columns representing the x, y, and z coordinates of vertices.
#' @param faces An integer matrix where each row represents a face, containing indices of vertices that form the face.
#' @param hemi A character string indicating the hemisphere ("lh" for left, "rh" for right, or other identifier).
#'
#' @return A new object of class "SurfaceGeometry" containing:
#'   \item{mesh}{An rgl mesh object representing the surface.}
#'   \item{graph}{An igraph object representing the mesh connectivity.}
#'   \item{hemi}{The hemisphere identifier.}
#'
#' @details
#' This function constructs a SurfaceGeometry object by creating a mesh and a graph
#' representation of the surface. It uses the rgl package for 3D visualization and
#' the igraph package for graph operations.
#'
#' The vertex indices in the faces matrix should be 0-based (starting from 0),
#' as they get incremented by 1 when passed to the rgl mesh function.
#'
#' @examples
#' \donttest{
#' # Create a simple icosahedron-like mesh with 12 vertices
#' set.seed(123)
#' vertices <- matrix(rnorm(36), ncol=3)
#' 
#' # Create faces with 0-based indices (0 to 11)
#' # Each face connects three vertices
#' faces <- matrix(sample(0:11, 60, replace=TRUE), ncol=3)
#' 
#' # Create the SurfaceGeometry object
#' surf_geom <- SurfaceGeometry(vertices, faces, "lh")
#' 
#' # Visualize the mesh if rgl is available
#' if(requireNamespace("rgl", quietly = TRUE)) {
#'   rgl::open3d()
#'   rgl::shade3d(surf_geom@mesh, col="lightblue")
#' }
#' }
#'
#' @importFrom rgl tmesh3d
#' @export
SurfaceGeometry <- function(vert, faces, hemi) {
  graph <- meshToGraph(vert, faces)
  mesh <- rgl::tmesh3d(as.vector(t(vert)), as.vector(t(faces))+1, homogeneous=FALSE)
  new("SurfaceGeometry",  mesh=mesh, graph=graph, hemi=hemi)
}


#' Read Surface Geometry from File
#'
#' This function loads a supported surface geometry from a file and returns a SurfaceGeometry object.
#'
#' @param surface_name A character string specifying the name of the file containing the surface geometry.
#'
#' @return A \code{SurfaceGeometry} object representing the loaded surface.
#'
#' @details
#' This function supports loading surface geometries from the following file formats:
#' \itemize{
#'   \item FreeSurfer ASCII (.asc)
#'   \item FreeSurfer binary
#'   \item GIFTI (.gii)
#' }
#' The appropriate loader is automatically selected based on the file extension or content.
#'
#' @examples
#' \dontrun{
#' # Load a FreeSurfer surface
#' lh_surface <- read_surf_geometry("lh.pial")
#'
#' # Load a GIFTI surface
#' rh_surface <- read_surf_geometry("rh.white.gii")
#' }
#'
#' @seealso \code{\link{SurfaceGeometrySource}}, \code{\link{load_data}}
#'
#' @export
read_surf_geometry <- function(surface_name) {
  surf_source <- SurfaceGeometrySource(surface_name)
  load_data(surf_source)
}



#' @rdname curvature
#' @export
setMethod(f="curvature", signature=c(x="SurfaceGeometry"),
          def=function(x) {
            curv <- Rvcg::vcgCurve(x@mesh)
            vbmean <- normalize(curv$meanvb)
            vbmean
          })



#' Remesh a SurfaceGeometry object
#'
#' @description
#' This function applies uniform remeshing to a SurfaceGeometry object, 
#' creating a new mesh with more regular face sizes and improved quality.
#'
#' @param surfgeom A \code{SurfaceGeometry} object to be remeshed.
#' @param voxel_size Numeric value specifying the target edge length in the remeshed output.
#'   Smaller values create finer meshes with more faces.
#' @param ... Additional arguments to pass to \code{Rvcg::vcgUniformRemesh}.
#'
#' @return A new \code{SurfaceGeometry} object with the remeshed surface.
#'
#' @details
#' Remeshing is a process that reconstructs the mesh to improve its quality and/or adjust its resolution.
#' Uniform remeshing creates a new mesh where the edge lengths are approximately equal throughout
#' the surface, which is often desirable for analysis and visualization.
#'
#' The \code{voxel_size} parameter controls the resolution of the output mesh:
#' \itemize{
#'   \item Smaller values create denser meshes with more vertices and faces
#'   \item Larger values create coarser meshes with fewer vertices and faces
#' }
#'
#' Common reasons to remesh a surface include:
#' \itemize{
#'   \item Simplifying high-resolution meshes for faster processing
#'   \item Creating more uniform triangle sizes for better numerical stability
#'   \item Preparing meshes for specific analyses that require regular structures
#'   \item Fixing mesh defects and improving the overall quality
#' }
#'
#' This function uses the VCG library (via the Rvcg package) to perform the remeshing operation,
#' which is a robust and widely-used algorithm for mesh processing.
#'
#' @examples
#' \donttest{
#' # Create a simple cube mesh
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
#' # Note indices are 0-based
#' faces <- matrix(c(
#'   # bottom face (z=0)
#'   0, 1, 2,
#'   0, 2, 3,
#'   # top face (z=1)
#'   4, 5, 6,
#'   4, 6, 7,
#'   # front face (y=0)
#'   0, 1, 5,
#'   0, 5, 4,
#'   # back face (y=1)
#'   2, 3, 7,
#'   2, 7, 6,
#'   # left face (x=0)
#'   0, 3, 7,
#'   0, 7, 4,
#'   # right face (x=1)
#'   1, 2, 6,
#'   1, 6, 5
#' ), ncol = 3, byrow = TRUE)
#' 
#' # Create the SurfaceGeometry object
#' surf_geom <- SurfaceGeometry(vertices, faces, "lh")
#' 
#' # Remesh with a coarse voxel size - fewer triangles
#' coarse_remesh <- remeshSurface(surf_geom, voxel_size = 0.5)
#' 
#' # Remesh with a fine voxel size - more triangles
#' fine_remesh <- remeshSurface(surf_geom, voxel_size = 0.2)
#' 
#' # Visualize the meshes if rgl is available
#' if(requireNamespace("rgl", quietly = TRUE)) {
#'   # Original mesh
#'   rgl::open3d()
#'   rgl::shade3d(surf_geom@mesh, col = "red")
#'   rgl::title3d(main = "Original Mesh")
#'   
#'   # Coarse remesh
#'   rgl::open3d()
#'   rgl::shade3d(coarse_remesh@mesh, col = "green")
#'   rgl::title3d(main = "Coarse Remesh (voxel_size = 0.5)")
#'   
#'   # Fine remesh
#'   rgl::open3d()
#'   rgl::shade3d(fine_remesh@mesh, col = "blue")
#'   rgl::title3d(main = "Fine Remesh (voxel_size = 0.2)")
#'   
#'   # Compare the number of faces in each mesh
#'   cat("Original mesh faces:", ncol(surf_geom@mesh$it), "\n")
#'   cat("Coarse remesh faces:", ncol(coarse_remesh@mesh$it), "\n")
#'   cat("Fine remesh faces:", ncol(fine_remesh@mesh$it), "\n")
#' }
#' }
#'
#' @seealso \code{\link{SurfaceGeometry}}, \code{\link[Rvcg]{vcgUniformRemesh}}
#' @importFrom Rvcg vcgUniformRemesh
#' @export
remeshSurface <- function(surfgeom, voxel_size=2, ...) {
  # Perform remeshing using Rvcg::vcgUniformRemesh
  smesh <- Rvcg::vcgUniformRemesh(surfgeom@mesh, voxelSize=voxel_size, ...)

  # Extract vertices and faces from the remeshed object
  vertices <- t(smesh$vb[1:3, ])
  faces <- t(smesh$it)

  # Create a new SurfaceGeometry object with the remeshed surface
  new_surfgeom <- SurfaceGeometry(vert = vertices, faces = faces-1, hemi = surfgeom@hemi)

  return(new_surfgeom)
}



#' Construct a Graph from Mesh Vertices and Faces
#'
#' @description
#' This function creates an igraph object representing the connectivity structure of a 3D mesh
#' based on its vertices and triangular faces.
#'
#' @param vertices A numeric matrix with 3 columns representing the x, y, and z coordinates of vertices.
#'   Each row corresponds to a vertex.
#' @param nodes A numeric matrix where each row represents a triangular face, containing 0-based indices 
#'   of three vertices that form the face.
#'
#' @return An \code{igraph} object representing the mesh connectivity. The graph has the following attributes:
#'   \itemize{
#'     \item Vertex attributes: "x", "y", and "z" coordinates from the vertices matrix
#'     \item Edge attribute: "dist" (Euclidean distance between connected vertices)
#'   }
#'
#' @details
#' The function converts a triangular mesh into a graph representation where:
#' \itemize{
#'   \item Vertices of the graph correspond to vertices of the mesh
#'   \item Edges of the graph correspond to the edges of triangular faces in the mesh
#' }
#' 
#' The function performs the following steps:
#' \enumerate{
#'   \item Extracts all unique edges from the triangular faces
#'   \item Creates an undirected graph from these edges
#'   \item Simplifies the graph to remove duplicate edges and loops
#'   \item Calculates Euclidean distances between connected vertices
#'   \item Adds vertex coordinates and edge distances as attributes to the graph
#' }
#' 
#' Note that the input \code{nodes} matrix should use 0-based indexing (starting from 0),
#' as the function will increment indices by 1 when creating the graph.
#'
#' @examples
#' \donttest{
#' # Create a simple cube mesh with 8 vertices
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
#' # Define triangular faces with 0-based indices
#' faces <- matrix(c(
#'   # bottom face (z=0)
#'   0, 1, 2,
#'   0, 2, 3,
#'   # top face (z=1)
#'   4, 5, 6,
#'   4, 6, 7,
#'   # front face (y=0)
#'   0, 1, 5,
#'   0, 5, 4,
#'   # back face (y=1)
#'   2, 3, 7,
#'   2, 7, 6,
#'   # left face (x=0)
#'   0, 3, 7,
#'   0, 7, 4,
#'   # right face (x=1)
#'   1, 2, 6,
#'   1, 6, 5
#' ), ncol = 3, byrow = TRUE)
#' 
#' # Create the graph representation of the mesh
#' graph <- meshToGraph(vertices, faces)
#' 
#' # Examine the graph properties
#' cat("Number of vertices:", igraph::vcount(graph), "\n")
#' cat("Number of edges:", igraph::ecount(graph), "\n")
#' 
#' # Plot the graph if igraph is available
#' if (requireNamespace("igraph", quietly = TRUE) && 
#'     requireNamespace("rgl", quietly = TRUE)) {
#'   # First visualize the mesh
#'   rgl::open3d()
#'   mesh <- rgl::tmesh3d(
#'     vertices = t(vertices),
#'     indices = t(faces) + 1,  # rgl uses 1-based indexing
#'     homogeneous = FALSE
#'   )
#'   rgl::shade3d(mesh, col = "lightblue")
#'   rgl::title3d(main = "Original Mesh")
#'   
#'   # Visualize the graph connections using the 3D coordinates
#'   rgl::open3d()
#'   # Plot vertices
#'   rgl::points3d(vertices[,1], vertices[,2], vertices[,3], size = 10, col = "red")
#'   # Plot edges
#'   edges <- igraph::as_edgelist(graph)
#'   for (i in 1:nrow(edges)) {
#'     v1 <- edges[i, 1]
#'     v2 <- edges[i, 2]
#'     coords <- vertices[c(v1, v2), ]
#'     rgl::lines3d(coords[,1], coords[,2], coords[,3], col = "black", lwd = 2)
#'   }
#'   rgl::title3d(main = "Graph Representation")
#' }
#' }
#'
#' @seealso \code{\link{SurfaceGeometry}}, \code{\link[igraph]{graph_from_edgelist}}
#'
#' @export
#' @importFrom igraph set.vertex.attribute simplify get.edgelist
#' @importFrom igraph graph_from_edgelist get.edgelist set.vertex.attribute
#' @importFrom igraph simplify graph.adjacency
meshToGraph <- function(vertices, nodes) {
  edge1 <- as.matrix(nodes[,1:2 ])
  edge2 <- as.matrix(nodes[,2:3 ])
  edge3 <- as.matrix(nodes[,c(1,3) ])
  edges <- rbind(edge1,edge2,edge3) + 1

  gg1 <- igraph::simplify(igraph::graph_from_edgelist(edges, directed=FALSE))

  emat <- igraph::get.edgelist(gg1)
  v1 <- vertices[emat[,1],]
  v2 <- vertices[emat[,2],]

  ED <- sqrt(rowSums((v1 - v2)^2))

  gg1 <- igraph::set.vertex.attribute(gg1, "x", igraph::V(gg1), vertices[,1])
  gg1 <- igraph::set.vertex.attribute(gg1, "y", igraph::V(gg1), vertices[,2])
  gg1 <- igraph::set.vertex.attribute(gg1, "z", igraph::V(gg1), vertices[,3])

  gg1 <- igraph::set.edge.attribute(gg1, "dist", igraph::E(gg1), ED)
  gg1

}



#' @rdname show
#' @importFrom crayon bold blue green red yellow style silver
#' @importFrom crayon bgBlue white
setMethod(f="show", signature=signature("SurfaceGeometry"),
          def=function(object) {
            # Check if crayon is available
            has_crayon <- requireNamespace("crayon", quietly = TRUE)
            
            # Define styling functions that degrade gracefully if crayon isn't available
            header <- if(has_crayon) function(x) crayon::bgBlue(crayon::white(crayon::bold(x))) else function(x) x
            title <- if(has_crayon) function(x) crayon::blue(crayon::bold(x)) else function(x) x
            subtitle <- if(has_crayon) function(x) crayon::green(x) else function(x) x
            highlight <- if(has_crayon) function(x) crayon::yellow(x) else function(x) x
            normal <- if(has_crayon) function(x) crayon::silver(x) else function(x) x
            warning_style <- if(has_crayon) function(x) crayon::red(x) else function(x) x
            
            # Get basic mesh statistics
            num_vertices <- length(nodes(object))
            num_faces <- ncol(object@mesh$it)
            
            # Get hemisphere
            hemi_text <- if (object@hemi == "lh") "left" else if (object@hemi == "rh") "right" else object@hemi
            
            # Calculate additional metrics
            ## Get unique edges
            edges <- rbind(
              t(object@mesh$it[1:2,]),
              t(object@mesh$it[2:3,]),
              t(object@mesh$it[c(1,3),])
            )
            edges <- unique(t(apply(edges, 1, sort)))
            num_edges <- nrow(edges)
            
            ## Calculate Euler characteristic (V - E + F)
            euler <- num_vertices - num_edges + num_faces
            genus <- (2 - euler) / 2
            
            ## Calculate some basic quality metrics
            if (num_vertices > 0 && num_faces > 0) {
              # Calculate average edge length
              v_coords <- t(object@mesh$vb[1:3,])
              edge_lengths <- numeric(num_edges)
              for (i in 1:num_edges) {
                v1 <- edges[i, 1]
                v2 <- edges[i, 2]
                edge_lengths[i] <- sqrt(sum((v_coords[v1,] - v_coords[v2,])^2))
              }
              avg_edge_length <- mean(edge_lengths)
              max_edge_length <- max(edge_lengths)
              min_edge_length <- min(edge_lengths)
              
              # Estimate surface area (sum of triangle areas)
              face_areas <- numeric(num_faces)
              for (i in 1:num_faces) {
                v1 <- object@mesh$it[1, i]
                v2 <- object@mesh$it[2, i]
                v3 <- object@mesh$it[3, i]
                
                # Calculate vectors for two sides of triangle
                side1 <- v_coords[v2,] - v_coords[v1,]
                side2 <- v_coords[v3,] - v_coords[v1,]
                
                # Cross product to get area
                cp <- c(
                  side1[2]*side2[3] - side1[3]*side2[2],
                  side1[3]*side2[1] - side1[1]*side2[3],
                  side1[1]*side2[2] - side1[2]*side2[1]
                )
                face_areas[i] <- sqrt(sum(cp^2)) / 2
              }
              total_area <- sum(face_areas)
              
              # Check mesh quality
              edge_length_ratio <- max_edge_length / min_edge_length
              has_quality_issues <- edge_length_ratio > 100
            }
            
            # Create a simple ASCII art mesh
            mesh_art <- "    /\\     \n   /  \\    \n  /____\\   \n /      \\  \n/        \\ "
            
            # Print the formatted output
            cat("\n")
            cat(" ◢◤ SurfaceGeometry ◥◣ \n")
            cat("\n")
            
            # Add ASCII art
            cat(normal(mesh_art), "\n\n")
            
            # Basic information
            cat(title("◆ Basic Information:"), "\n")
            cat("  ", subtitle("Hemisphere:"), " ", highlight(hemi_text), "\n", sep="")
            cat("  ", subtitle("Vertices:"), "   ", highlight(format(num_vertices, big.mark=",")), "\n", sep="")
            cat("  ", subtitle("Faces:"), "      ", highlight(format(num_faces, big.mark=",")), "\n", sep="")
            cat("  ", subtitle("Edges:"), "      ", highlight(format(num_edges, big.mark=",")), "\n", sep="")
            cat("\n")
            
            # Geometry metrics
            cat(title("◆ Geometry Metrics:"), "\n")
            cat("  ", subtitle("Euler Characteristic:"), " ", highlight(euler), "\n", sep="")
            cat("  ", subtitle("Genus:"), "               ", highlight(genus), "\n", sep="")
            
            if (exists("total_area")) {
              cat("  ", subtitle("Surface Area:"), "        ", highlight(format(total_area, digits=4)), "\n", sep="")
              cat("  ", subtitle("Avg Edge Length:"), "     ", highlight(format(avg_edge_length, digits=4)), "\n", sep="")
              
              if (has_quality_issues) {
                cat("\n")
                cat(warning_style("⚠ Warning: Mesh quality issues detected ⚠"), "\n")
                cat(warning_style("  Edge length ratio: ", format(edge_length_ratio, digits=2)), "\n")
                cat(warning_style("  Consider using remeshSurface() to improve mesh quality"), "\n")
              }
            }
            
            cat("\n")
          })




#' @rdname coords
#' @export
setMethod(f="coords", signature=c("SurfaceGeometry"),
          def=function(x) {
            t(x@mesh$vb[1:3,])
          })


#' @export
#' @rdname vertices
setMethod(f="vertices", signature=c("SurfaceGeometry"),
          def=function(x, indices) {
            t(x@mesh$vb[1:3,indices, drop=FALSE])
          })


#' @export
#' @rdname nodes
setMethod(f="nodes", signature=c("SurfaceGeometry"),
          def=function(x) {
            seq(1, ncol(x@mesh$vb))
          })


#' @rdname coords
#' @importMethodsFrom neuroim2 coords
#' @export
setMethod(f="coords", signature=c("igraph"),
          def=function(x) {
            cbind(igraph:::vertex_attr(x, "x"),
                  igraph:::vertex_attr(x, "y"),
                  igraph:::vertex_attr(x, "z"))
          })



#' @export
#' @rdname graph
setMethod("graph", signature(x="SurfaceGeometry"),
          def=function(x) {
            x@graph
          })


#' @export
#' @rdname load_data
setMethod(f="load_data", signature=c("SurfaceGeometrySource"),
          def=function(x) {
            geometry <- load_data(x@meta_info)
          })

