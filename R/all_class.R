#' @importClassesFrom neuroim2 FileFormat
NULL

setOldClass("mesh3d")
setOldClass("igraph")
setOldClass("gifti")

is_surface_like <- function(x) {
  is(x, "SurfaceGeometry") || (methods::isClass("SurfaceSet") && is(x, "SurfaceSet"))
}

.validation_result <- function(errors) {
  if (length(errors)) errors else TRUE
}

.surface_geometry_for_validation <- function(x) {
  if (is(x, "SurfaceGeometry")) {
    return(x)
  }

  if (methods::isClass("SurfaceSet") && is(x, "SurfaceSet") &&
      length(x@surfaces) > 0L && length(x@default_label) == 1L &&
      !is.na(x@default_label) &&
      x@default_label %in% names(x@surfaces)) {
    return(x@surfaces[[x@default_label]])
  }

  NULL
}

.surface_vertex_count <- function(x) {
  geometry <- .surface_geometry_for_validation(x)
  if (is.null(geometry) || !is.matrix(geometry@mesh$vb)) {
    return(NA_integer_)
  }
  ncol(geometry@mesh$vb)
}

.surface_index_errors <- function(indices, geometry, field = "'indices'") {
  errors <- character()
  if (anyNA(indices)) {
    errors <- c(errors, paste(field, "must not contain missing values"))
  }
  if (any(indices < 1L, na.rm = TRUE)) {
    errors <- c(errors, paste(field, "must contain positive indices"))
  }
  if (anyDuplicated(indices)) {
    errors <- c(errors, paste(field, "must not contain duplicates"))
  }

  vertex_count <- .surface_vertex_count(geometry)
  if (!is.na(vertex_count) && any(indices > vertex_count, na.rm = TRUE)) {
    errors <- c(errors, paste(field, "contains indices outside the geometry"))
  }
  errors
}

.coerce_surface_indices <- function(indices, field = "'indices'") {
  if (!is.numeric(indices)) {
    stop(field, " must be numeric", call. = FALSE)
  }
  if (anyNA(indices) || any(!is.finite(indices))) {
    stop(field, " must not contain missing or non-finite values", call. = FALSE)
  }
  if (any(indices != floor(indices))) {
    stop(field, " must contain integer-valued indices", call. = FALSE)
  }
  if (any(abs(indices) > .Machine$integer.max)) {
    stop(field, " contains values outside the integer range", call. = FALSE)
  }
  as.integer(indices)
}

.valid_hex_colors <- function(x) {
  length(x) == 0L ||
    all(!is.na(x) & grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", x))
}

.descriptor_validity <- function(object, expected_format) {
  errors <- character()
  slots <- c(
    "file_format", "header_encoding", "header_extension",
    "data_encoding", "data_extension"
  )
  for (slot_name in slots) {
    value <- methods::slot(object, slot_name)
    if (length(value) != 1L || is.na(value) || !nzchar(value)) {
      errors <- c(errors, paste0("'", slot_name,
                                "' must be one non-empty string"))
    }
  }
  if (length(object@file_format) == 1L &&
      !is.na(object@file_format) &&
      object@file_format != expected_format) {
    errors <- c(errors, paste0("'file_format' must be '",
                              expected_format, "'"))
  }
  .validation_result(errors)
}

.hemisphere_side <- function(x) {
  if (length(x) != 1L || is.na(x)) {
    return(NA_character_)
  }
  value <- tolower(trimws(x))
  if (value %in% c("l", "lh", "left")) {
    "left"
  } else if (value %in% c("r", "rh", "right")) {
    "right"
  } else {
    NA_character_
  }
}


#' SurfaceGeometry Class
#'
#' @description
#' The `SurfaceGeometry` class represents a three-dimensional surface consisting of a set of triangle vertices.
#' It encapsulates the mesh structure, graph representation, and hemisphere information of a brain surface.
#'
#' @slot mesh An object of class \code{mesh3d} representing the underlying 3D mesh structure.
#' @slot graph An object of class \code{igraph} representing the underlying graph structure of the surface.
#' @slot hemi A character string indicating the hemisphere of the surface ("left", "right", or "both").
#' @slot label A character string describing the surface type (e.g., "pial", "white", "inflated", "sphere").
#' @slot surf_to_world A 4x4 numeric matrix representing the affine transformation from surface
#'   coordinates to world (scanner RAS) coordinates. This handles coordinate system conventions
#'   (e.g., RAS vs LPI), reference space alignment (e.g., MNI305 vs MNI152), and any embedded
#'   transforms from file formats (e.g., GIFTI CoordinateSystemTransformMatrix). Defaults to
#'   the identity matrix (surface coordinates are assumed to be in world space).
#'
#' @details
#' This class is fundamental for representing brain surface geometries in neuroimaging analyses.
#' The mesh slot contains the vertex and face information, while the graph slot provides
#' a network representation of the surface topology. The hemi slot specifies which
#' hemisphere the surface represents.
#'
#' The \code{surf_to_world} transform enables proper projection between surface and volume spaces:
#' \code{world_pos = surf_to_world \%*\% c(vertex_pos, 1)}, then
#' \code{voxel_ijk = world_to_vox \%*\% world_pos}.
#'
#' @seealso \code{\link[rgl]{mesh3d}}, \code{\link[igraph]{igraph}}, \code{\link{surf_to_world}}
#'
#' @return An object of class \code{SurfaceGeometry}.
#'
#' @examples
#' \donttest{
#' geometry <- example_surface_geometry()
#' }
#'
#' @export
setClass("SurfaceGeometry",
         representation = representation(
           mesh = "mesh3d",
           graph = "igraph",
           hemi = "character",
           label = "character",
           surf_to_world = "matrix"
         ),
         prototype = list(
           label = NA_character_,
           surf_to_world = diag(4)
         ),
         validity = function(object) {
           errors <- character()
           vb <- object@mesh$vb
           it <- object@mesh$it

           if (!is.matrix(vb) || !is.numeric(vb) || nrow(vb) < 3L ||
               ncol(vb) < 1L || any(!is.finite(vb))) {
             errors <- c(errors,
                         "'mesh$vb' must contain finite vertex coordinates")
           }

           vertex_count <- if (is.matrix(vb)) ncol(vb) else NA_integer_
           if (!is.matrix(it) || !is.numeric(it) || nrow(it) != 3L ||
               ncol(it) < 1L || anyNA(it) || any(!is.finite(it)) ||
               any(it != floor(it))) {
             errors <- c(errors,
                         "'mesh$it' must be a 3-row matrix of triangle indices")
           } else if (!is.na(vertex_count) &&
                      (any(it < 1L) || any(it > vertex_count))) {
             errors <- c(errors,
                         "'mesh$it' contains indices outside 'mesh$vb'")
           }

           if (!is.na(vertex_count) &&
               igraph::vcount(object@graph) != vertex_count) {
             errors <- c(errors,
                         "'graph' vertex count must match 'mesh$vb'")
           }
           if (length(object@hemi) != 1L || is.na(object@hemi) ||
               !nzchar(object@hemi)) {
             errors <- c(errors, "'hemi' must be one non-empty string")
           }
           if (length(object@label) != 1L ||
               (!is.na(object@label) && !nzchar(object@label))) {
             errors <- c(errors,
                         "'label' must be one non-empty string or NA")
           }
           if (!is.matrix(object@surf_to_world) ||
               !is.numeric(object@surf_to_world) ||
               !identical(dim(object@surf_to_world), c(4L, 4L)) ||
               any(!is.finite(object@surf_to_world))) {
             errors <- c(errors,
                         "'surf_to_world' must be a finite numeric 4x4 matrix")
           }
           .validation_result(errors)
         })





#' SurfaceGeometryMetaInfo Class
#'
#' @description
#' The `SurfaceGeometryMetaInfo` class encapsulates meta information for brain surface geometry.
#' It stores details about the file locations, surface properties, and spatial characteristics.
#'
#' @slot header_file A character string specifying the name of the file containing meta information.
#' @slot data_file A character string specifying the name of the file containing the actual surface data.
#' @slot file_descriptor An object of class `FileFormat` describing the image file format.
#' @slot vertices An integer indicating the number of surface vertices.
#' @slot faces An integer indicating the number of faces in the surface mesh.
#' @slot embed_dimension An integer specifying the dimensionality of the embedding (typically 3 for 3D surfaces).
#' @slot label A character string indicating the type of surface (e.g., "white", "pial", "inflated", "flat", "spherical").
#' @slot hemi A character string indicating the hemisphere ("lh" for left, "rh" for right, or "unknown").
#'
#' @details
#' This class is crucial for maintaining metadata about brain surface geometries.
#' It provides a structured way to store information about file locations,
#' surface properties, and spatial characteristics, which is essential for
#' proper handling and processing of brain surface data in neuroimaging analyses.
#'
#'
#' @examples
#' \donttest{
#' meta_info <- new("SurfaceGeometryMetaInfo",
#'                  header_file = "surface_meta.txt",
#'                  data_file = "surface_data.gii",
#'                  file_descriptor = new("FileFormat"),
#'                  vertices = 40000L,
#'                  faces = 79998L,
#'                  embed_dimension = 3L,
#'                  label = "white",
#'                  hemi = "lh")
#' }
#'
#' @return An object of class \code{SurfaceGeometryMetaInfo}.
#'
#' @importClassesFrom neuroim2 FileFormat
#' @export
setClass("SurfaceGeometryMetaInfo",
         representation = representation(
           header_file = "character",
           data_file = "character",
           file_descriptor = "FileFormat",
           vertices = "integer",
           faces = "integer",
           label = "character",
           hemi = "character",
           embed_dimension = "integer"
         ),
         validity = function(object) {
           errors <- character()
           if (length(object@header_file) != 1L ||
               is.na(object@header_file) || !nzchar(object@header_file)) {
             errors <- c(errors,
                         "'header_file' must be one non-empty string")
           }
           if (length(object@data_file) != 1L ||
               is.na(object@data_file) || !nzchar(object@data_file)) {
             errors <- c(errors,
                         "'data_file' must be one non-empty string")
           }
           if (length(object@vertices) != 1L || is.na(object@vertices) ||
               object@vertices < 1L) {
             errors <- c(errors, "'vertices' must be one positive integer")
           }
           if (length(object@faces) != 1L || is.na(object@faces) ||
               object@faces < 1L) {
             errors <- c(errors, "'faces' must be one positive integer")
           }
           if (length(object@embed_dimension) != 1L ||
               is.na(object@embed_dimension) || object@embed_dimension < 1L) {
             errors <- c(errors,
                         "'embed_dimension' must be one positive integer")
           }
           if (length(object@label) != 1L || is.na(object@label) ||
               !nzchar(object@label)) {
             errors <- c(errors, "'label' must be one non-empty string")
           }
           if (length(object@hemi) != 1L || is.na(object@hemi) ||
               !nzchar(object@hemi)) {
             errors <- c(errors, "'hemi' must be one non-empty string")
           }
           .validation_result(errors)
         })

#' FreesurferSurfaceGeometryMetaInfo Class
#'
#' @description
#' The `FreesurferSurfaceGeometryMetaInfo` class extends `SurfaceGeometryMetaInfo` to specifically
#' handle meta information for Freesurfer-formatted brain surface geometries.
#'
#' @details
#' This class inherits all slots from the parent `SurfaceGeometryMetaInfo` class and is specialized
#' for working with Freesurfer surface files (e.g., .asc, .pial, .white, .inflated). It maintains
#' the same structure but is used specifically when the surface data originates from Freesurfer
#' processing pipelines.
#'
#'
#' @examples
#' \donttest{
#' fs_meta <- new("FreesurferSurfaceGeometryMetaInfo",
#'                header_file = "lh.white.asc",
#'                data_file = "lh.white.asc",
#'                file_descriptor = new("FileFormat"),
#'                vertices = 140000L,
#'                faces = 279998L,
#'                embed_dimension = 3L,
#'                label = "white",
#'                hemi = "lh")
#' }
#'
#' @return An object of class \code{FreesurferSurfaceGeometryMetaInfo}.
#'
#' @rdname FreesurferSurfaceGeometryMetaInfo-class
#' @export
setClass("FreesurferSurfaceGeometryMetaInfo", contains=c("SurfaceGeometryMetaInfo"))

#' SurfaceDataMetaInfo
#'
#' This class contains meta information for surface-based data (the values that map to a surface geometry)
#'
#' @rdname SurfaceDataMetaInfo-class
#' @slot header_file name of the file containing meta information
#' @slot data_file name of the file containing data
#' @slot file_descriptor descriptor of image file format
#' @slot node_count the number of nodes for which surface data exists
#' @slot nels the number of data vectors (typically the number of columns
#' in the surface data matrix; nels = 1 for a single surface data set)
#' @slot label a label indicating the type of surface
#' (e.g. white, pial, inflated, flat, spherical)
#'
#' @examples
#' \dontshow{geometry <- example_surface_geometry()}
#' \donttest{
#' meta_info <- new("SurfaceDataMetaInfo",
#'                  header_file = "data_header.txt",
#'                  data_file = "surface_data.1D",
#'                  file_descriptor = new("FileFormat"),
#'                  node_count = length(nodes(geometry)),
#'                  nels = 1L,
#'                  label = "thickness")
#' }
#'
#' @return An object of class \code{SurfaceDataMetaInfo}.
#'
#' @export
setClass("SurfaceDataMetaInfo",
         representation=
           representation(
             header_file="character",
             data_file="character",
             file_descriptor="FileFormat",
             node_count="integer",
             nels="integer",
             label="character"),
         validity = function(object) {
           errors <- character()
           if (length(object@header_file) != 1L ||
               is.na(object@header_file) || !nzchar(object@header_file)) {
             errors <- c(errors,
                         "'header_file' must be one non-empty string")
           }
           if (length(object@data_file) != 1L ||
               is.na(object@data_file) || !nzchar(object@data_file)) {
             errors <- c(errors,
                         "'data_file' must be one non-empty string")
           }
           if (length(object@node_count) != 1L ||
               is.na(object@node_count) || object@node_count < 0L) {
             errors <- c(errors,
                         "'node_count' must be one non-negative integer")
           }
           if (length(object@nels) != 1L || is.na(object@nels) ||
               object@nels < 1L) {
             errors <- c(errors, "'nels' must be one positive integer")
           }
           if (length(object@label) != 1L || is.na(object@label) ||
               !nzchar(object@label)) {
             errors <- c(errors, "'label' must be one non-empty string")
           }
           .validation_result(errors)
         })


#' NIMLSurfaceDataMetaInfo
#'
#' This class contains meta information for surface-based data for the NIML data format
#'
#' @rdname NIMLSurfaceDataMetaInfo-class
#' @slot data the numeric data matrix of surface values (rows = nodes, columns=surface vectors)
#' @slot node_indices the indices of the nodes for mapping to associated surface geometry.
#'
#' @examples
#' \dontshow{geometry <- example_surface_geometry()}
#' \donttest{
#' meta_info <- new("SurfaceDataMetaInfo",
#'                  header_file = "data_header.txt",
#'                  data_file = "surface_data.niml.dset",
#'                  file_descriptor = new("FileFormat"),
#'                  node_count = length(nodes(geometry)),
#'                  nels = 2L,
#'                  label = "thickness")
#'
#' surface_data <- matrix(rnorm(meta_info@node_count * meta_info@nels),
#'                        nrow = meta_info@node_count, ncol = meta_info@nels)
#'
#' niml_meta <- new("NIMLSurfaceDataMetaInfo",
#'                 header_file = meta_info@header_file,
#'                 data_file = meta_info@data_file,
#'                 file_descriptor = meta_info@file_descriptor,
#'                 node_count = meta_info@node_count,
#'                 nels = meta_info@nels,
#'                 label = meta_info@label,
#'                 data = surface_data,
#'                 node_indices = seq_len(meta_info@node_count))
#' }
#'
#' @return An object of class \code{NIMLSurfaceDataMetaInfo}.
#'
#' @export
setClass("NIMLSurfaceDataMetaInfo",
         representation=
           representation(
             data="matrix",
             node_indices="integer"),
         contains=c("SurfaceDataMetaInfo"),
         validity = function(object) {
           errors <- character()
           if (length(object@node_count) == 1L &&
               !is.na(object@node_count) &&
               nrow(object@data) != object@node_count) {
             errors <- c(errors,
                         "'nrow(data)' must equal 'node_count'")
           }
           if (length(object@nels) == 1L && !is.na(object@nels) &&
               ncol(object@data) != object@nels) {
             errors <- c(errors,
                         "'ncol(data)' must equal 'nels'")
           }
           if (length(object@node_count) == 1L &&
               !is.na(object@node_count) &&
               length(object@node_indices) != object@node_count) {
             errors <- c(errors,
                         "length of 'node_indices' must equal 'node_count'")
           }
           if (anyNA(object@node_indices) ||
               any(object@node_indices < 0L, na.rm = TRUE)) {
             errors <- c(errors,
                         "'node_indices' must be non-negative and non-missing")
           }
           if (anyDuplicated(object@node_indices)) {
             errors <- c(errors, "'node_indices' must not contain duplicates")
           }
           .validation_result(errors)
         })


#' GIFTISurfaceGeometryMetaInfo
#'
#' This class contains meta information for surface-based geometry for the GIFTI data format
#'
#' @rdname GIFTISurfaceGeometryMetaInfo-class
#' @slot info the underlying \code{gifti} object returned by
#'   \code{\link[gifti]{readgii}}
#'
#' @examples
#' \donttest{
#' # First create a SurfaceGeometryMetaInfo parent object
#' meta_info <- new("SurfaceGeometryMetaInfo",
#'                  header_file = "rscan01_lh.gii",
#'                  data_file = "rscan01_lh.gii",
#'                  file_descriptor = new("FileFormat"),
#'                  vertices = 40000L,
#'                  faces = 79998L,
#'                  embed_dimension = 3L,
#'                  label = "white",
#'                  hemi = "lh")
#'
#' # Use the example file included in the package
#' example_gii_file <- system.file("extdata", "rscan01_lh.gii", package = "neurostyle")
#'
#' if (file.exists(example_gii_file) && requireNamespace("gifti", quietly = TRUE)) {
#'   # Load the actual GIFTI file
#'   gifti_obj <- gifti::readgii(example_gii_file)
#'
#'   # Create GIFTISurfaceGeometryMetaInfo object with the real file
#'   gifti_meta <- new("GIFTISurfaceGeometryMetaInfo",
#'                    header_file = meta_info@header_file,
#'                    data_file = meta_info@data_file,
#'                    file_descriptor = meta_info@file_descriptor,
#'                    vertices = meta_info@vertices,
#'                    faces = meta_info@faces,
#'                    embed_dimension = meta_info@embed_dimension,
#'                    label = meta_info@label,
#'                    hemi = meta_info@hemi,
#'                    info = gifti_obj)
#' }
#' }
#'
#' @return An object of class \code{GIFTISurfaceGeometryMetaInfo}.
#'
#' @import gifti
#' @export
setClass("GIFTISurfaceGeometryMetaInfo",
         representation=
           representation(
             info="gifti"),
         contains=c("SurfaceGeometryMetaInfo"))

#' GIFTISurfaceDataMetaInfo
#'
#' This class contains meta information for surface-based data for the GIFTI data format
#'
#' @rdname GIFTISurfaceDataMetaInfo-class
#' @slot info the underlying \code{gifti} object returned by
#'   \code{\link[gifti]{readgii}}
#'
#' @examples
#' \donttest{
#' # First create a SurfaceDataMetaInfo parent object
#' meta_info <- new("SurfaceDataMetaInfo",
#'                  header_file = "rscan01_lh.gii",
#'                  data_file = "rscan01_lh.gii",
#'                  file_descriptor = new("FileFormat"),
#'                  node_count = 40000L,
#'                  nels = 1L,
#'                  label = "thickness")
#'
#' # Use the example file included in the package
#' example_gii_file <- system.file("extdata", "rscan01_lh.gii", package = "neurostyle")
#'
#' if (file.exists(example_gii_file) && requireNamespace("gifti", quietly = TRUE)) {
#'   # Load the actual GIFTI file
#'   gifti_obj <- gifti::readgii(example_gii_file)
#'
#'   # Create GIFTISurfaceDataMetaInfo object with the real file
#'   gifti_data_meta <- new("GIFTISurfaceDataMetaInfo",
#'                         header_file = meta_info@header_file,
#'                         data_file = meta_info@data_file,
#'                         file_descriptor = meta_info@file_descriptor,
#'                         node_count = meta_info@node_count,
#'                         nels = meta_info@nels,
#'                         label = meta_info@label,
#'                         info = gifti_obj)
#' }
#' }
#'
#' @return An object of class \code{GIFTISurfaceDataMetaInfo}.
#'
#' @import gifti
#' @export
setClass("GIFTISurfaceDataMetaInfo",
         representation=
           representation(
             info="gifti"),
         contains=c("SurfaceDataMetaInfo"))





#' SurfaceGeometrySource Class
#'
#' @description
#' The `SurfaceGeometrySource` class serves as a factory for creating
#' \code{\linkS4class{SurfaceGeometry}} instances. It encapsulates the
#' meta information required to construct a surface geometry.
#'
#' @slot meta_info An object of class \code{\linkS4class{SurfaceGeometryMetaInfo}}
#'   containing the metadata necessary for creating a surface geometry.
#'
#' @details
#' This class is designed to facilitate the creation of \code{\linkS4class{SurfaceGeometry}}
#' objects by providing a standardized way to store and access the required metadata.
#' It acts as an intermediate step in the process of loading and constructing
#' surface geometries from various file formats and sources.
#'
#' @seealso
#' \code{\linkS4class{SurfaceGeometry}}, \code{\linkS4class{SurfaceGeometryMetaInfo}}
#'
#' @examples
#' \donttest{
#' # Create a SurfaceGeometryMetaInfo object
#' meta_info <- new("SurfaceGeometryMetaInfo",
#'                  header_file = "surface_meta.txt",
#'                  data_file = "surface_data.gii",
#'                  file_descriptor = new("FileFormat"),
#'                  vertices = 40000L,
#'                  faces = 79998L,
#'                  embed_dimension = 3L,
#'                  label = "white",
#'                  hemi = "lh")
#'
#' # Create a SurfaceGeometrySource object
#' geom_source <- new("SurfaceGeometrySource", meta_info = meta_info)
#'
#' # Use geom_source to create a SurfaceGeometry object (hypothetical function)
#' # surface_geometry <- createSurfaceGeometry(geom_source)
#' }
#'
#' @return An object of class \code{SurfaceGeometrySource}.
#'
#' @export
setClass("SurfaceGeometrySource",
         representation = representation(meta_info = "SurfaceGeometryMetaInfo"))


#' NeuroSurfaceSource Class
#'
#' @description
#' The `NeuroSurfaceSource` class serves as a factory for creating
#' \code{\linkS4class{NeuroSurface}} instances. It encapsulates all necessary
#' information to construct a neuroimaging surface, including geometry,
#' metadata, and indexing information.
#'
#' @slot geometry An object of class \code{\linkS4class{SurfaceGeometry}}
#'   representing the underlying surface structure.
#' @slot data_meta_info An object of class \code{\linkS4class{SurfaceDataMetaInfo}}
#'   containing metadata about the surface data.
#' @slot colind An integer specifying the column index of the surface map to be loaded.
#' @slot nodeind An integer vector specifying the node indices of the surface map to be loaded.
#'
#' @details
#' This class is designed to facilitate the creation of \code{\linkS4class{NeuroSurface}}
#' objects by providing a standardized way to store and access all required components.
#' It combines geometric information, metadata, and indexing details necessary for
#' constructing a complete neuroimaging surface representation.
#'
#' @seealso
#' \code{\linkS4class{NeuroSurface}}, \code{\linkS4class{SurfaceGeometry}},
#' \code{\linkS4class{SurfaceDataMetaInfo}}
#'
#' @examples
#' \donttest{
#' # Create a simple mesh for the example
#' vertices <- c(
#'   0, 0, 0,
#'   1, 0, 0,
#'   0, 1, 0,
#'   0, 0, 1
#' )
#' triangles <- c(
#'   1, 2, 3,
#'   1, 2, 4,
#'   1, 3, 4,
#'   2, 3, 4
#' )
#'
#' # Create mesh3d object
#' mesh <- rgl::mesh3d(vertices = vertices, triangles = triangles)
#'
#' # Create a graph representation
#' edges <- rbind(
#'   c(1,2), c(1,3), c(1,4),
#'   c(2,3), c(2,4),
#'   c(3,4)
#' )
#' graph <- igraph::graph_from_edgelist(edges)
#'
#' # Create a SurfaceGeometry object
#' geometry <- new("SurfaceGeometry",
#'                mesh = mesh,
#'                graph = graph,
#'                hemi = "left")
#'
#' # Create a SurfaceDataMetaInfo object
#' data_meta_info <- new("SurfaceDataMetaInfo",
#'                       header_file = "data_meta.txt",
#'                       data_file = "surface_data.1D",
#'                       file_descriptor = new("FileFormat"),
#'                       node_count = 4L,
#'                       nels = 1L,
#'                       label = "thickness")
#'
#' # Create a NeuroSurfaceSource object
#' neuro_source <- new("NeuroSurfaceSource",
#'                     geometry = geometry,
#'                     data_meta_info = data_meta_info,
#'                     colind = 1L,
#'                     nodeind = 1:4)
#' }
#'
#' @export
setClass("NeuroSurfaceSource",
         representation = representation(
           geometry = "SurfaceGeometry",
           data_meta_info = "SurfaceDataMetaInfo",
           colind = "integer",
           nodeind = "integer"
         ),
         validity = function(object) {
           errors <- character()
           if (length(object@colind) < 1L || anyNA(object@colind) ||
               any(object@colind < 1L, na.rm = TRUE)) {
             errors <- c(errors,
                         "'colind' must contain positive, non-missing indices")
           }
           if (length(object@data_meta_info@nels) == 1L &&
               !is.na(object@data_meta_info@nels) &&
               any(object@colind > object@data_meta_info@nels,
                   na.rm = TRUE)) {
             errors <- c(errors,
                         "'colind' contains columns outside 'data_meta_info'")
           }
           if (anyDuplicated(object@colind)) {
             errors <- c(errors, "'colind' must not contain duplicates")
           }
           errors <- c(
             errors,
             .surface_index_errors(object@nodeind, object@geometry,
                                   field = "'nodeind'")
           )
           .validation_result(errors)
         })

#' NeuroSurfaceVectorSource
#'
#' A class that is used to produce a \code{\linkS4class{NeuroSurfaceVector}} instance
#'
#' @rdname NeuroSurfaceVectorSource-class
#' @slot geometry a \code{\linkS4class{SurfaceGeometry}} instance
#' @slot data_meta_info a \code{\linkS4class{SurfaceDataMetaInfo}} instance
#' @slot colind the column indices vector of the surface maps to be loaded
#'
#' @return An object of class \code{NeuroSurfaceVectorSource}.
#'
#' @export
setClass("NeuroSurfaceVectorSource", representation=
           representation(geometry="SurfaceGeometry",
                          data_meta_info="SurfaceDataMetaInfo",
                          colind="integer",
                          nodeind="integer"),
         contains=c("NeuroSurfaceSource"))







#' NIMLSurfaceFileDescriptor
#'
#' This class supports the NIML file format for surface-based data
#'
#' @return An object of class \code{NIMLSurfaceFileDescriptor}.
#'
#' @rdname NIMLSurfaceFileDescriptor-class
#' @export
setClass("NIMLSurfaceFileDescriptor",
         contains = c("FileFormat"),
         prototype = list(
           file_format = "NIML",
           header_encoding = "raw",
           header_extension = "niml.dset",
           data_encoding = "raw",
           data_extension = "niml.dset"
         ),
         validity = function(object) {
           .descriptor_validity(object, "NIML")
         })


#' AFNISurfaceFileDescriptor
#'
#' This class supports the AFNI 1D file format for surface-based data
#'
#' @return An object of class \code{AFNISurfaceFileDescriptor}.
#'
#' @rdname AFNISurfaceFileDescriptor-class
#' @export
setClass("AFNISurfaceFileDescriptor",
         contains = c("FileFormat"),
         prototype = list(
           file_format = "1D",
           header_encoding = "raw",
           header_extension = "1D.dset",
           data_encoding = "raw",
           data_extension = "1D.dset"
         ),
         validity = function(object) {
           .descriptor_validity(object, "1D")
         })

#' GIFTISurfaceFileDescriptor
#'
#' This class supports the GIFTI file format for surface-based data
#'
#' @return An object of class \code{GIFTISurfaceFileDescriptor}.
#'
#' @rdname GIFTISurfaceFileDescriptor-class
#' @export
setClass("GIFTISurfaceFileDescriptor",
         contains = c("FileFormat"),
         prototype = list(
           file_format = "GIFTI",
           header_encoding = "raw",
           header_extension = "gii",
           data_encoding = "gii",
           data_extension = "gii"
         ),
         validity = function(object) {
           .descriptor_validity(object, "GIFTI")
         })


#' FresurferAsciiSurfaceFileDescriptor
#'
#' This class supports the Freesurfer Ascii file format for surface geometry
#'
#' @rdname FreesurferAsciiSurfaceFileDescriptor-class
#'
#' @examples
#' \donttest{
#' # Create a FreesurferAsciiSurfaceFileDescriptor object
#' fs_ascii_descriptor <- new("FreesurferAsciiSurfaceFileDescriptor")
#'
#' # This descriptor would typically be used when loading Freesurfer ASCII surfaces
#' # Example of a path to a Freesurfer ASCII surface file:
#' # fs_ascii_file <- "/path/to/subject/surf/lh.pial.asc"
#'
#' # In practice, this descriptor would be used in code like:
#' # surface_geo <- loadSurfaceGeometry(fs_ascii_file, descriptor = fs_ascii_descriptor)
#' }
#'
#' @return An object of class \code{FreesurferAsciiSurfaceFileDescriptor}.
#'
#' @export
setClass("FreesurferAsciiSurfaceFileDescriptor",
         contains = c("FileFormat"),
         prototype = list(
           file_format = "Freesurfer_ASCII",
           header_encoding = "text",
           header_extension = "asc",
           data_encoding = "raw",
           data_extension = "asc"
         ),
         validity = function(object) {
           .descriptor_validity(object, "Freesurfer_ASCII")
         })

#' FresurferBinarySurfaceFileDescriptor
#'
#' This class supports the Freesurfer binary file format for surface geometry
#'
#' @rdname FreesurferBinarySurfaceFileDescriptor-class
#'
#' @examples
#' \donttest{
#' # Create a FreesurferBinarySurfaceFileDescriptor object
#' fs_binary_descriptor <- new("FreesurferBinarySurfaceFileDescriptor")
#'
#' # This descriptor would typically be used when loading Freesurfer binary surfaces
#' # Example of a path to a Freesurfer binary surface file:
#' # fs_binary_file <- "/path/to/subject/surf/lh.pial"
#'
#' # In practice, this descriptor would be used in code like:
#' # surface_geo <- loadSurfaceGeometry(fs_binary_file, descriptor = fs_binary_descriptor)
#' }
#'
#' @return An object of class \code{FreesurferBinarySurfaceFileDescriptor}.
#'
#' @export
setClass("FreesurferBinarySurfaceFileDescriptor",
         contains = c("FileFormat"),
         prototype = list(
           file_format = "Freesurfer_BINARY",
           header_encoding = "raw",
           header_extension = ".",
           data_encoding = "raw",
           data_extension = "."
         ),
         validity = function(object) {
           .descriptor_validity(object, "Freesurfer_BINARY")
         })



#' ROISurface
#'
#' A class that represents a surface-based region of interest
#'
#' @slot geometry the geometry of the parent surface: a \code{SurfaceGeometry} instance
#' @slot data the vector-valued \code{numeric} data stored in ROI
#' @slot coords the surface-based coordinates of the data
#' @slot indices the node indices of the parent surface stored in the \code{geometry} field.
#'
#' @details
#' The ROISurface class provides a way to represent a specific subset of vertices on a
#' brain surface along with their associated data values. This is particularly useful
#' for analyzing or visualizing specific anatomical or functional regions on the cortical
#' surface.
#'
#' The class maintains a reference to the complete parent surface geometry while storing
#' only the relevant subset of vertices, their coordinates, and their data values. The
#' \code{indices} slot allows mapping back to the original vertex indices in the parent
#' surface.
#'
#' Typical use cases include:
#' \itemize{
#'   \item Extracting and analyzing data from anatomical regions of interest
#'   \item Working with functional clusters identified in analyses
#'   \item Isolating specific surface features for detailed investigation
#'   \item Statistical analysis of data within defined surface regions
#' }
#'
#' @examples
#' \donttest{
#' # Create a simple tetrahedron mesh for the example
#' vertices <- c(
#'   0, 0, 0,
#'   1, 0, 0,
#'   0, 1, 0,
#'   0, 0, 1
#' )
#' triangles <- c(
#'   1, 2, 3,
#'   1, 2, 4,
#'   1, 3, 4,
#'   2, 3, 4
#' )
#'
#' # Create mesh3d object
#' mesh <- rgl::mesh3d(vertices = vertices, triangles = triangles)
#'
#' # Create a graph representation
#' edges <- rbind(
#'   c(1,2), c(1,3), c(1,4),
#'   c(2,3), c(2,4),
#'   c(3,4)
#' )
#' graph <- igraph::graph_from_edgelist(edges)
#'
#' # Create a SurfaceGeometry object
#' geometry <- new("SurfaceGeometry",
#'                mesh = mesh,
#'                graph = graph,
#'                hemi = "left")
#'
#' # Define the ROI - just using vertices 1 and 2 as an example
#' roi_indices <- c(1L, 2L)
#'
#' # Extract coordinates for these vertices
#' roi_coords <- matrix(
#'   c(0, 0, 0,  # coordinates for vertex 1
#'     1, 0, 0), # coordinates for vertex 2
#'   ncol = 3, byrow = TRUE
#' )
#'
#' # Create data values for the ROI vertices
#' roi_data <- c(0.75, 1.25)  # example values for the two vertices
#'
#' # Create the ROISurface object
#' roi <- new("ROISurface",
#'           geometry = geometry,
#'           data = roi_data,
#'           coords = roi_coords,
#'           indices = roi_indices)
#' }
#'
#' @return An object of class \code{ROISurface}.
#'
#' @importClassesFrom neuroim2 ROI
#' @exportClass ROISurface
#' @rdname ROISurface-class
setClass("ROISurface",
         representation=representation(geometry="SurfaceGeometry", data="numeric",
                                       coords="matrix", indices="integer"),
         validity = function(object) {
           errors <- character()
           if (ncol(object@coords) != 3) {
             errors <- c(errors,
                         "'coords' must be a matrix with 3 columns")
           }
           if (nrow(object@coords) != length(object@indices)) {
             errors <- c(errors,
                         "length of 'indices' must equal 'nrow(coords)'")
           }
           if (length(object@data) != length(object@indices)) {
             errors <- c(errors,
                         "length of 'data' must equal length of 'indices'")
           }
           index_errors <- .surface_index_errors(object@indices,
                                                 object@geometry)
           errors <- c(errors, index_errors)
           if (!length(index_errors) &&
               ncol(object@coords) == 3L &&
               nrow(object@coords) == length(object@indices)) {
             expected <- t(object@geometry@mesh$vb[
               1:3, object@indices, drop = FALSE
             ])
             if (!isTRUE(all.equal(object@coords, expected,
                                   check.attributes = FALSE))) {
               errors <- c(errors,
                           "'coords' must match geometry at 'indices'")
             }
           }
           .validation_result(errors)
         }, contains="ROI")

#' ROISurfaceVector
#'
#' A class that represents a surface-based region of interest
#'
#' @slot geometry the geometry of the parent surface: a \code{SurfaceGeometry} instance
#' @slot data \code{matrix} data stored in ROI with number of columns equal to number of coordinates in ROI.
#' @slot coords the surface-based coordinates of the data
#' @slot indices the nodes of the parent surface stored in the \code{geometry} field.
#'
#' @details
#' The ROISurfaceVector class extends the concept of ROISurface to handle multiple measurements
#' for each vertex in the region of interest. While ROISurface stores a single value per vertex,
#' ROISurfaceVector stores a matrix of values where each row represents a different measure and
#' each column corresponds to a vertex.
#'
#' This structure is particularly useful for multivariate analyses of specific brain regions,
#' allowing researchers to:
#' \itemize{
#'   \item Store time series data for each vertex in an ROI
#'   \item Maintain multiple modalities of data for the same surface region
#'   \item Perform multivariate statistical analyses on regional surface data
#'   \item Compare different metrics within the same anatomical region
#' }
#'
#' The data matrix is organized with rows representing different measures and columns
#' representing different vertices, which facilitates efficient access to all measurements
#' for a particular vertex or all vertices for a particular measurement.
#'
#' @examples
#' \donttest{
#' # Create a simple tetrahedron mesh for the example
#' vertices <- c(
#'   0, 0, 0,
#'   1, 0, 0,
#'   0, 1, 0,
#'   0, 0, 1
#' )
#' triangles <- c(
#'   1, 2, 3,
#'   1, 2, 4,
#'   1, 3, 4,
#'   2, 3, 4
#' )
#'
#' # Create mesh3d object
#' mesh <- rgl::mesh3d(vertices = vertices, triangles = triangles)
#'
#' # Create a graph representation
#' edges <- rbind(
#'   c(1,2), c(1,3), c(1,4),
#'   c(2,3), c(2,4),
#'   c(3,4)
#' )
#' graph <- igraph::graph_from_edgelist(edges)
#'
#' # Create a SurfaceGeometry object
#' geometry <- new("SurfaceGeometry",
#'                mesh = mesh,
#'                graph = graph,
#'                hemi = "left")
#'
#' # Define the ROI - just using vertices 1 and 2 as an example
#' roi_indices <- c(1L, 2L)
#'
#' # Extract coordinates for these vertices
#' roi_coords <- matrix(
#'   c(0, 0, 0,  # coordinates for vertex 1
#'     1, 0, 0), # coordinates for vertex 2
#'   ncol = 3, byrow = TRUE
#' )
#'
#' # Create data matrix for the ROI vertices - 3 different measures
#' # Each row represents a different measure, each column a different vertex
#' roi_data <- matrix(
#'   c(0.75, 1.25,  # values for measure 1
#'     0.50, 0.80,  # values for measure 2
#'     0.30, 0.60), # values for measure 3
#'   nrow = 3, ncol = 2
#' )
#'
#' # Create the ROISurfaceVector object
#' roi_vector <- new("ROISurfaceVector",
#'                  geometry = geometry,
#'                  data = roi_data,
#'                  coords = roi_coords,
#'                  indices = roi_indices)
#' }
#'
#' @return An object of class \code{ROISurfaceVector}.
#'
#' @exportClass ROISurfaceVector
#' @rdname ROISurfaceVector-class
setClass("ROISurfaceVector",
         representation=representation(geometry="SurfaceGeometry", data="matrix",
                                       coords="matrix", indices="integer"),
         validity = function(object) {
           errors <- character()
           if (ncol(object@coords) != 3) {
             errors <- c(errors,
                         "'coords' must be a matrix with 3 columns")
           }
           if (nrow(object@coords) != length(object@indices)) {
             errors <- c(errors,
                         "length of 'indices' must equal 'nrow(coords)'")
           }

           if (ncol(object@data) != nrow(object@coords)) {
             errors <- c(errors,
                         "'ncol(data)' must equal 'nrow(coords)'")
           }
           index_errors <- .surface_index_errors(object@indices,
                                                 object@geometry)
           errors <- c(errors, index_errors)
           if (!length(index_errors) && ncol(object@coords) == 3L &&
               nrow(object@coords) == length(object@indices)) {
             expected <- t(object@geometry@mesh$vb[
               1:3, object@indices, drop = FALSE
             ])
             if (!isTRUE(all.equal(object@coords, expected,
                                   check.attributes = FALSE))) {
               errors <- c(errors,
                           "'coords' must match geometry at 'indices'")
             }
           }
           .validation_result(errors)
         }, contains="ROI")


#' VertexData
#'
#' A set of arbitrary vertices associated with a data table
#'
#' @rdname VertexData-class
#' @slot indices the node indices
#' @slot data the associated table with \code{nrow(data) == length(indices)}
#'
#' @details
#' The VertexData class provides a flexible way to associate arbitrary data with specific vertices
#' in a brain surface. Unlike ROISurface and similar classes, VertexData does not require or
#' maintain a reference to surface geometry, making it lighter and more versatile for storing
#' and manipulating vertex-specific information.
#'
#' This class is particularly useful for:
#' \itemize{
#'   \item Storing heterogeneous data (different data types) associated with vertices
#'   \item Maintaining analysis results like statistical outcomes for specific vertices
#'   \item Tracking vertex-specific annotations or classifications
#'   \item Creating lookup tables for vertex properties that can be joined with other data
#' }
#'
#' The \code{data} slot contains a data frame where each row corresponds to a vertex
#' in the same order as the \code{indices} slot. This structure allows storing multiple attributes
#' of different types (numeric, character, logical) for each vertex, and facilitates
#' standard data frame operations like filtering, sorting, and joining.
#'
#' @examples
#' \donttest{
#' # Create a set of vertex indices
#' vertex_indices <- c(10L, 25L, 50L, 100L, 200L)
#'
#' # Create a data frame with information for each vertex
#' # Each row corresponds to one vertex in the same order as indices
#' vertex_data <- data.frame(
#'   value = c(0.5, 1.2, 0.8, 1.5, 0.3),
#'   label = c("A", "B", "A", "C", "B"),
#'   significant = c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' )
#'
#' # Create the VertexData object
#' vd <- new("VertexData",
#'          indices = vertex_indices,
#'          data = vertex_data)
#'
#' # Access the data
#' # vd@data$value
#' # vd@data$label[vd@data$significant]
#' # vd@indices[vd@data$label == "A"]
#' }
#'
#' @return An object of class \code{VertexData}.
#'
#' @export
setClass("VertexData",
         representation=representation(indices="integer",
                                       data="data.frame"),
         validity = function(object) {
           errors <- character()
           if (nrow(object@data) != length(object@indices)) {
             errors <- c(errors,
                         "'nrow(data)' must equal length of 'indices'")
           }
           if (anyNA(object@indices) ||
               any(object@indices < 1L, na.rm = TRUE)) {
             errors <- c(errors,
                         "'indices' must be positive and non-missing")
           }
           if (anyDuplicated(object@indices)) {
             errors <- c(errors, "'indices' must not contain duplicates")
           }
           .validation_result(errors)
         })



#' NeuroSurface
#'
#' a three-dimensional surface consisting of a set of triangle vertices with one value per vertex.
#'
#' @rdname NeuroSurface-class
#' @slot geometry the surface geometry, an instance of \code{SurfaceGeometry} or \code{SurfaceSet}
#' @slot indices an \code{integer} vector specifying the subset of valid surface nodes encoded in the \code{geometry} object.
#' @slot data the 1-D vector of data value at each vertex of the mesh
#'
#' @details
#' The NeuroSurface class is a fundamental representation of surface-based neuroimaging data.
#' It combines geometric information about a brain surface with data values mapped to
#' each vertex of that surface.
#'
#' The class consists of three core components:
#' \itemize{
#'   \item \code{geometry}: The underlying 3D surface structure, containing vertex coordinates,
#'      face definitions, and topological information
#'   \item \code{indices}: Identifiers for the subset of vertices in the geometry that have
#'      associated data values
#'   \item \code{data}: A numeric vector containing one value per vertex, representing
#'      measurements such as cortical thickness, functional activation, or any other
#'      surface-mapped metric
#' }
#'
#' This class serves as the foundation for more specialized surface representations like
#' \code{ColorMappedNeuroSurface}, \code{VertexColoredNeuroSurface}, and \code{LabeledNeuroSurface}.
#' It facilitates common operations such as visualization, statistical analysis, and spatial
#' processing of surface-based neuroimaging data.
#'
#' @examples
#' \donttest{
#' # Create a simple tetrahedron mesh for the example
#' vertices <- c(
#'   0, 0, 0,
#'   1, 0, 0,
#'   0, 1, 0,
#'   0, 0, 1
#' )
#' triangles <- c(
#'   1, 2, 3,
#'   1, 2, 4,
#'   1, 3, 4,
#'   2, 3, 4
#' )
#'
#' # Create mesh3d object
#' mesh <- rgl::mesh3d(vertices = vertices, triangles = triangles)
#'
#' # Create a graph representation
#' edges <- rbind(
#'   c(1,2), c(1,3), c(1,4),
#'   c(2,3), c(2,4),
#'   c(3,4)
#' )
#' graph <- igraph::graph_from_edgelist(edges)
#'
#' # Create a SurfaceGeometry object
#' geometry <- new("SurfaceGeometry",
#'                mesh = mesh,
#'                graph = graph,
#'                hemi = "left")
#'
#' # Define indices for all vertices
#' indices <- 1:4
#'
#' # Create data values for each vertex
#' vertex_data <- c(0.5, 1.2, 0.8, 1.5)  # example values for the vertices
#'
#' # Create the NeuroSurface object
#' neuro_surface <- new("NeuroSurface",
#'                     geometry = geometry,
#'                     indices = indices,
#'                     data = vertex_data)
#'
#' # The data values are now mapped to the surface vertices
#' # and can be visualized or analyzed
#' }
#'
#' @return An object of class \code{NeuroSurface}.
#'
#' @export
setClass("NeuroSurface",
         slots=c(geometry="ANY",
                 indices="integer",
                 data="numeric"),
         validity = function(object) {
           errors <- character()
           if (!is_surface_like(object@geometry)) {
             errors <- c(errors,
                         "'geometry' must be a SurfaceGeometry or SurfaceSet")
           }
           if (length(object@data) != length(object@indices)) {
             errors <- c(errors,
                         "length of 'data' must equal length of 'indices'")
           }
           errors <- c(
             errors,
             .surface_index_errors(object@indices, object@geometry)
           )
           .validation_result(errors)
         })

#' ColorMappedNeuroSurface
#'
#' A three-dimensional surface consisting of a set of triangle vertices with one value per vertex,
#' mapped to colors using a specified colormap and range.
#'
#' @rdname ColorMappedNeuroSurface-class
#' @slot geometry The surface geometry, an instance of \code{SurfaceGeometry} or \code{SurfaceSet}
#' @slot indices An \code{integer} vector specifying the subset of valid surface nodes encoded in the \code{geometry} object
#' @slot data The 1-D vector of data values at each vertex of the mesh
#' @slot cmap A character vector of hex color codes representing the colormap
#' @slot irange A numeric vector of length 2 specifying the low and high values for color mapping
#' @slot thresh A numeric vector of length 2 specifying the low and high thresholds for visibility
#'
#' @details
#' This class extends \code{NeuroSurface} by adding color mapping functionality.
#' The \code{cmap} slot contains a vector of hex color codes that define the colormap.
#' The \code{irange} slot specifies the range of data values to be mapped to the colormap.
#' The \code{thresh} slot defines the visibility thresholds: data values below \code{thresh[1]}
#' or above \code{thresh[2]} are visible, while values in between are not visible.
#'
#' The color mapping process works as follows:
#' \enumerate{
#'   \item Data values are linearly mapped to the range [0,1] based on \code{irange}
#'   \item These normalized values are used to interpolate colors from the \code{cmap}
#'   \item Values falling between the thresholds in \code{thresh} are marked as invisible
#' }
#'
#' This approach is particularly useful for visualizing statistical maps (e.g., t-statistics)
#' where researchers are often interested in highlighting values above or below certain
#' significance thresholds.
#'
#' @seealso \code{\link{view_surface}}, \code{\link{plot-methods}}
#'
#' @return An object of class \code{ColorMappedNeuroSurface}
#'
#' @examples
#' \donttest{
#' # First create a simple tetrahedron mesh
#' vertices <- c(
#'   0, 0, 0,
#'   1, 0, 0,
#'   0, 1, 0,
#'   0, 0, 1
#' )
#' triangles <- c(
#'   1, 2, 3,
#'   1, 2, 4,
#'   1, 3, 4,
#'   2, 3, 4
#' )
#'
#' # Create mesh3d object
#' mesh <- rgl::mesh3d(vertices = vertices, triangles = triangles)
#'
#' # Create a graph representation
#' edges <- rbind(
#'   c(1,2), c(1,3), c(1,4),
#'   c(2,3), c(2,4),
#'   c(3,4)
#' )
#' graph <- igraph::graph_from_edgelist(edges)
#'
#' # Create a SurfaceGeometry object
#' geometry <- new("SurfaceGeometry",
#'                mesh = mesh,
#'                graph = graph,
#'                hemi = "left")
#'
#' # Define indices for all vertices
#' indices <- 1:4
#'
#' # Create data values for each vertex
#' vertex_data <- c(-1.5, -0.5, 0.8, 2.0)  # example values for the vertices
#'
#' # Define a simple colormap (blue to red)
#' colormap <- c("#0000FF", "#FFFFFF", "#FF0000")
#'
#' # Define intensity range for mapping data to colors
#' intensity_range <- c(-2.0, 2.0)
#'
#' # Define thresholds (values between -0.5 and 0.5 will be invisible)
#' thresholds <- c(-0.5, 0.5)
#'
#' # Create the ColorMappedNeuroSurface object
#' colored_surface <- new("ColorMappedNeuroSurface",
#'                       geometry = geometry,
#'                       indices = indices,
#'                       data = vertex_data,
#'                       cmap = colormap,
#'                       irange = intensity_range,
#'                       thresh = thresholds)
#'
#' # In this example:
#' # - Vertex 1 (-1.5) will be visible and colored blue (below lower threshold)
#' # - Vertex 2 (-0.5) will be exactly at the lower threshold
#' # - Vertex 3 (0.8) will be visible and colored light red (above upper threshold)
#' # - Vertex 4 (2.0) will be visible and colored bright red (above upper threshold)
#' }
#'
#' @export
setClass("ColorMappedNeuroSurface",
         slots = c(
           cmap = "character",
           irange = "numeric",
           thresh = "numeric"
         ),
         contains = "NeuroSurface",
         validity = function(object) {
           errors <- character()
           if (length(object@irange) != 2) {
             errors <- c(errors,
                         "'irange' must be a numeric vector of length 2")
           } else if (anyNA(object@irange) ||
                      any(!is.finite(object@irange))) {
             errors <- c(errors, "'irange' must contain finite values")
           } else if (object@irange[1] > object@irange[2]) {
             errors <- c(errors,
                         "'irange[1]' must be less than or equal to 'irange[2]'")
           }
           if (length(object@thresh) != 2) {
             errors <- c(errors,
                         "'thresh' must be a numeric vector of length 2")
           } else if (anyNA(object@thresh)) {
             errors <- c(errors, "'thresh' must not contain missing values")
           } else if (object@thresh[1] > object@thresh[2]) {
             errors <- c(errors,
                         "'thresh[1]' must be less than or equal to 'thresh[2]'")
           }
           if (length(object@cmap) < 2) {
             errors <- c(errors, "'cmap' must contain at least 2 colors")
           } else if (!.valid_hex_colors(object@cmap)) {
             errors <- c(errors,
                         "all elements in 'cmap' must be valid hex colors")
           }
           .validation_result(errors)
         })

#' VertexColoredNeuroSurface
#'
#' A three-dimensional surface consisting of a set of triangle vertices with one color per vertex.
#'
#' @rdname VertexColoredNeuroSurface-class
#' @slot geometry The surface geometry, an instance of \code{SurfaceGeometry} or \code{SurfaceSet}
#' @slot indices An \code{integer} vector specifying the subset of valid surface nodes encoded in the \code{geometry} object
#' @slot colors A character vector of hex color codes representing the color of each vertex
#'
#' @examples
#' \donttest{
#' # First create a simple tetrahedron mesh
#' vertices <- c(
#'   0, 0, 0,
#'   1, 0, 0,
#'   0, 1, 0,
#'   0, 0, 1
#' )
#' triangles <- c(
#'   1, 2, 3,
#'   1, 2, 4,
#'   1, 3, 4,
#'   2, 3, 4
#' )
#'
#' # Create mesh3d object
#' mesh <- rgl::mesh3d(vertices = vertices, triangles = triangles)
#'
#' # Create a graph representation
#' edges <- rbind(
#'   c(1,2), c(1,3), c(1,4),
#'   c(2,3), c(2,4),
#'   c(3,4)
#' )
#' graph <- igraph::graph_from_edgelist(edges)
#'
#' # Create a SurfaceGeometry object
#' geometry <- new("SurfaceGeometry",
#'                mesh = mesh,
#'                graph = graph,
#'                hemi = "left")
#'
#' # Define indices for all vertices
#' indices <- 1:4
#'
#' # Create placeholder data (required by NeuroSurface parent class)
#' vertex_data <- c(0, 0, 0, 0)  # Not used for coloring
#'
#' # Define explicit colors for each vertex
#' vertex_colors <- c(
#'   "#FF0000",  # Red for vertex 1
#'   "#00FF00",  # Green for vertex 2
#'   "#0000FF",  # Blue for vertex 3
#'   "#FFFF00"   # Yellow for vertex 4
#' )
#'
#' # Create the VertexColoredNeuroSurface object
#' colored_vertices <- new("VertexColoredNeuroSurface",
#'                        geometry = geometry,
#'                        indices = indices,
#'                        data = vertex_data,
#'                        colors = vertex_colors)
#'
#' # In this example, each vertex has an explicit color:
#' # - Vertex 1 is red
#' # - Vertex 2 is green
#' # - Vertex 3 is blue
#' # - Vertex 4 is yellow
#' }
#'
#' @details
#' This class extends \code{NeuroSurface} by adding per-vertex coloring functionality.
#' The \code{colors} slot contains a vector of hex color codes that define the color for each vertex.
#' Unlike \code{ColorMappedNeuroSurface}, this class does not use a colormap or data mapping,
#' but instead directly specifies colors for each vertex.
#'
#' @seealso \code{\link{view_surface}}
#'
#' @return An object of class \code{VertexColoredNeuroSurface}.
#'
#' @export
setClass("VertexColoredNeuroSurface",
         slots = c(
           colors = "character"
         ),
         contains = "NeuroSurface",
         validity = function(object) {
           errors <- character()
           if (length(object@colors) != length(object@indices)) {
             errors <- c(errors,
                         "Length of 'colors' must equal length of 'indices'")
           }
           if (!.valid_hex_colors(object@colors)) {
             errors <- c(errors,
                         "All elements in 'colors' must be valid hex color codes")
           }
           .validation_result(errors)
         })




#' LabeledNeuroSurface Class
#'
#' @description
#' Represents a 3D surface with labeled vertices, extending NeuroSurface.
#'
#' @slot labels Character vector of label annotations
#' @slot cols Character vector of hex color codes for labels
#'
#' @examples
#' \donttest{
#' # Create a simple tetrahedron mesh for the example
#' vertices <- c(
#'   0, 0, 0,
#'   1, 0, 0,
#'   0, 1, 0,
#'   0, 0, 1
#' )
#' triangles <- c(
#'   1, 2, 3,
#'   1, 2, 4,
#'   1, 3, 4,
#'   2, 3, 4
#' )
#'
#' # Create mesh3d object
#' mesh <- rgl::mesh3d(vertices = vertices, triangles = triangles)
#'
#' # Create a graph representation
#' edges <- rbind(
#'   c(1,2), c(1,3), c(1,4),
#'   c(2,3), c(2,4),
#'   c(3,4)
#' )
#' graph <- igraph::graph_from_edgelist(edges)
#'
#' # Create a SurfaceGeometry object
#' geometry <- new("SurfaceGeometry",
#'                mesh = mesh,
#'                graph = graph,
#'                hemi = "left")
#'
#' # Define indices for all vertices
#' indices <- 1:4
#'
#' # Create data values for each vertex
#' vertex_data <- c(1, 2, 1, 3)  # Values representing region IDs
#'
#' # Define the unique labels for the regions
#' labels <- c("Region A", "Region B", "Region C")
#'
#' # Define colors for each label (in the same order as labels)
#' colors <- c("#FF0000", "#00FF00", "#0000FF")  # Red, Green, Blue
#'
#' # Create the LabeledNeuroSurface object
#' labeled_surface <- new("LabeledNeuroSurface",
#'                       geometry = geometry,
#'                       indices = indices,
#'                       data = vertex_data,
#'                       labels = labels,
#'                       cols = colors)
#'
#' # In this example, vertices are labeled as follows:
#' # - Vertices 1 and 3 belong to "Region A" (Red)
#' # - Vertex 2 belongs to "Region B" (Green)
#' # - Vertex 4 belongs to "Region C" (Blue)
#' }
#'
#' @details
#' The LabeledNeuroSurface class provides a way to represent anatomical parcellations
#' or other categorical divisions of a brain surface. Each vertex is assigned a label
#' based on its data value, which typically represents a region ID. The class maintains
#' a mapping between these IDs and human-readable labels, along with colors for visualization.
#'
#' This is particularly useful for displaying anatomical atlases or the results of clustering
#' algorithms on the brain surface.
#'
#' @seealso \code{\link{NeuroSurface}}
#'
#' @return An object of class \code{LabeledNeuroSurface}.
#'
#' @export
setClass("LabeledNeuroSurface",
         slots = c(labels = "character", cols = "character"),
         contains = c("NeuroSurface"),
         validity = function(object) {
           errors <- character()
           if (length(object@cols) != length(object@labels)) {
             errors <- c(errors, "Number of colors must match number of labels")
           }
           if (length(object@labels) < 1L || anyNA(object@labels) ||
               any(!nzchar(object@labels))) {
             errors <- c(errors,
                         "'labels' must contain non-empty, non-missing strings")
           }
           if (!.valid_hex_colors(object@cols)) {
             errors <- c(errors,
                         "all elements in 'cols' must be valid hex colors")
           }
           .validation_result(errors)
         })

#' NeuroSurfaceVector Class
#'
#' @description
#' Represents a 3D surface with multiple values per vertex.
#'
#' @slot geometry SurfaceGeometry instance representing the surface structure
#' @slot indices Integer vector of valid surface node indices
#' @slot data Matrix of values, where columns represent different measures and rows correspond to surface nodes
#'
#' @details
#' The NeuroSurfaceVector class extends the concept of NeuroSurface to handle multiple measurements
#' for each vertex across the entire surface. Unlike NeuroSurface which stores a single value per vertex,
#' NeuroSurfaceVector stores a matrix of values where columns represent different measures and rows
#' correspond to vertices.
#'
#' This structure is particularly useful for:
#' \itemize{
#'   \item Time series data where each column represents a different timepoint
#'   \item Multi-modal data where each column represents a different imaging modality
#'   \item Statistical results where columns represent different statistical parameters
#'   \item Feature vectors for machine learning applications
#' }
#'
#' The data matrix organization (vertices as rows, measures as columns) facilitates efficient
#' vertex-wise operations and analyses. This is in contrast to ROISurfaceVector where the matrix
#' is transposed (measures as rows, vertices as columns).
#'
#' @examples
#' \donttest{
#' # Create a simple tetrahedron mesh for the example
#' vertices <- c(
#'   0, 0, 0,
#'   1, 0, 0,
#'   0, 1, 0,
#'   0, 0, 1
#' )
#' triangles <- c(
#'   1, 2, 3,
#'   1, 2, 4,
#'   1, 3, 4,
#'   2, 3, 4
#' )
#'
#' # Create mesh3d object
#' mesh <- rgl::mesh3d(vertices = vertices, triangles = triangles)
#'
#' # Create a graph representation
#' edges <- rbind(
#'   c(1,2), c(1,3), c(1,4),
#'   c(2,3), c(2,4),
#'   c(3,4)
#' )
#' graph <- igraph::graph_from_edgelist(edges)
#'
#' # Create a SurfaceGeometry object
#' geometry <- new("SurfaceGeometry",
#'                mesh = mesh,
#'                graph = graph,
#'                hemi = "left")
#'
#' # Define indices for all vertices
#' indices <- 1:4
#'
#' # Create a Matrix with multiple measures for each vertex
#' # Each row corresponds to a vertex, each column to a different measure
#' require(Matrix)
#' vertex_data <- Matrix(
#'   c(0.5, 1.2, 0.8,    # Measure 1 values for vertices 1-4
#'     0.7, 0.3, 1.5, 0.9,  # Measure 2 values for vertices 1-4
#'     1.1, 0.6, 0.4, 1.3), # Measure 3 values for vertices 1-4
#'   nrow = 4, ncol = 3,
#'   byrow = FALSE
#' )
#'
#' # Create the NeuroSurfaceVector object
#' neuro_surface_vector <- new("NeuroSurfaceVector",
#'                          geometry = geometry,
#'                          indices = indices,
#'                          data = vertex_data)
#'
#' # The data matrix now maps multiple values to each surface vertex
#' # Vertex 1 has values: 0.5, 0.7, 1.1
#' # Vertex 2 has values: 1.2, 0.3, 0.6
#' # etc.
#' }
#'
#' @note The number of rows in 'data' must match the number of nodes in 'geometry'.
#'
#' @seealso \code{\link{SurfaceGeometry}}, \code{\link{NeuroSurface}}
#'
#' @return An object of class \code{NeuroSurfaceVector}
#'
#' @export
setClass("NeuroSurfaceVector",
         representation = representation(
           geometry = "ANY",
           indices = "integer",
           data = "Matrix"
         ),
         validity = function(object) {
           errors <- character()
           if (!is_surface_like(object@geometry)) {
             errors <- c(errors,
                         "'geometry' must be a SurfaceGeometry or SurfaceSet")
           }
           errors <- c(
             errors,
             .surface_index_errors(object@indices, object@geometry)
           )
           vertex_count <- .surface_vertex_count(object@geometry)
           if (!is.na(vertex_count) && nrow(object@data) != vertex_count) {
             errors <- c(errors,
                         "'nrow(data)' must equal the geometry vertex count")
           }
           .validation_result(errors)
         })


#' Bilateral NeuroSurface Vector Class
#'
#' @description
#' Represents surface data for both left and right hemispheres.
#'
#' @slot left NeuroSurfaceVector instance for the left hemisphere
#' @slot right NeuroSurfaceVector instance for the right hemisphere
#'
#' @details
#' The BilatNeuroSurfaceVector class provides a convenient container for organizing and
#' analyzing data from both hemispheres of the brain simultaneously. It holds two
#' NeuroSurfaceVector objects, one for each hemisphere, allowing researchers to:
#'
#' \itemize{
#'   \item Analyze bilateral symmetric or asymmetric patterns in brain data
#'   \item Compare corresponding regions across hemispheres
#'   \item Represent whole-brain surface data with proper hemisphere separation
#'   \item Apply operations to both hemispheres while maintaining their distinct identities
#' }
#'
#' This class is particularly useful for studies examining interhemispheric differences,
#' bilateral effects, or any analysis that requires maintaining separate representations
#' of the two hemispheres while treating them as parts of a unified whole.
#'
#' @examples
#' \donttest{
#' # Create two simple tetrahedron meshes (one for each hemisphere)
#' # Left hemisphere
#' lh_vertices <- c(
#'   0, 0, 0,
#'   -1, 0, 0,
#'   0, 1, 0,
#'   0, 0, 1
#' )
#' # Right hemisphere
#' rh_vertices <- c(
#'   0, 0, 0,
#'   1, 0, 0,
#'   0, 1, 0,
#'   0, 0, 1
#' )
#'
#' triangles <- c(
#'   1, 2, 3,
#'   1, 2, 4,
#'   1, 3, 4,
#'   2, 3, 4
#' )
#'
#' # Create mesh3d objects
#' lh_mesh <- rgl::mesh3d(vertices = lh_vertices, triangles = triangles)
#' rh_mesh <- rgl::mesh3d(vertices = rh_vertices, triangles = triangles)
#'
#' # Create graph representations
#' edges <- rbind(
#'   c(1,2), c(1,3), c(1,4),
#'   c(2,3), c(2,4),
#'   c(3,4)
#' )
#' lh_graph <- igraph::graph_from_edgelist(edges)
#' rh_graph <- igraph::graph_from_edgelist(edges)
#'
#' # Create SurfaceGeometry objects
#' lh_geometry <- new("SurfaceGeometry",
#'                   mesh = lh_mesh,
#'                   graph = lh_graph,
#'                   hemi = "left")
#' rh_geometry <- new("SurfaceGeometry",
#'                   mesh = rh_mesh,
#'                   graph = rh_graph,
#'                   hemi = "right")
#'
#' # Define indices for all vertices
#' indices <- 1:4
#'
#' # Create Matrix data for each hemisphere
#' # Each has 4 vertices and 2 measures
#' require(Matrix)
#' lh_data <- Matrix(
#'   c(0.5, 1.2, 0.8, 0.6,   # Measure 1 values
#'     0.7, 0.3, 1.5, 0.9),  # Measure 2 values
#'   nrow = 4, ncol = 2,
#'   byrow = FALSE
#' )
#'
#' rh_data <- Matrix(
#'   c(0.4, 1.1, 0.7, 0.5,   # Measure 1 values (slightly different from left)
#'     0.8, 0.4, 1.6, 1.0),  # Measure 2 values (slightly different from left)
#'   nrow = 4, ncol = 2,
#'   byrow = FALSE
#' )
#'
#' # Create NeuroSurfaceVector objects for each hemisphere
#' lh_nsv <- new("NeuroSurfaceVector",
#'              geometry = lh_geometry,
#'              indices = indices,
#'              data = lh_data)
#'
#' rh_nsv <- new("NeuroSurfaceVector",
#'              geometry = rh_geometry,
#'              indices = indices,
#'              data = rh_data)
#'
#' # Create the BilatNeuroSurfaceVector object
#' bilat_nsv <- new("BilatNeuroSurfaceVector",
#'                 left = lh_nsv,
#'                 right = rh_nsv)
#'
#' # Now you can access each hemisphere separately:
#' # bilat_nsv@left@data  # Left hemisphere data
#' # bilat_nsv@right@data # Right hemisphere data
#' }
#'
#' @seealso \code{\link{NeuroSurfaceVector}}
#'
#' @return An object of class \code{BilatNeuroSurfaceVector}.
#'
#' @export
setClass("BilatNeuroSurfaceVector",
  representation = representation(
    left = "NeuroSurfaceVector",
    right = "NeuroSurfaceVector"
  ),
  validity = function(object) {
    errors <- character()
    if (ncol(object@left@data) != ncol(object@right@data)) {
      errors <- c(errors,
                  "left and right data must have the same number of columns")
    }

    left_geometry <- .surface_geometry_for_validation(object@left@geometry)
    right_geometry <- .surface_geometry_for_validation(object@right@geometry)
    if (is.null(left_geometry) ||
        !identical(.hemisphere_side(left_geometry@hemi), "left")) {
      errors <- c(errors,
                  "'left' geometry must identify the left hemisphere")
    }
    if (is.null(right_geometry) ||
        !identical(.hemisphere_side(right_geometry@hemi), "right")) {
      errors <- c(errors,
                  "'right' geometry must identify the right hemisphere")
    }
    .validation_result(errors)
  }
)
