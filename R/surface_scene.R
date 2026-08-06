#' A portable cortical surface scene
#'
#' \code{SurfaceScene} stores one or two hemisphere geometries and named scalar
#' layers together with the presentation metadata needed to reconstruct a
#' report viewer. Use \code{\link{surface_scene}} to create validated instances.
#'
#' @slot id A stable scene identifier.
#' @slot geometries A named list of \code{SurfaceGeometry} objects.
#' @slot curvature Optional named curvature vectors.
#' @slot layers A named list of validated surface-layer specifications.
#' @slot selected_layer The initially selected layer name.
#' @slot metadata Arbitrary scene metadata.
#' @slot provenance Arbitrary scene provenance.
#' @slot fallback Plain-text content shown when JavaScript or WebGL is absent.
#' @slot alt_text Alternative text for the interactive figure.
#' @slot preset A visual preset such as \code{"paper-light"}.
#' @slot mode Widget behavior mode. \code{"report"} enables curated controls.
#' @slot asset_mode Default asset serialization mode.
#'
#' @return A \code{SurfaceScene} object.
#' @exportClass SurfaceScene
setClass(
  "SurfaceScene",
  slots = c(
    id = "character",
    geometries = "list",
    curvature = "list",
    layers = "list",
    selected_layer = "character",
    metadata = "list",
    provenance = "list",
    fallback = "character",
    alt_text = "character",
    preset = "character",
    mode = "character",
    asset_mode = "character"
  ),
  validity = function(object) {
    errors <- character()
    if (length(object@id) != 1L || !nzchar(object@id)) {
      errors <- c(errors, "'id' must be one non-empty string")
    }
    if (length(object@geometries) < 1L || length(object@geometries) > 2L) {
      errors <- c(errors, "'geometries' must contain one or two hemispheres")
    }
    if (!identical(names(object@geometries), names(object@curvature))) {
      errors <- c(errors, "curvature names must match geometry names")
    }
    if (length(object@layers) < 1L || anyDuplicated(names(object@layers))) {
      errors <- c(errors, "'layers' must contain uniquely named layers")
    }
    if (length(object@selected_layer) != 1L ||
        !object@selected_layer %in% names(object@layers)) {
      errors <- c(errors, "'selected_layer' must name a scene layer")
    }
    if (length(object@fallback) != 1L || !nzchar(trimws(object@fallback))) {
      errors <- c(errors, "'fallback' must be one non-empty string")
    }
    if (length(object@alt_text) != 1L || !nzchar(trimws(object@alt_text))) {
      errors <- c(errors, "'alt_text' must be one non-empty string")
    }
    if (!object@mode %in% c("report", "viewer")) {
      errors <- c(errors, "'mode' must be 'report' or 'viewer'")
    }
    if (!object@asset_mode %in% c("inline", "directory")) {
      errors <- c(errors, "'asset_mode' must be 'inline' or 'directory'")
    }
    if (length(errors)) errors else TRUE
  }
)

.scene_nonempty_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop("'", name, "' must be one non-empty string", call. = FALSE)
  }
  x
}

.scene_list <- function(x, name) {
  if (!is.list(x)) {
    stop("'", name, "' must be a list", call. = FALSE)
  }
  x
}

.scene_record <- function(x, name) {
  x <- .scene_list(x, name)
  if (length(x) && (is.null(names(x)) || any(!nzchar(names(x))) ||
                    anyDuplicated(names(x)))) {
    stop("'", name, "' must be an empty or uniquely named list", call. = FALSE)
  }
  x
}

.scene_hemi <- function(x) {
  key <- tolower(trimws(x))
  if (key %in% c("l", "lh", "left")) return("left")
  if (key %in% c("r", "rh", "right")) return("right")
  stop(
    "hemisphere must be left/lh or right/rh; received '", x, "'",
    call. = FALSE
  )
}

.scene_geometry <- function(x, expected) {
  if (!is(x, "SurfaceGeometry") && !is(x, "SurfaceSet")) {
    stop("'", expected, "' must be a SurfaceGeometry or SurfaceSet", call. = FALSE)
  }
  geometry <- resolve_surface_geometry(x)
  actual <- .scene_hemi(geometry@hemi)
  if (!identical(actual, expected)) {
    stop(
      "geometry supplied as '", expected, "' declares hemisphere '",
      geometry@hemi, "'",
      call. = FALSE
    )
  }
  geometry
}

.scene_mapping_names <- function(x) {
  vapply(names(x), .scene_hemi, character(1))
}

.scene_mapping <- function(x, geometry_ids, what, allow_null = FALSE) {
  if (is.null(x) && allow_null) {
    return(stats::setNames(rep(list(NULL), length(geometry_ids)), geometry_ids))
  }
  if (is.numeric(x) && length(geometry_ids) == 1L) {
    return(stats::setNames(list(x), geometry_ids))
  }
  if (!is.list(x) || is.null(names(x)) || any(!nzchar(names(x)))) {
    stop(
      "'", what, "' must be numeric for a unilateral scene or a named list",
      call. = FALSE
    )
  }
  normalized <- .scene_mapping_names(x)
  if (anyDuplicated(normalized) || !setequal(normalized, geometry_ids)) {
    stop(
      "'", what, "' names must match scene hemispheres: ",
      paste(geometry_ids, collapse = ", "),
      call. = FALSE
    )
  }
  names(x) <- normalized
  x[geometry_ids]
}

.scene_limits <- function(limits, values, layer_name) {
  if (is.null(limits)) {
    finite <- unlist(values, use.names = FALSE)
    finite <- finite[is.finite(finite)]
    if (!length(finite)) {
      stop(
        "layer '", layer_name,
        "' has no finite values; supply explicit limits",
        call. = FALSE
      )
    }
    limits <- range(finite)
  }
  if (!is.numeric(limits) || length(limits) != 2L ||
      any(!is.finite(limits)) || limits[1] > limits[2]) {
    stop("layer '", layer_name, "' limits must be an ordered finite pair", call. = FALSE)
  }
  as.numeric(limits)
}

#' Define a named scalar layer for a surface scene
#'
#' @param name Stable layer name.
#' @param values A numeric vector for a unilateral scene, or a named list with
#'   one numeric vector per hemisphere.
#' @param indices Optional 1-based vertex indices with the same structure and
#'   lengths as \code{values}. Omit for full-vertex maps.
#' @param colormap A surfview colormap name or a character vector of colors.
#' @param limits Optional finite display limits. By default they are computed
#'   from finite values across hemispheres.
#' @param opacity Numeric scalar between zero and one.
#' @param units Optional measurement units.
#' @param legend A list with optional \code{title}, \code{units},
#'   \code{visible}, and \code{metadata} fields.
#' @param metadata,provenance Arbitrary lists carried into the manifest.
#' @param visible Whether this layer is a candidate for initial selection.
#' @param threshold Optional static threshold pair. This preserves explicit
#'   legacy display thresholds; report mode does not add a threshold control.
#'
#' @return A validated layer specification for \code{\link{surface_scene}}.
#'
#' @details
#' The browser renders these values as supplied. Apply inferential thresholds,
#' tail selection, capping, and atlas projection in R. \code{threshold} preserves an
#' explicitly authored display threshold for legacy compatibility; report mode
#' does not add an exploratory threshold control.
#'
#' @examples
#' surface_layer(
#'   "effect", c(-1.5, 0, 2),
#'   colormap = c("#2166ac", "#f7f7f7", "#b2182b"),
#'   limits = c(-2, 2), units = "z"
#' )
#' @export
surface_layer <- function(name, values, indices = NULL, colormap = "viridis",
                          limits = NULL, opacity = 1, units = NULL,
                          legend = list(), metadata = list(),
                          provenance = list(), visible = TRUE,
                          threshold = NULL) {
  name <- .scene_nonempty_string(name, "name")
  if (!grepl("^[A-Za-z][A-Za-z0-9_.-]*$", name)) {
    stop(
      "layer name must start with a letter and contain only letters, digits, '.', '_', or '-'",
      call. = FALSE
    )
  }
  if (!(is.character(colormap) && length(colormap) >= 1L &&
        all(!is.na(colormap)) && all(nzchar(colormap)))) {
    stop("'colormap' must be a name or non-empty character vector", call. = FALSE)
  }
  if (!is.numeric(opacity) || length(opacity) != 1L ||
      !is.finite(opacity) || opacity < 0 || opacity > 1) {
    stop("'opacity' must be a finite scalar between 0 and 1", call. = FALSE)
  }
  if (!is.null(units)) .scene_nonempty_string(units, "units")
  legend <- .scene_record(legend, "legend")
  metadata <- .scene_record(metadata, "metadata")
  provenance <- .scene_record(provenance, "provenance")
  if (!is.logical(visible) || length(visible) != 1L || is.na(visible)) {
    stop("'visible' must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.null(legend$visible) &&
      (!is.logical(legend$visible) || length(legend$visible) != 1L ||
       is.na(legend$visible))) {
    stop("'legend$visible' must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.null(threshold) &&
      (!is.numeric(threshold) || length(threshold) != 2L ||
       any(!is.finite(threshold)) || threshold[1] > threshold[2])) {
    stop("'threshold' must be an ordered finite pair", call. = FALSE)
  }
  structure(
    list(
      name = name,
      values = values,
      indices = indices,
      colormap = colormap,
      limits = limits,
      opacity = as.numeric(opacity),
      units = units,
      legend = legend,
      metadata = metadata,
      provenance = provenance,
      visible = visible,
      threshold = threshold
    ),
    class = "SurfaceLayer"
  )
}

.scene_validate_layer <- function(layer, geometries) {
  if (!inherits(layer, "SurfaceLayer")) {
    stop("every layer must be created by surface_layer()", call. = FALSE)
  }
  geometry_ids <- names(geometries)
  values <- .scene_mapping(layer$values, geometry_ids, paste0(layer$name, " values"))
  indices <- .scene_mapping(
    layer$indices,
    geometry_ids,
    paste0(layer$name, " indices"),
    allow_null = TRUE
  )
  for (geometry_id in geometry_ids) {
    layer_values <- values[[geometry_id]]
    layer_indices <- indices[[geometry_id]]
    vertex_count <- nrow(coords(geometries[[geometry_id]]))
    if (!is.numeric(layer_values)) {
      stop("layer '", layer$name, "' values must be numeric", call. = FALSE)
    }
    if (is.null(layer_indices)) {
      if (length(layer_values) != vertex_count) {
        stop(
          "layer '", layer$name, "' has ", length(layer_values),
          " values for ", vertex_count, " ", geometry_id, " vertices",
          call. = FALSE
        )
      }
    } else {
      if (!is.numeric(layer_indices) || any(!is.finite(layer_indices)) ||
          any(layer_indices != floor(layer_indices))) {
        stop("layer '", layer$name, "' indices must be whole numbers", call. = FALSE)
      }
      if (length(layer_values) != length(layer_indices)) {
        stop("layer '", layer$name, "' values and indices must have equal length", call. = FALSE)
      }
      if (any(layer_indices < 1L | layer_indices > vertex_count)) {
        stop("layer '", layer$name, "' indices are outside the geometry", call. = FALSE)
      }
      if (anyDuplicated(layer_indices)) {
        stop("layer '", layer$name, "' indices must be unique", call. = FALSE)
      }
      indices[[geometry_id]] <- as.integer(layer_indices)
    }
    values[[geometry_id]] <- as.numeric(layer_values)
  }
  layer$values <- values
  layer$indices <- indices
  layer$limits <- .scene_limits(layer$limits, values, layer$name)
  if (is.null(layer$legend$title)) layer$legend$title <- layer$name
  if (is.null(layer$legend$visible)) layer$legend$visible <- TRUE
  layer
}

.scene_curvature <- function(curvature, geometries) {
  ids <- names(geometries)
  if (is.null(curvature)) {
    return(stats::setNames(rep(list(NULL), length(ids)), ids))
  }
  mapped <- .scene_mapping(curvature, ids, "curvature")
  for (id in ids) {
    if (!is.numeric(mapped[[id]]) ||
        length(mapped[[id]]) != nrow(coords(geometries[[id]]))) {
      stop("curvature for '", id, "' must have one numeric value per vertex", call. = FALSE)
    }
    mapped[[id]] <- as.numeric(mapped[[id]])
  }
  mapped
}

#' Construct a validated portable surface scene
#'
#' @param left,right Optional left and right \code{SurfaceGeometry} or
#'   \code{SurfaceSet}
#'   objects. Supply at least one.
#' @param layers A \code{\link{surface_layer}} object or a list of them.
#' @param curvature Optional numeric vector for a unilateral scene or named
#'   hemisphere list.
#' @param selected_layer Initially selected layer name. Defaults to the first
#'   visible layer, then the first layer.
#' @param id Stable scene identifier.
#' @param metadata,provenance Arbitrary lists carried into the manifest.
#' @param fallback Required plain-text fallback content.
#' @param alt_text Required alternative text for the interactive figure.
#' @param preset Visual appearance preset. \code{"paper-light"} is intended for
#'   light-background figures and does not change widget behavior.
#' @param mode \code{"report"} for curated controls and fallback behavior, or
#'   \code{"viewer"} for a bare interactive viewer.
#' @param asset_mode Default serialization mode: inline base64 or adjacent
#'   content-addressed files.
#'
#' @return A validated \code{SurfaceScene} object.
#'
#' @details
#' A scene owns the portable report description, not the statistical analysis.
#' \code{preset} changes appearance; \code{mode} changes viewer behavior. Inline and
#' adjacent asset modes encode the same typed-array bytes and preserve missing
#' values. Use \code{\link{surfwidget}} inside an R Markdown or Quarto document
#' and \code{\link{write_surface_scene}} for an ordinary HTML page.
#'
#' @examples
#' geometry <- example_surface_geometry()
#' scene <- surface_scene(
#'   left = geometry,
#'   layers = surface_layer(
#'     "effect", seq_len(nrow(coords(geometry))), limits = c(1, 4)
#'   ),
#'   fallback = "Static left-hemisphere surface figure.",
#'   alt_text = "Left cortical surface colored by an example effect."
#' )
#' scene
#'
#' @seealso \code{\link{surface_layer}}, \code{\link{surfwidget}},
#'   \code{\link{surface_scene_manifest}}, \code{\link{write_surface_scene}}
#' @export
surface_scene <- function(left = NULL, right = NULL, layers,
                          curvature = NULL, selected_layer = NULL,
                          id = "surface-scene", metadata = list(),
                          provenance = list(), fallback, alt_text,
                          preset = "paper-light",
                          mode = c("report", "viewer"),
                          asset_mode = c("inline", "directory")) {
  if (is.null(left) && is.null(right)) {
    stop("supply at least one of 'left' or 'right'", call. = FALSE)
  }
  geometries <- list()
  if (!is.null(left)) geometries$left <- .scene_geometry(left, "left")
  if (!is.null(right)) geometries$right <- .scene_geometry(right, "right")

  if (inherits(layers, "SurfaceLayer")) layers <- list(layers)
  if (!is.list(layers) || !length(layers)) {
    stop("'layers' must contain at least one surface_layer()", call. = FALSE)
  }
  layers <- lapply(layers, .scene_validate_layer, geometries = geometries)
  layer_names <- vapply(layers, `[[`, character(1), "name")
  if (anyDuplicated(layer_names)) {
    stop("layer names must be unique", call. = FALSE)
  }
  names(layers) <- layer_names
  if (is.null(selected_layer)) {
    visible <- layer_names[vapply(layers, `[[`, logical(1), "visible")]
    selected_layer <- if (length(visible)) visible[[1]] else layer_names[[1]]
  }
  .scene_nonempty_string(selected_layer, "selected_layer")
  if (!selected_layer %in% layer_names) {
    stop("'selected_layer' must name a scene layer", call. = FALSE)
  }

  scene <- new(
    "SurfaceScene",
    id = .scene_nonempty_string(id, "id"),
    geometries = geometries,
    curvature = .scene_curvature(curvature, geometries),
    layers = layers,
    selected_layer = selected_layer,
    metadata = .scene_record(metadata, "metadata"),
    provenance = .scene_record(provenance, "provenance"),
    fallback = .scene_nonempty_string(fallback, "fallback"),
    alt_text = .scene_nonempty_string(alt_text, "alt_text"),
    preset = .scene_nonempty_string(preset, "preset"),
    mode = match.arg(mode),
    asset_mode = match.arg(asset_mode)
  )
  validObject(scene)
  scene
}

.scene_float32 <- function(x) {
  writeBin(as.numeric(x), raw(), size = 4L, endian = "little")
}

.scene_uint32 <- function(x) {
  if (any(x < 0) || any(x > .Machine$integer.max)) {
    stop("Uint32 values exceed R's supported integer range", call. = FALSE)
  }
  writeBin(as.integer(x), raw(), size = 4L, endian = "little")
}

.scene_asset <- function(values, role, dtype, shape, inline, asset_dir) {
  bytes <- if (identical(dtype, "float32")) {
    .scene_float32(values)
  } else {
    .scene_uint32(values)
  }
  sha256 <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
  id <- paste0(role, "-sha256-", sha256)
  descriptor <- list(
    id = id,
    role = role,
    dtype = dtype,
    shape = I(as.integer(shape)),
    byteLength = length(bytes),
    sha256 = sha256,
    endianness = "little"
  )
  if (inline) {
    descriptor$encoding <- "base64"
    descriptor$data <- base64enc::base64encode(bytes)
  } else {
    filename <- paste0("sha256-", sha256, ".", role, ".bin")
    descriptor$uri <- filename
    path <- file.path(asset_dir, filename)
    if (!file.exists(path)) writeBin(bytes, path)
  }
  list(descriptor = descriptor, bytes = bytes)
}

#' Serialize a SurfaceScene as a surfview.scene.v1 manifest
#'
#' @param scene A \code{SurfaceScene}.
#' @param asset_mode \code{"inline"} or \code{"directory"}.
#' @param asset_dir Directory for adjacent binary assets. Required for directory
#'   mode.
#'
#' @return A JSON-compatible named list. Directory mode also writes canonical
#'   content-addressed assets to \code{asset_dir}.
#' @export
surface_scene_manifest <- function(scene, asset_mode = scene@asset_mode,
                                   asset_dir = NULL) {
  if (!is(scene, "SurfaceScene")) {
    stop("'scene' must be a SurfaceScene", call. = FALSE)
  }
  asset_mode <- match.arg(asset_mode, c("inline", "directory"))
  inline <- identical(asset_mode, "inline")
  if (!inline) {
    if (is.null(asset_dir) || length(asset_dir) != 1L || !nzchar(asset_dir)) {
      stop("'asset_dir' is required for directory mode", call. = FALSE)
    }
    dir.create(asset_dir, recursive = TRUE, showWarnings = FALSE)
  }

  assets <- list()
  add_asset <- function(values, role, dtype, shape) {
    built <- .scene_asset(values, role, dtype, shape, inline, asset_dir)
    assets[[built$descriptor$id]] <<- built$descriptor
    built$descriptor$id
  }

  geometry_manifests <- list()
  for (geometry_id in names(scene@geometries)) {
    geometry <- scene@geometries[[geometry_id]]
    vertex_matrix <- coords(geometry)
    face_matrix <- faces(geometry) - 1L
    vertices_id <- add_asset(
      as.vector(t(vertex_matrix)), "vertices", "float32",
      c(nrow(vertex_matrix), 3L)
    )
    faces_id <- add_asset(
      as.vector(t(face_matrix)), "faces", "uint32",
      c(nrow(face_matrix), 3L)
    )
    curvature_id <- NULL
    if (!is.null(scene@curvature[[geometry_id]])) {
      curvature_id <- add_asset(
        scene@curvature[[geometry_id]], "curvature", "float32",
        nrow(vertex_matrix)
      )
    }
    geometry_manifests[[geometry_id]] <- c(
      list(
        id = geometry_id,
        hemisphere = geometry_id,
        vertices = vertices_id,
        faces = faces_id
      ),
      if (!is.null(curvature_id)) list(curvature = curvature_id),
      list(
        vertexCount = nrow(vertex_matrix),
        faceCount = nrow(face_matrix),
        metadata = list(label = geometry@label)
      )
    )
  }

  layer_manifests <- list()
  for (layer_name in names(scene@layers)) {
    layer <- scene@layers[[layer_name]]
    value_refs <- list()
    for (geometry_id in names(scene@geometries)) {
      value_id <- add_asset(
        layer$values[[geometry_id]], "values", "float32",
        length(layer$values[[geometry_id]])
      )
      index_id <- NULL
      if (!is.null(layer$indices[[geometry_id]])) {
        index_id <- add_asset(
          layer$indices[[geometry_id]] - 1L, "indices", "uint32",
          length(layer$indices[[geometry_id]])
        )
      }
      value_refs[[geometry_id]] <- c(
        list(values = value_id),
        if (!is.null(index_id)) list(indices = index_id)
      )
    }
    layer_manifests[[layer_name]] <- c(
      list(
        id = layer_name,
        label = layer$legend$title %||% layer_name,
        values = value_refs,
        colorMap = layer$colormap,
        limits = layer$limits
      ),
      if (!is.null(layer$threshold)) list(threshold = layer$threshold),
      list(
        opacity = layer$opacity,
        visible = layer$visible,
        units = layer$units,
        legend = layer$legend
      ),
      if (length(layer$metadata)) list(metadata = layer$metadata),
      if (length(layer$provenance)) list(provenance = layer$provenance)
    )
  }

  structure(
    c(list(
      schemaVersion = "surfview.scene.v1",
      id = scene@id,
      assets = assets,
      geometries = geometry_manifests,
      layers = layer_manifests,
      selectedLayer = scene@selected_layer
    ),
    if (length(scene@metadata)) list(metadata = scene@metadata),
    if (length(scene@provenance)) list(provenance = scene@provenance)),
    class = c("surfview_scene_manifest", "list")
  )
}

.scene_embed_path <- function() {
  installed <- system.file(
    "htmlwidgets/lib/neurosurface/surfview.embed.iife.js",
    package = "neurosurf"
  )
  if (nzchar(installed)) return(installed)
  development <- file.path(
    "inst", "htmlwidgets", "lib", "neurosurface",
    "surfview.embed.iife.js"
  )
  if (file.exists(development)) return(normalizePath(development))
  stop("surfview embed artifact is missing; run 'make sync-surfviewjs'", call. = FALSE)
}

#' Write a standalone portable surface report
#'
#' @param scene A \code{SurfaceScene}.
#' @param path Output directory.
#' @param self_contained If \code{TRUE}, inline both assets and the browser runtime.
#'   Otherwise, write the runtime and SHA-addressed assets beside
#'   \code{index.html}.
#' @param title HTML document title.
#'
#' @return The path to \code{index.html}, invisibly.
#'
#' @details
#' With \code{self_contained = FALSE}, the function writes \code{index.html}, the local
#' surfview runtime, and content-addressed binary assets. With
#' \code{self_contained = TRUE}, it inlines the runtime and assets into one HTML
#' file. Neither mode requires a runtime network connection.
#'
#' @seealso \code{\link{surface_scene}}, \code{\link{surfwidget}}
#' @export
write_surface_scene <- function(scene, path, self_contained = FALSE,
                                title = scene@id) {
  if (!is.logical(self_contained) || length(self_contained) != 1L ||
      is.na(self_contained)) {
    stop("'self_contained' must be TRUE or FALSE", call. = FALSE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  manifest <- surface_scene_manifest(
    scene,
    asset_mode = if (self_contained) "inline" else "directory",
    asset_dir = if (self_contained) NULL else path
  )
  manifest_json <- jsonlite::toJSON(
    unclass(manifest), auto_unbox = TRUE, null = "null", na = "null",
    digits = NA
  )
  manifest_json <- gsub("</", "<\\/", manifest_json, fixed = TRUE)
  embed_path <- .scene_embed_path()
  if (self_contained) {
    script_tag <- paste0("<script>", paste(readLines(embed_path, warn = FALSE), collapse = "\n"), "</script>")
  } else {
    file.copy(embed_path, file.path(path, "surfview.embed.iife.js"), overwrite = TRUE)
    script_tag <- '<script src="./surfview.embed.iife.js"></script>'
  }
  title_text <- htmltools::htmlEscape(.scene_nonempty_string(title, "title"))
  fallback_text <- htmltools::htmlEscape(scene@fallback)
  alt_text <- jsonlite::toJSON(scene@alt_text, auto_unbox = TRUE)
  options_json <- jsonlite::toJSON(
    list(
      lazy = TRUE,
      preset = scene@preset,
      controls = identical(scene@mode, "report")
    ),
    auto_unbox = TRUE
  )
  html <- paste0(
    '<!doctype html><html lang="en"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width,initial-scale=1">',
    "<title>", title_text, "</title>",
    '<style>html,body{margin:0}#surfview-report{min-height:480px}',
    '.surfview-author-fallback{display:none;padding:1rem;font:14px system-ui}',
    '@media print{.surfview-mount{display:none!important}',
    '.surfview-author-fallback{display:block!important}}</style></head><body>',
    '<noscript><div class="surfview-author-fallback" style="display:block">',
    fallback_text, '</div></noscript>',
    '<div id="surfview-report" role="img" aria-label=', alt_text, '></div>',
    '<div id="surfview-fallback" class="surfview-author-fallback">',
    fallback_text, '</div>', script_tag,
    '<script type="application/json" id="surfview-manifest">',
    manifest_json, '</script><script>',
    'const manifest=JSON.parse(document.getElementById("surfview-manifest").textContent);',
    'const host=document.getElementById("surfview-report");',
    'const fallback=document.getElementById("surfview-fallback");',
    'const options=', options_json, ';options.baseUrl=document.baseURI;',
    'options.onError=()=>{host.hidden=true;fallback.style.display="block"};',
    'const handle=surfview.mountSurfView(host,manifest,options);',
    'handle.ready.then(()=>{window.surfviewHandle=handle;',
    'const bar=host.querySelector(".surfview-report-controls");',
    'if(bar){const b=document.createElement("button");b.type="button";',
    'b.textContent="Fullscreen";b.setAttribute("aria-label","Show surface viewer fullscreen");',
    'if(!host.requestFullscreen){b.disabled=true;b.title="Fullscreen is unavailable"}',
    'else b.addEventListener("click",()=>host.requestFullscreen().catch(()=>{b.title="Fullscreen could not be opened"}));',
    'bar.appendChild(b)}}).catch(options.onError);',
    '</script></body></html>'
  )
  output <- file.path(path, "index.html")
  writeLines(html, output, useBytes = TRUE)
  invisible(output)
}

#' @export
#' @rdname SurfaceScene-class
#' @param object A \code{SurfaceScene} to summarize.
#' @export
setMethod("show", "SurfaceScene", function(object) {
  cat("SurfaceScene '", object@id, "'\n", sep = "")
  cat("  hemispheres: ", paste(names(object@geometries), collapse = ", "), "\n", sep = "")
  cat("  layers: ", paste(names(object@layers), collapse = ", "), "\n", sep = "")
  cat("  selected: ", object@selected_layer, "\n", sep = "")
})
