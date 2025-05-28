#' surfwidget
#'
#' Create a surfwidget to display brain surface data.
#'
#' @param x A SurfaceGeometry, NeuroSurface, ColorMappedNeuroSurface, or VertexColoredNeuroSurface object
#' @param width The width of the widget
#' @param height The height of the widget
#' @param data Optional. Numeric vector of data values for each vertex.
#' @param cmap Optional. Color map for data visualization.
#' @param irange Optional. Intensity range for data visualization.
#' @param thresh Optional. Threshold range for data visualization.
#' @param vertexColors Optional. Vector of colors for each vertex.
#' @param alpha Opacity of the surface (0 to 1).
#' @param curvature Optional numeric vector of curvature values for each vertex.
#'   If not supplied for a \code{SurfaceGeometry} object, it is computed via
#'   \code{curvature(x)}.
#' @param config A list of configuration options for the surface rendering:
#'   \itemize{
#'     \item{\code{shininess}}{Numeric between 0 and 100. Controls the shininess of the material. Higher values create a more polished appearance. Default is 30.}
#'     \item{\code{specularColor}}{Character. Hex color code for the specular highlights. Default is "#111111".}
#'     \item{\code{flatShading}}{Logical scalar. If \code{TRUE}, uses flat shading; if \code{FALSE}, uses smooth shading. Default is \code{FALSE}.}
#'     \item{\code{ambientLightColor}}{Character. Hex color code for the ambient light. Default is "#404040".}
#'     \item{\code{directionalLightColor}}{Character. Hex color code for the directional light. Default is "#ffffff".}
#'     \item{\code{directionalLightIntensity}}{Numeric between 0 and 1. Intensity of the directional light. Default is 0.5.}
#'   }
#'   Unknown elements in \code{config} are ignored with a warning.
#'
#' @import htmlwidgets
#' @importFrom grDevices col2rgb rgb
#'
#' @return An HTMLWidget object
#' @export
setMethod("surfwidget", signature(x = "SurfaceGeometry"),
  function(x, width = NULL, height = NULL, data = NULL, cmap = rainbow(256),
           irange = NULL, thresh = c(0,0), vertexColors = NULL, alpha = 1,
           curvature = NULL, config = list(), ...) {

    curv_vals <- curvature
    if (is.null(curv_vals)) {
      curv_vals <- curvature(x)
    }

    # Extract data values if not provided
    if (is.null(data)) {
      data <- curvature(x)
    }

    # Create a NeuroSurface and call its method
    neuro_surface <- NeuroSurface(x, indices=nodes(x), data)
    surfwidget(neuro_surface, width, height, cmap = cmap, irange = irange,
               thresh = thresh, vertexColors = vertexColors, alpha = alpha,
               curvature = curv_vals, config = config, ...)
  }
)

#' @rdname surfwidget
#' @export
setMethod("surfwidget", signature(x = "NeuroSurface"),
  function(x, width = NULL, height = NULL, cmap = rainbow(256),
           irange = range(x@data), thresh = c(0,0), vertexColors = NULL,
           alpha = 1, curvature = NULL, config = list(), ...) {

    if (is.null(irange)) {
      irange <- range(x@data)
    }
    curv_vals <- curvature
    if (is.null(curv_vals)) {
      curv_vals <- curvature(x@geometry)
    }
    # Create a ColorMappedNeuroSurface and call its method
    color_mapped_surface <- ColorMappedNeuroSurface(x@geometry, x@indices, x@data, cmap = cmap,
                                irange = irange, thresh=thresh)
    surfwidget(color_mapped_surface, width, height, thresh = thresh,
               vertexColors = vertexColors, alpha = alpha, curvature = curv_vals,
               config = config, ...)
  }
)

#' @rdname surfwidget
#' @export
setMethod("surfwidget", signature(x = "ColorMappedNeuroSurface"),
  function(x, width = NULL, height = NULL, thresh = c(0,0), vertexColors = NULL,
           alpha = 1, curvature = NULL, config = list(), ...) {

    curv_vals <- curvature
    if (is.null(curv_vals)) {
      curv_vals <- curvature(x@geometry)
    }

    surface_data <- prepare_surface_data(x, thresh, vertexColors, alpha, config,
                                         curv_vals)
    create_widget(surface_data, width, height)
  }
)

#' @rdname surfwidget
#' @export
setMethod("surfwidget", signature(x = "VertexColoredNeuroSurface"),
  function(x, width = NULL, height = NULL, alpha = 1, curvature = NULL,
           config = list(), ...) {

    curv_vals <- curvature
    if (is.null(curv_vals)) {
      curv_vals <- curvature(x@geometry)
    }

    surface_data <- prepare_surface_data(x, c(0,0), x@colors, alpha, config,
                                         curv_vals)
    create_widget(surface_data, width, height)
  }
)

# Helper function to prepare surface data
#' @noRd
prepare_surface_data <- function(x, thresh, vertexColors, alpha, config, curvature = NULL) {
  surface_data <- list(
    vertices = as.vector(x@geometry@mesh$vb[1:3,]),
    faces = as.vector(x@geometry@mesh$it - 1),
    hemi = x@geometry@hemi,
    data = x@data,
    indices = x@indices - 1,
    thresh = thresh,
    alpha = alpha
  )

  if (inherits(x, "ColorMappedNeuroSurface")) {
    surface_data$cmap <- x@cmap
    surface_data$irange <- x@irange
  }

  curv_vals <- curvature
  if (is.null(curv_vals)) {
    curv_vals <- curvature(x@geometry)
  }
  surface_data$curv <- curv_vals

  if (!is.null(vertexColors)) {
    surface_data$vertexColors <- sapply(vertexColors, color_to_hex)
  }

  surface_data$config <- process_config(config)

  surface_data
}

# Helper function to create the widget
#' @noRd
create_widget <- function(surface_data, width, height) {
  htmlwidgets::createWidget(
    name = 'surfwidget',
    surface_data,
    width = width,
    height = height,
    package = 'neurosurf'
  )
}

# Helper function to process config options
#' @noRd
process_config <- function(config) {
  if (length(config) == 0) return(config)

  known_keys <- c(
    "shininess", "specularColor", "flatShading",
    "ambientLightColor", "directionalLightColor",
    "directionalLightIntensity", "color"
  )

  unknown <- setdiff(names(config), known_keys)
  if (length(unknown) > 0) {
    warning("Ignoring unknown config key(s): ", paste(unknown, collapse = ", "))
    config[unknown] <- NULL
  }

  # convert colors
  color_config_keys <- c("color", "ambientLightColor", "directionalLightColor", "specularColor")
  for (key in color_config_keys) {
    if (key %in% names(config) && is.character(config[[key]])) {
      config[[key]] <- color_to_hex(config[[key]])
    }
  }

  # validate numeric and logical options
  if ("shininess" %in% names(config)) {
    val <- config$shininess
    if (!is.numeric(val) || length(val) != 1 || val < 0 || val > 100) {
      warning("Invalid 'shininess' value; expected numeric in [0,100].")
      config$shininess <- NULL
    }
  }

  if ("directionalLightIntensity" %in% names(config)) {
    val <- config$directionalLightIntensity
    if (!is.numeric(val) || length(val) != 1 || val < 0 || val > 1) {
      warning("Invalid 'directionalLightIntensity' value; expected numeric in [0,1].")
      config$directionalLightIntensity <- NULL
    }
  }

  if ("flatShading" %in% names(config)) {
    val <- config$flatShading
    if (!is.logical(val) || length(val) != 1) {
      warning("Invalid 'flatShading' value; expected logical scalar.")
      config$flatShading <- NULL
    }
  }

  config
}

# Helper function to convert R color to hex
#' @noRd
color_to_hex <- function(color) {
  rgb_values <- col2rgb(color)
  sprintf("#%02X%02X%02X", rgb_values[1], rgb_values[2], rgb_values[3])
}


#' Shiny bindings for surfwidget
#'
#' Output and render functions for using surfwidget within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a surfwidget
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name surfwidget-shiny
#'
#' @export
surfwidgetOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'surfwidget', width, height, package = 'neurosurf')
}

#' @rdname surfwidget-shiny
#' @export
renderSurfwidget <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, surfwidgetOutput, env, quoted = TRUE)
}

#' Update Surfwidget Configuration
#'
#' Update the configuration of an existing surfwidget.
#'
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#' @param id The ID of the surfwidget output.
#' @param config A list of configuration options to update. See \code{\link{surfwidget}} for details on available options.
#' @details Sends a custom message of type \code{"neurosurf-surfwidget-config"} to update the widget configuration on the client.
#'
#' @export
updateSurfwidgetConfig <- function(session, id, config) {
  message <- list(id = id, config = config)
  session$sendCustomMessage(type = 'neurosurf-surfwidget-config', message)
}

#' Update Surface Color Map
#'
#' Change the color map used by an existing surfwidget.
#'
#' @param widget A surfwidget object as returned by \code{\link{surfwidget}}.
#' @param colorMap A vector of colors defining the new color map.
#'
#' @export
updateColorMap <- function(widget, colorMap) {
  htmlwidgets::invokeMethod(widget, "setColorMap", colorMap)
}

#' Update Data Intensity Range
#'
#' Modify the minimum and maximum values used for data mapping.
#'
#' @param widget A surfwidget object as returned by \code{\link{surfwidget}}.
#' @param min Numeric minimum value for the intensity range.
#' @param max Numeric maximum value for the intensity range.
#'
#' @export
updateIRange <- function(widget, min, max) {
  htmlwidgets::invokeMethod(widget, "setIRange", min, max)
}

#' Update Display Threshold
#'
#' Set the threshold limits for showing surface data.
#'
#' @param widget A surfwidget object as returned by \code{\link{surfwidget}}.
#' @param min Numeric lower bound of the threshold.
#' @param max Numeric upper bound of the threshold.
#'
#' @export
updateThreshold <- function(widget, min, max) {
  htmlwidgets::invokeMethod(widget, "setThreshold", min, max)
}

#' Update Vertex Colors
#'
#' Replace the per-vertex colors of an existing surfwidget.
#'
#' @param widget A surfwidget object as returned by \code{\link{surfwidget}}.
#' @param colors A vector of colors to apply to each vertex.
#'
#' @export
updateVertexColors <- function(widget, colors) {
  htmlwidgets::invokeMethod(widget, "setVertexColors", colors)
}

#' Update Surface Opacity
#'
#' Adjust the overall opacity of an existing surfwidget.
#'
#' @param widget A surfwidget object as returned by \code{\link{surfwidget}}.
#' @param alpha Numeric opacity value between 0 (transparent) and 1 (opaque).
#'
#' @export
updateAlpha <- function(widget, alpha) {
  htmlwidgets::invokeMethod(widget, "setAlpha", alpha)
}

#' Update Zoom Level
#'
#' Adjust the zoom level of a surfwidget widget.
#'
#' @param widget A surfwidget htmlwidget object.
#' @param zoom   Numeric zoom factor.
#' @export
updateZoom <- function(widget, zoom) {
  htmlwidgets::invokeMethod(widget, "setZoom", zoom)
}

#' Update Rotation Speed
#'
#' Change the automatic rotation speed of a surfwidget widget.
#'
#' @param widget A surfwidget htmlwidget object.
#' @param speed  Numeric rotation speed.
#' @export
updateRotationSpeed <- function(widget, speed) {
  htmlwidgets::invokeMethod(widget, "setRotationSpeed", speed)
}
