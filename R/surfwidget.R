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
#' @param config A list of configuration options for the surface rendering:
#'   \itemize{
#'     \item{\code{shininess}}{Numeric. Controls the shininess of the material. Higher values create a more polished appearance. Default is 30.}
#'     \item{\code{specularColor}}{Character. Hex color code for the specular highlights. Default is "#111111".}
#'     \item{\code{flatShading}}{Logical. If TRUE, uses flat shading; if FALSE, uses smooth shading. Default is FALSE.}
#'     \item{\code{ambientLightColor}}{Character. Hex color code for the ambient light. Default is "#404040".}
#'     \item{\code{directionalLightColor}}{Character. Hex color code for the directional light. Default is "#ffffff".}
#'     \item{\code{directionalLightIntensity}}{Numeric. Intensity of the directional light. Default is 0.5.}
#'   }
#'
#' @import htmlwidgets
#' @importFrom grDevices col2rgb rgb
#'
#' @return An HTMLWidget object
#' @export
setMethod("surfwidget", signature(x = "SurfaceGeometry"),
  function(x, width = NULL, height = NULL, data = NULL, cmap = rainbow(256),
           irange = NULL, thresh = c(0,0), vertexColors = NULL, alpha = 1,
           config = list(), ...) {

    # Extract curvature values if data is not provided
    if (is.null(data)) {
      data <- curvature(x)
    }

    # Create a NeuroSurface and call its method
    neuro_surface <- NeuroSurface(x, indices=nodes(x), data)
    surfwidget(neuro_surface, width, height, cmap = cmap, irange = irange,
               thresh = thresh, vertexColors = vertexColors, alpha = alpha,
               config = config, ...)
  }
)

#' @rdname surfwidget
#' @export
setMethod("surfwidget", signature(x = "NeuroSurface"),
  function(x, width = NULL, height = NULL, cmap = rainbow(256),
           irange = range(x@data), thresh = c(0,0), vertexColors = NULL,
           alpha = 1, config = list(), ...) {

    if (is.null(irange)) {
      irange <- range(x@data)
    }
    print(irange)
    # Create a ColorMappedNeuroSurface and call its method
    color_mapped_surface <- ColorMappedNeuroSurface(x@geometry, x@indices, x@data, cmap = cmap,
                                irange = irange, thresh=thresh)
    surfwidget(color_mapped_surface, width, height, thresh = thresh,
               vertexColors = vertexColors, alpha = alpha, config = config, ...)
  }
)

#' @rdname surfwidget
#' @export
setMethod("surfwidget", signature(x = "ColorMappedNeuroSurface"),
  function(x, width = NULL, height = NULL, thresh = c(0,0), vertexColors = NULL,
           alpha = 1, config = list(), ...) {

    surface_data <- prepare_surface_data(x, thresh, vertexColors, alpha, config)
    create_widget(surface_data, width, height)
  }
)

#' @rdname surfwidget
#' @export
setMethod("surfwidget", signature(x = "VertexColoredNeuroSurface"),
  function(x, width = NULL, height = NULL, alpha = 1, config = list(), ...) {

    surface_data <- prepare_surface_data(x, c(0,0), x@colors, alpha, config)
    create_widget(surface_data, width, height)
  }
)

# Helper function to prepare surface data
#' @noRd
prepare_surface_data <- function(x, thresh, vertexColors, alpha, config) {
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
  color_config_keys <- c("color", "ambientLightColor", "directionalLightColor", "specularColor")
  for (key in color_config_keys) {
    if (key %in% names(config) && is.character(config[[key]])) {
      config[[key]] <- color_to_hex(config[[key]])
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
#'
#' @export
updateSurfwidgetConfig <- function(session, id, config) {
  message <- list(config = config)
  session$sendCustomMessage(type = 'surfwidget-config', message)
}

#' @export
updateColorMap <- function(widget, colorMap) {
  htmlwidgets::invokeMethod(widget, "setColorMap", colorMap)
}

#' @export
updateIRange <- function(widget, min, max) {
  htmlwidgets::invokeMethod(widget, "setIRange", min, max)
}

#' @export
updateThreshold <- function(widget, min, max) {
  htmlwidgets::invokeMethod(widget, "setThreshold", min, max)
}

#' @export
updateVertexColors <- function(widget, colors) {
  htmlwidgets::invokeMethod(widget, "setVertexColors", colors)
}

#' @export
updateAlpha <- function(widget, alpha) {
  htmlwidgets::invokeMethod(widget, "setAlpha", alpha)
}
