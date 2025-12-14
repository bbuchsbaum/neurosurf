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
#' @param colorbar Logical; if \code{TRUE} (default), render a colorbar when a
#'   colormap is used.
#' @param colorbar_label Optional character label shown alongside the colorbar.
#' @param layers Optional list of additional data layers to display on the surface.
#'   Each layer should be a list with elements such as \code{data}, \code{cmap},
#'   \code{alpha}, and optionally outline-specific parameters.
#' @param config A list of configuration options for the surface rendering:
#'   \describe{
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
#' @importFrom grDevices col2rgb rgb rainbow
#'
#' @return An HTMLWidget object
#' @rdname surfwidget-methods
#' @export
setMethod("surfwidget", signature(x = "SurfaceGeometry"),
  function(x, width = NULL, height = NULL, data = NULL, cmap = grDevices::rainbow(256),
           irange = NULL, thresh = c(0,0), vertexColors = NULL, alpha = 1,
           curvature = NULL, colorbar = TRUE, colorbar_label = NULL,
           layers = NULL, config = list(), ...) {

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
               curvature = curv_vals, colorbar = colorbar,
               colorbar_label = colorbar_label, layers = layers,
               config = config, ...)
  }
)

#' @rdname surfwidget-methods
#' @export
setMethod("surfwidget", signature(x = "NeuroSurface"),
  function(x, width = NULL, height = NULL, cmap = grDevices::rainbow(256),
           irange = range(x@data), thresh = c(0,0), vertexColors = NULL,
           alpha = 1, curvature = NULL, colorbar = TRUE,
           colorbar_label = NULL, layers = NULL,
           config = list(), ...) {

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
               colorbar = colorbar, colorbar_label = colorbar_label,
               layers = layers,
               config = config, ...)
  }
)

#' @rdname surfwidget-methods
#' @export
setMethod("surfwidget", signature(x = "ColorMappedNeuroSurface"),
  function(x, width = NULL, height = NULL, thresh = c(0,0), vertexColors = NULL,
           alpha = 1, curvature = NULL, colorbar = TRUE,
           colorbar_label = NULL, layers = NULL,
           config = list(), ...) {

    curv_vals <- curvature
    if (is.null(curv_vals)) {
      curv_vals <- curvature(x@geometry)
    }

    surface_data <- prepare_surface_data(x, thresh, vertexColors, alpha, config,
                                         curv_vals,
                                         colorbar = colorbar,
                                         colorbar_label = colorbar_label,
                                         layers = layers)
    create_widget(surface_data, width, height)
  }
)

#' @rdname surfwidget-methods
#' @export
setMethod("surfwidget", signature(x = "VertexColoredNeuroSurface"),
  function(x, width = NULL, height = NULL, alpha = 1, curvature = NULL,
           colorbar = TRUE, colorbar_label = NULL,
           layers = NULL,
           config = list(), ...) {

    curv_vals <- curvature
    if (is.null(curv_vals)) {
      curv_vals <- curvature(x@geometry)
    }

    surface_data <- prepare_surface_data(x, c(0,0), x@colors, alpha, config,
                                         curv_vals,
                                         colorbar = colorbar,
                                         colorbar_label = colorbar_label,
                                         layers = layers)
    create_widget(surface_data, width, height)
  }
)

# Helper function to prepare surface data
#' @noRd
prepare_surface_data <- function(x, thresh, vertexColors, alpha, config,
                                 curvature = NULL,
                                 colorbar = TRUE,
                                 colorbar_label = NULL,
                                 layers = NULL) {
  geom <- resolve_surface_geometry(x@geometry)

  surface_data <- list(
    vertices = as.vector(geom@mesh$vb[1:3,]),
    faces = as.vector(geom@mesh$it - 1),
    hemi = geom@hemi,
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
    curv_vals <- curvature(geom)
  }
  surface_data$curv <- curv_vals

  if (!is.null(vertexColors)) {
    # JS expects a numeric vector; if we got colors, convert or drop safely
    if (is.numeric(vertexColors)) {
      surface_data$vertexColors <- vertexColors
    } else if (is.character(vertexColors)) {
      vc_hex <- sapply(vertexColors, color_to_hex)
      # pack as integer RGB (0-255) to satisfy JS numeric check
      rgb_mat <- grDevices::col2rgb(vc_hex)
      surface_data$vertexColors <- as.numeric(rgb_mat[1, ] * 65536 +
                                              rgb_mat[2, ] * 256 +
                                              rgb_mat[3, ])
    } else {
      warning("vertexColors must be numeric or character; ignoring provided vertexColors")
    }
  }

  surface_data$config <- process_config(config)

  # Colorbar metadata for the JS widget
  surface_data$colorbar <- list(
    show = isTRUE(colorbar),
    label = colorbar_label
  )

  if (!is.null(layers)) {
    # Keep structure lightweight for JS
    surface_data$layers <- lapply(layers, function(layer) {
      # allow both snake_case and camelCase for incoming lists
      lc <- function(name) if (!is.null(layer[[name]])) layer[[name]] else NULL
      list(
        id = layer$label %||% NULL,
        data = layer$data,
        cmap = layer$cmap,
        color_range = layer$color_range,
        thresh = layer$thresh %||% layer$threshold,
        alpha = layer$alpha %||% 1,
        visible = isTRUE(layer$visible) || is.null(layer$visible),
        as_outline = isTRUE(layer$as_outline),
        blendMode = layer$blendMode,
        # Outline-specific
        outline_col = lc("outline_col"),
        outline_width = lc("outline_width"),
        outline_halo = lc("outline_halo"),
        outline_halo_col = lc("outline_halo_col"),
        outline_halo_width = lc("outline_halo_width"),
        outline_offset = lc("outline_offset"),
        outline_rois = lc("outline_rois"),
        indices = layer$indices
      )
    })
  }

  surface_data
}

# Helper function to create the widget
#' @noRd
#' @keywords internal
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
#' @keywords internal
process_config <- function(config) {
  if (length(config) == 0) return(config)

  known_keys <- c(
    "shininess", "specularColor", "flatShading",
    "ambientLightColor", "directionalLightColor",
    "directionalLightIntensity", "color",
    "materialType", "showControls", "controlType",
    "rotationSpeed", "initialZoom", "ambientLightIntensity"
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

  if ("ambientLightIntensity" %in% names(config)) {
    val <- config$ambientLightIntensity
    if (!is.numeric(val) || length(val) != 1 || val < 0 || val > 1) {
      warning("Invalid 'ambientLightIntensity' value; expected numeric in [0,1].")
      config$ambientLightIntensity <- NULL
    }
  }

  if ("rotationSpeed" %in% names(config)) {
    val <- config$rotationSpeed
    if (!is.numeric(val) || length(val) != 1 || val < 0) {
      warning("Invalid 'rotationSpeed' value; expected non-negative numeric scalar.")
      config$rotationSpeed <- NULL
    }
  }

  if ("initialZoom" %in% names(config)) {
    val <- config$initialZoom
    if (!is.numeric(val) || length(val) != 1 || val <= 0) {
      warning("Invalid 'initialZoom' value; expected positive numeric scalar.")
      config$initialZoom <- NULL
    }
  }

  if ("showControls" %in% names(config)) {
    val <- config$showControls
    if (!is.logical(val) || length(val) != 1) {
      warning("Invalid 'showControls' value; expected logical scalar.")
      config$showControls <- NULL
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
#' @keywords internal
color_to_hex <- function(color) {
  rgb_values <- col2rgb(color)
  sprintf("#%02X%02X%02X", rgb_values[1], rgb_values[2], rgb_values[3])
}


#' Show an interactive surface widget
#'
#' This is a convenience wrapper around \code{\link{surfwidget}} for quick,
#' interactive inspection of a single surface. It returns an HTML widget that
#' can be used in R Markdown documents or Shiny applications.
#'
#' @param x A \code{SurfaceGeometry}, \code{NeuroSurface}, or related object
#'   supported by \code{\link{surfwidget}}.
#' @param data Optional numeric vector of data values for each vertex when
#'   \code{x} is a \code{SurfaceGeometry}. Ignored if \code{x} already carries
#'   data (e.g., a \code{NeuroSurface}).
#' @param ... Additional arguments passed on to \code{\link{surfwidget}}.
#'
#' @return An \code{htmlwidget} object.
#'
#' @examples
#' \donttest{
#' fs <- load_fsaverage_std8("inflated")
#' show_surface_widget(fs$lh)
#' }
#'
#' @export
show_surface_widget <- function(x, data = NULL, ...) {
  if (inherits(x, "SurfaceGeometry")) {
    surfwidget(x, data = data, ...)
  } else {
    surfwidget(x, ...)
  }
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
#' @rdname surfwidget-shiny
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

invoke_widget_method <- function(widget, method, ...) {
  widget$x$calls <- c(widget$x$calls, list(list(method = method, args = list(...))))
  widget
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
#' @rdname surfwidget-shiny
#' @keywords internal
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
  invoke_widget_method(widget, "setColorMap", colorMap)
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
#' @rdname surfwidget-shiny
#' @keywords internal
updateIRange <- function(widget, min, max) {
  invoke_widget_method(widget, "setIRange", min, max)
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
#' @rdname surfwidget-shiny
#' @keywords internal
updateThreshold <- function(widget, min, max) {
  invoke_widget_method(widget, "setThreshold", min, max)
}


#' Update Vertex Colors
#'
#' Replace the per-vertex colors of an existing surfwidget.
#'
#' @param widget A surfwidget object as returned by \code{\link{surfwidget}}.
#' @param colors A vector of colors to apply to each vertex.
#'
#' @export

#' @rdname surfwidget-shiny
#' @keywords internal
updateVertexColors <- function(widget, colors) {
  invoke_widget_method(widget, "setVertexColors", colors)
}


#' Update Surface Opacity
#'
#' Adjust the overall opacity of an existing surfwidget.
#'
#' @param widget A surfwidget object as returned by \code{\link{surfwidget}}.
#' @param alpha Numeric opacity value between 0 (transparent) and 1 (opaque).
#'
#' @export
#' @rdname surfwidget-shiny
updateAlpha <- function(widget, alpha) {
  invoke_widget_method(widget, "setAlpha", alpha)
}

#' Update Zoom Level
#'
#' Adjust the zoom level of a surfwidget widget.
#'
#' @param widget A surfwidget htmlwidget object.
#' @param zoom   Numeric zoom factor.
#' @export
#' @rdname surfwidget-shiny
updateZoom <- function(widget, zoom) {
  invoke_widget_method(widget, "setZoom", zoom)
}

#' Update Rotation Speed
#'
#' Change the automatic rotation speed of a surfwidget widget.
#'
#' @param widget A surfwidget htmlwidget object.
#' @param speed  Numeric rotation speed.
#' @export
#' @rdname surfwidget-shiny
updateRotationSpeed <- function(widget, speed) {
  invoke_widget_method(widget, "setRotationSpeed", speed)
}

#' Debugging Helper for surfwidget
#'
#' This function helps diagnose issues with surfwidget rendering by checking
#' common problems and providing debugging information.
#'
#' @param x The surface object you're trying to visualize
#' @param verbose Logical, whether to print detailed information
#'
#' @return Invisibly returns a list of diagnostic information
#'
#' @examples
#' \donttest{
#' # Load a surface and check it
#' surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package="neurosurf")
#' surf <- read_surf(surf_file)
#' debug_surfwidget(surf)
#' }
#'
#' @export
debug_surfwidget <- function(x, verbose = TRUE) {
  diagnostics <- list()
  
  if (verbose) {
    cat("=== surfwidget Diagnostic Report ===\n\n")
  }
  
  # Check object class
  obj_class <- class(x)[1]
  diagnostics$class <- obj_class
  if (verbose) {
    cat("Object class:", obj_class, "\n")
  }
  
  # Check basic structure
  if (inherits(x, "SurfaceGeometry")) {
    n_vertices <- nrow(coords(x))
    n_faces <- ncol(x@mesh$it)
    hemi <- x@hemi
    
    diagnostics$n_vertices <- n_vertices
    diagnostics$n_faces <- n_faces
    diagnostics$hemisphere <- hemi
    
    if (verbose) {
      cat("Vertices:", n_vertices, "\n")
      cat("Faces:", n_faces, "\n") 
      cat("Hemisphere:", hemi, "\n")
    }
    
    # Check for common issues
    issues <- character(0)
    
    if (n_vertices == 0) {
      issues <- c(issues, "No vertices found")
    }
    
    if (n_faces == 0) {
      issues <- c(issues, "No faces found")
    }
    
    if (is.null(x@mesh)) {
      issues <- c(issues, "Mesh is NULL")
    }
    
    if (length(issues) > 0) {
      diagnostics$issues <- issues
      if (verbose) {
        cat("\nWARNING: Issues found:\n")
        for (issue in issues) {
          cat("  -", issue, "\n")
        }
      }
    } else {
      diagnostics$issues <- "None"
      if (verbose) {
        cat("\nOK: Basic structure looks good\n")
      }
    }
    
  } else if (inherits(x, "NeuroSurface")) {
    geom_diag <- debug_surfwidget(x@geometry, verbose = FALSE)
    diagnostics <- c(diagnostics, geom_diag)
    
    n_data <- length(x@data)
    n_indices <- length(x@indices)
    
    diagnostics$n_data_points <- n_data
    diagnostics$n_indices <- n_indices
    
    if (verbose) {
      cat("Geometry info:\n")
      cat("  Vertices:", geom_diag$n_vertices, "\n")
      cat("  Faces:", geom_diag$n_faces, "\n")
      cat("  Hemisphere:", geom_diag$hemisphere, "\n")
      cat("Data info:\n")
      cat("  Data points:", n_data, "\n")
      cat("  Indices:", n_indices, "\n")
    }
    
    # Check data consistency
    if (n_data != n_indices) {
      diagnostics$data_issues <- "Data length doesn't match indices length"
      if (verbose) {
        cat("\nWARNING: Data/indices length mismatch\n")
      }
    }
  }
  
  # JavaScript debugging instructions
  if (verbose) {
    cat("\n=== JavaScript Debugging ===\n")
    cat("1. Open browser developer tools (F12)\n")
    cat("2. Go to Console tab\n")
    cat("3. Run: neurosurface.setDebug(true)\n")
    cat("4. Reload the page and check console for debug messages\n")
    cat("5. Look for error messages in red\n\n")
    
    cat("=== Common Solutions ===\n")
    cat("- If widget is blank: Check JavaScript console for errors\n")
    cat("- If data doesn't show: Verify data ranges and color mapping\n")
    cat("- If widget doesn't load: Check if WebGL is supported\n")
    cat("- Try with a simpler surface first\n")
  }
  
  invisible(diagnostics)
}
