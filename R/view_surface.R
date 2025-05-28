


# plotlySurface <- function(surfgeom, vals, col=brewer_pal(palette="RdBu")(9),
#                           alpha=1,
#                           add_normals=FALSE,
#                           threshold=NULL,
#                           irange=NULL,
#                           bgcol="#D3D3D3") {
#
#
#
#   curv <- curvature(surfgeom)
#
#   cmat <- rbind(vals[surfgeom@mesh$it[1,]],vals[surfgeom@mesh$it[2,]],vals[surfgeom@mesh$it[3,]] )
#   fvals <- colMeans(cmat)
#
#   fg_layer <- IntensityColorPlane(fvals, col,alpha=1)
#   fg_clrs <- map_colors(fg_layer, alpha=alpha, threshold=threshold, irange=irange)
#
#   #bg_layer <- IntensityColorPlane(curv, c("#D3D3D3FF", "#A9A9A9FF", "#A9A9A9FF"),alpha=1)
#   #bg_clrs <- map_colors(bg_layer, alpha=1)
#
#   #browser()
#
#
#   #combined <- blend_colors(bg_clrs, fg_clrs, alpha=alpha)
#   #face_cols <- as_hexcol(combined)
#   face_cols <- as_hexcol(fg_clrs)
#
#   cds <- coords(surfgeom)
#   p <- plot_ly(
#     x = cds[,1], y=cds[,2], z = cds[,3],
#     i = surfgeom@mesh$it[1,]-1, j =surfgeom@mesh$it[2,]-1, k = surfgeom@mesh$it[3,]-1,
#     facecolor=face_cols,
#     #vertexcolor=face_cols,
#     #intensity=vals,
#     type = "mesh3d",
#     flatshading=TRUE
#   )
#   p
#
#
# }
#
#
#

#' Convert Curvature Values to Binary Colors for Visualization
#'
#' @description
#' This function maps a vector of surface curvature values (e.g., mean curvature)
#' to a binary color scheme, typically used to distinguish gyri (outward folds)
#' from sulci (inward folds) on a brain surface visualization.
#'
#' @param vals A numeric vector containing curvature values for each vertex
#'   on the surface.
#' @param incol A character string specifying the hex color code to represent
#'   vertices with curvature values *greater than* the median curvature.
#'   Default is "#B3B3B3" (light gray).
#' @param outcol A character string specifying the hex color code to represent
#'   vertices with curvature values *less than or equal to* the median curvature.
#'   Default is "#404040" (dark gray).
#'
#' @return A character vector of the same length as `vals`, containing hex color
#'   codes based on the binary classification of curvature values relative to the median.
#'
#' @details
#' Surface curvature provides information about the local shape of the surface.
#' Mean curvature is often used, where positive values typically indicate outward
#' curvature (gyri) and negative values indicate inward curvature (sulci).
#' This function simplifies the curvature map into two colors based on whether
#' the value is above or below the median curvature, providing a quick visual
#' distinction between these features. Note the default coloring assigns `incol`
#' to values *above* the median and `outcol` to values *at or below* the median.
#' You might need to adjust `incol` and `outcol` depending on the specific
#' interpretation of curvature values in your data (e.g., if positive values
#' represent sulci).
#'
#' @examples
#' # Generate some example curvature values
#' set.seed(123)
#' curvature_values <- rnorm(100, mean = 0, sd = 0.1)
#'
#' # Get binary colors using default light/dark gray
#' gray_colors <- curv_cols(curvature_values)
#' table(gray_colors)
#'
#' # Use different colors (e.g., red for above median, blue for below)
#' red_blue_colors <- curv_cols(curvature_values, incol = "#FF0000", outcol = "#0000FF")
#' table(red_blue_colors)
#'
#' @seealso \code{\link{curvature}}, \code{\link{view_surface}}
#'
#' @export
curv_cols <- function(vals, incol="#B3B3B3", outcol="#404040") {
  ifelse(vals > stats::median(vals), incol, outcol)
}


#' @noRd
#' @keywords internal
surface_views <- list(
  left_lateral=rbind(c(0,-1,0,0), c(0,0,1,0),c(-1,0,0,0), c(0,0,0,1)),
  left_medial=rbind(c(0,1,0,0), c(0,0,1,0),c(1,0,0,0), c(0,0,0,1)),
  left_ventral=rbind(c(-1,0,0,0), c(0,1,0,0),c(0,0,-1,0), c(0,0,0,1)),
  left_posterior=rbind(c(1,0,0,0), c(0,0,1,0),c(0,-1,0,0), c(0,0,0,1)),

  right_lateral=rbind(c(0,1,0,0), c(0,0,1,0), c(1,0,0,0), c(0,0,0,1)),
  right_medial=rbind(c(0,-1,0,0), c(0,0,1,0), c(-1,0,0,0), c(0,0,0,1)),
  right_ventral=rbind(c(-1,0,0,0), c(0,1,0,0),c(0,0,-1,0), c(0,0,0,1)),
  right_posterior=rbind(c(1,0,0,0), c(0,0,1,0),c(0,-1,0,0), c(0,0,0,1))
)


#' Display a 3D Brain Surface using RGL
#'
#' @description
#' Renders a 3D brain surface mesh using the `rgl` package. This function provides
#' flexible options for coloring the surface based on data values or predefined
#' colors, adjusting transparency, controlling lighting, setting viewpoints, and
#' overlaying spherical markers.
#'
#' @param surfgeom A \code{\linkS4class{SurfaceGeometry}} object representing the
#'   3D brain surface mesh to be displayed.
#' @param vals An optional numeric vector containing data values for each vertex
#'   on the surface. If provided and `vert_clrs` is NULL, these values are mapped
#'   to colors using `cmap` and `irange`.
#' @param cmap A vector of colors (e.g., hex codes) defining the color map used
#'   when `vals` is provided and `vert_clrs` is NULL. Defaults to `rainbow(256)`.
#' @param vert_clrs An optional character vector of hex color codes for each vertex.
#'   If provided, these colors directly override any coloring derived from `vals` and `cmap`.
#'   The length should match the number of vertices in `surfgeom`.
#' @param bgcol A single hex color code or a vector of hex color codes used as the
#'   base color for the surface. If `vals` or `vert_clrs` are provided, this color
#'   is blended with the data/vertex colors. Defaults to "lightgray".
#' @param alpha A numeric value between 0 (fully transparent) and 1 (fully opaque)
#'   controlling the overall transparency of the surface. Defaults to 1.
#' @param add_normals Logical. If TRUE (default), surface normals are calculated
#'   and added to the mesh, which improves the appearance of lighting effects.
#' @param thresh An optional numeric vector of length 2, `c(lower, upper)`.
#'   Vertices with `vals` *outside* this range (i.e., `< lower` or `> upper`)
#'   are made fully transparent. This is applied *after* the general `alpha`.
#'   Defaults to NULL (no thresholding).
#' @param irange An optional numeric vector of length 2, `c(min, max)`. Specifies
#'   the range of `vals` to map onto the `cmap`. Values outside this range will be
#'   clamped to the min/max colors. Defaults to the full range of `vals`.
#' @param specular The color of specular highlights on the surface, affecting its
#'   shininess. Can be a color name (e.g., "white") or hex code. Defaults to "white".
#'   Set to "black" or NULL for a matte appearance.
#' @param viewpoint A character string specifying a predefined view (e.g., "lateral",
#'   "medial", "ventral", "posterior"). The actual view depends on the hemisphere
#'   (`surfgeom@hemi`, e.g., "left_lateral"). Alternatively, a 4x4 transformation
#'   matrix defining a custom view. Defaults to "lateral".
#' @param new_window Logical. If TRUE (default), opens a new `rgl` window for the plot.
#'   If FALSE, attempts to plot in the currently active `rgl` window (useful for
#'   updates or within Shiny apps).
#' @param offset A numeric vector of length 3 specifying a translation offset
#'   `c(x, y, z)` applied to the surface coordinates before rendering. Defaults to `c(0, 0, 0)`.
#' @param zoom A numeric value controlling the camera zoom level. Defaults to 1 (no zoom).
#'   Values > 1 zoom in, < 1 zoom out.
#' @param spheres An optional data frame to draw spheres at specific locations on
#'   or near the surface. Must contain columns `x`, `y`, `z` (coordinates), and
#'   `radius`. Can optionally include a `color` column (hex codes or color names)
#'   for individual sphere colors (defaults to black).
#' @param ... Additional arguments passed directly to `rgl::shade3d` for fine-grained
#'   control over rendering (e.g., `lit`, `smooth`).
#'
#' @return Invisibly returns the object ID(s) of the shape(s) added to the RGL scene
#'   by `rgl::shade3d`. This can be useful for modifying the scene later.
#'
#' @details
#' **Coloring:** Surface vertex colors are determined by the following priority:
#'   1. `vert_clrs`: If provided, these specific hex colors are used.
#'   2. `vals` & `cmap`: If `vals` is provided and `vert_clrs` is NULL, `vals` are mapped to `cmap` based on `irange`.
#'   3. `bgcol`: If neither `vert_clrs` nor `vals` are used for coloring, `bgcol` is applied uniformly.
#'   If `bgcol` is specified alongside `vert_clrs` or `vals`, the colors are blended based on the `alpha` parameter.
#'
#' **Transparency:** Overall transparency is set by `alpha`. Additional threshold-based
#'   transparency can be applied using `thresh` when `vals` are provided. Vertices
#'   with values outside the `thresh` range become fully transparent.
#'
#' **Lighting:** `add_normals=TRUE` is recommended for realistic lighting. The `specular`
#'   parameter controls the shininess.
#'
#' **Viewpoint:** Predefined viewpoints (`"lateral"`, `"medial"`, etc.) are automatically
#'   adjusted based on the hemisphere specified in `surfgeom@hemi` (e.g., "lh" results
#'   in "left_lateral"). If `hemi` is unknown, the current `rgl` view is used unless
#'   a custom 4x4 matrix is provided.
#'
#' **Performance:** Rendering very large surfaces or surfaces with complex coloring/transparency
#'   can be computationally intensive.
#'
#' @importFrom gplots col2hex
#' @importFrom rgl open3d clear3d shade3d spheres3d view3d par3d addNormals rgl.useNULL
#' @importFrom colorplane IntensityColorPlane HexColorPlane map_colors blend_colors as_hexcol
#' @importFrom stats median
#' @importFrom grDevices rainbow
#'
#' @examples
#' \donttest{
#' # Assume 'surf_geom' is a SurfaceGeometry object loaded previously
#' # e.g., surf_geom <- read_surf_geometry("path/to/surface.gii")
#'
#' # Simple display with default background color
#' view_surface(surf_geom, viewpoint = "lateral")
#'
#' # Display with curvature coloring (assuming you have curvature data)
#' # curv_vals <- curvature(surf_geom) # Calculate curvature if needed
#' # view_surface(surf_geom, vals = curv_vals, cmap = gray.colors(256), viewpoint = "medial")
#'
#' # Display with specific vertex colors (e.g., based on labels)
#' # num_verts <- length(nodes(surf_geom))
#' # colors_for_verts <- sample(c("red", "blue", "green"), num_verts, replace = TRUE)
#' # view_surface(surf_geom, vert_clrs = colors_for_verts, viewpoint = "ventral")
#'
#' # Display with thresholding and custom color map
#' # random_data <- rnorm(length(nodes(surf_geom)))
#' # view_surface(surf_geom, vals = random_data,
#' #              cmap = colorRampPalette(c("blue", "white", "red"))(256),
#' #              thresh = c(-1.5, 1.5), # Make values between -1.5 and 1.5 transparent
#' #              irange = c(-3, 3),     # Map values from -3 to 3 onto the cmap
#' #              viewpoint = "posterior")
#'
#' # Display with spheres marking specific coordinates
#' sphere_coords <- data.frame(
#'   x = c(10, -15, 5),
#'   y = c(20, 0, -10),
#'   z = c(-5, 25, 15),
#'   radius = c(3, 4, 2.5),
#'   color = c("yellow", "cyan", "magenta")
#' )
#' view_surface(surf_geom, viewpoint = "lateral", spheres = sphere_coords)
#'
#' # Plot in the current rgl window without opening a new one
#' # rgl::open3d() # Open window first
#' # view_surface(surf_geom, new_window = FALSE)
#' }
#'
#' @seealso \code{\link[rgl]{shade3d}}, \code{\link[rgl]{spheres3d}}, \code{\link[rgl]{view3d}}, \code{\link{SurfaceGeometry}}
#' @export
view_surface <- function(surfgeom, vals=NA,
                         cmap=grDevices::rainbow(256, alpha = 1),
                         vert_clrs=NULL,
                         bgcol = "lightgray",
                         alpha=1,
                         add_normals=TRUE,
                         thresh=NULL,
                         irange=range(vals,na.rm=TRUE),
                         specular="white",  # Default to white for a shiny surface
                         viewpoint=c("lateral","medial", "ventral", "posterior"),
                         new_window=TRUE,  # New argument to control RGL window
                         offset=c(0,0,0),
                         zoom=1,
                         spheres=NULL,  # New argument for spheres
                         ...) {


  # Open a new rgl window only if not in Shiny
  if (new_window && !rgl::rgl.useNULL()) {
    rgl::open3d()
  } else {
    #rgl::clear3d(type = "all")
  }



  if (add_normals) {
    surfgeom@mesh <- rgl::addNormals(surfgeom@mesh)
  }

  viewpoint <- match.arg(viewpoint)

  umat <- if (is.matrix(viewpoint)) {
    stopifnot(nrow(viewpoint) == 4 && ncol(viewpoint) == 4)
    viewpoint
  } else if (surfgeom@hemi == "lh") {
    viewpoint <- paste0("left_", viewpoint)
    surface_views[[viewpoint]]
  } else if (surfgeom@hemi == "rh") {
    viewpoint <- paste0("right_", viewpoint)
    surface_views[[viewpoint]]
  } else {
    warning("unknown hemisphere, default using viewpoint")
    rgl::par3d()$userMatrix
  }

  if (is.character(bgcol)) {
    bgcol <- gplots::col2hex(bgcol)
  }

  if (length(bgcol) == 1) {
    bg_layer <- colorplane::HexColorPlane(rep(bgcol, length(surfgeom@mesh$vb[1,])))
  } else {
    bg_layer <- colorplane::HexColorPlane(bgcol)
  }

  if (any(!is.na(vals)) && !is.null(vals) && is.null(vert_clrs)) {
    fg_layer <- colorplane::IntensityColorPlane(vals, cmap, alpha=1)
    fg_clrs <- colorplane::map_colors(fg_layer, alpha=alpha, threshold=thresh, irange=irange)
    combined <- colorplane::blend_colors(bg_layer, fg_clrs, alpha=alpha)
    vertex_cols <- colorplane::as_hexcol(combined)
  } else if (!is.null(vert_clrs)) {
    fg_layer <- colorplane::HexColorPlane(vert_clrs)
    #fg_clrs <- fg_layer@clr
    combined <- colorplane::blend_colors(bg_layer, fg_layer, alpha=alpha)
    vertex_cols <- colorplane::as_hexcol(combined)
  } else {
    vertex_cols <- colorplane::as_hexcol(bg_layer)
  }

  rgl::par3d(mouseMode="trackball")
  ret <- rgl::shade3d(surfgeom@mesh, col=vertex_cols, specular=specular, meshColor="vertices", ...)
  rgl::view3d(fov=0, userMatrix=umat, zoom=zoom)

  # Add spheres if specified
  if (!is.null(spheres)) {
    # Ensure the spheres data frame has the required columns
    if (!all(c("x", "y", "z", "radius") %in% names(spheres))) {
      stop("spheres data frame must contain columns 'x', 'y', 'z', and 'radius'.")
    }
    for (i in seq_len(nrow(spheres))) {
      # Use provided color or default to black
      sphere_color <- if ("color" %in% names(spheres)) spheres$color[i] else "black"
      rgl::spheres3d(
        x = spheres$x[i],
        y = spheres$y[i],
        z = spheres$z[i],
        radius = spheres$radius[i],
        color = sphere_color
      )
    }
  }

  ret
}
#' plot a surface
#'
#' @rdname plot-methods
#' @param x the surface to plot
#' @param ... extra args to send to \code{view_surface}
#' @export
#' @importFrom graphics plot
#' @importFrom grDevices gray
#' @inheritParams view_surface
setMethod("plot", signature=signature(x="SurfaceGeometry", y="missing"),
          def=function(x,vals=NA, cmap=grDevices::gray(seq(0,1,length.out=255)),
                       vert_clrs=NULL,
                       irange=range(vals),
                       thresh=c(0,0),
                       alpha=1,
                       specular="black",
                       bgcol="lightgray", ...) {

            view_surface(x,vals,cmap=cmap,vert_clrs=vert_clrs, irange=irange,thresh=thresh,alpha=alpha,bgcol=bgcol,specular=specular,...)

          })


#' @export
#' @rdname plot-methods
setMethod("plot", signature=signature(x="NeuroSurface", y="missing"),
          def=function(x,cmap=grDevices::gray(seq(0,1,length.out=255)),
                       vert_clrs=NULL,
                       irange=range(x@data, na.rm=TRUE),
                       thresh=c(0,0),
                       alpha=1,
                       specular="black",
                       bgcol="lightgray", ...) {

            ind <- x@indices
            vals <- rep(NA, length(nodes(x)))
            vals[ind] <- x@data

            view_surface(x@geometry,vals,cmap=cmap,vert_clrs=vert_clrs, irange=irange,thresh=thresh,alpha=alpha,bgcol=bgcol,specular=specular,...)

          })

#' @export
#' @importFrom graphics plot
#' @rdname plot-methods
setMethod("plot", signature=signature(x="LabeledNeuroSurface", y="missing"),
          def=function(x,cmap=x@cols,
                       vert_clrs=NULL,
                       irange=range(x@data, na.rm=TRUE),
                       thresh=c(0,0),
                       alpha=1,
                       specular="black",
                       bgcol="lightgray", ...) {

            ind <- x@indices
            vals <- rep(NA, length(nodes(x)))
            vals[ind] <- x@data

            view_surface(x@geometry,vals,cmap=cmap,vert_clrs=vert_clrs, irange=irange,thresh=thresh,alpha=alpha,bgcol=bgcol,specular=specular,...)

          })


#' @export
#' @rdname plot-methods
setMethod("plot", signature=signature(x="ColorMappedNeuroSurface", y="missing"),
          def=function(x,
                       vert_clrs=NULL,
                       alpha=1,
                       specular="black",
                       bgcol="lightgray", ...) {

            ind <- x@indices
            vals <- rep(NA, length(nodes(x)))
            vals[ind] <- x@data

            view_surface(x@geometry,
                         vals,
                         cmap=x@cmap,
                         vert_clrs=vert_clrs,
                         irange=x@irange,
                         thresh=x@thresh,
                         alpha=alpha,
                         bgcol=bgcol,
                         specular=specular,
                         ...)

          })


#' @export
#' @rdname plot-methods
setMethod("plot", signature=signature(x="VertexColoredNeuroSurface", y="missing"),
          def=function(x,
                       alpha=1,
                       specular="black",
                       bgcol="lightgray", ...) {

            ind <- x@indices
            colors <- rep(NA, length(nodes(x@geometry)))
            colors[ind] <- x@colors

            view_surface(x@geometry,
                         vals=NULL,
                         cmap=NULL,
                         vert_clrs=colors,
                         irange=NULL,
                         thresh=NULL,
                         alpha=alpha,
                         bgcol=bgcol,
                         specular=specular,
                         ...)

          })



#' viewShiny <- function(surfgeom, vals=1:length(nodes(surfgeom)), col=rainbow(255, alpha = 1)) {
#'   options(rgl.useNULL = TRUE)
#'
#'   app <- shinyApp(
#'     ui = fluidPage(
#'       rgl::registerSceneChange(),
#'       sidebarLayout(
#'         sidebarPanel(
#'           sliderInput(inputId="threshold",
#'                       "Intensity Threshold:",
#'                       min = min(vals),
#'                       max = max(vals),
#'                       value = c(.45*max(vals), .55*max(vals))),
#'           sliderInput(inputId="range",
#'                       "Intensity Range:",
#'                        min = min(vals),
#'                        max = max(vals),
#'                        value = c(.02*min(vals), .98*max(vals)))
#'         ),
#'         mainPanel(h3("Surface View"),
#'                   rgl::rglwidgetOutput("surface_widget", width = "100%", height = 512))
#'       )
#'     ),
#'
#'     server = function(input, output, session) {
#'       options(rgl.useNULL=TRUE)
#'       rgl::open3d()
#'       dev <- rgl::rgl.cur()
#'       save <- options(rgl.inShiny = TRUE)
#'       on.exit(options(save))
#'
#'       session$onSessionEnded(function() {
#'         rgl::rgl.set(dev)
#'         rgl::rgl.close()
#'       })
#'
#'       #surf <- viewSurface(surfgeom, vals, col, add_normals=TRUE)
#'
#'       #start_surf <- reactiveValues(my_mesh=NULL)
#'
#'       #viewSurface(surfgeom, vals,  col)
#'       #scene1 <- scene3d()
#'
#'
#'        # observeEvent(input$threshold, {
#'        #   cat("got event \n")
#'        #   cat("thresh = ", input$threshold)
#'        #   rgl.set(dev)
#'        #   newsurf <- viewSurface(surfgeom, vals, col, add_normals=TRUE, threshold=input$threshold, irange=input$range)
#'        #   cat("newsurf id:", as.integer(newsurf))
#'        #   cat("old surf id: ", as.integer(start_surf$my_mesh))
#'        #   session$sendCustomMessage("sceneChange",
#'        #                             sceneChange("surface_widget", replace=start_surf$my_mesh, skipRedraw=TRUE))
#'        #
#'        #   start_surf$my_mesh <- newsurf
#'        #   session$onFlushed(function()
#'        #     session$sendCustomMessage("sceneChange",
#'        #                               sceneChange("surface_widget", skipRedraw = FALSE)))
#'        # })
#'
#'
#'       output$surface_widget <- rgl::renderRglwidget({
#'         rgl.set(dev)
#'
#'         cat("new thresh ", input$threshold)
#'         #viewSurface(surfgeom, vals, col, add_normals=TRUE, threshold=input$threshold, irange=input$range)
#'         #start_surf()
#'         viewSurface(surfgeom, vals, col, threshold=input$threshold, irange=input$range)
#'         scene1 <- scene3d()
#'         rglwidget(scene1, width=500, height=500)
#'         #rglwidget(scene1)
#'       })
#'     }
#'   )
#'
#'   runApp(app, launch.browser=TRUE)
#' }
#'
#'

