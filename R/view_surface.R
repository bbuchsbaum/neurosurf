


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

#' convert curvature vector to a set of binary colors
#'
#' @param vals the curvature values
#' @param incol the hex color for the sulci (inward)
#' @param outcol the hex color for the gyri (outward)
#' @export
curv_cols <- function(vals, incol="#B3B3B3", outcol="#404040") {
  ifelse(vals > stats::median(vals), incol, outcol)
}

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


#' Display a Brain Surface with RGL
#'
#' This function visualizes a 3D brain surface using the \code{rgl} package. It allows for the rendering of a surface with optional vertex colors, transparency, and lighting effects. Additionally, the function supports the display of spheres at specified coordinates on the surface, making it versatile for highlighting specific regions or points of interest.
#'
#' @param surfgeom A \code{\linkS4class{SurfaceGeometry}} object representing the 3D brain surface geometry to be displayed.
#' @param vals A numeric vector of values corresponding to each surface node. These values will be mapped to colors using the provided color map (\code{cmap}).
#' @param cmap A color map consisting of a vector of colors in hex format. Default is \code{rainbow(256, alpha = 1)}. This color map is used to color the surface based on the \code{vals} vector.
#' @param vert_clrs Optional vertex colors in hex format. If provided, these colors will override the colors generated from \code{vals} and \code{cmap}.
#' @param irange A numeric vector of length 2 indicating the lower and upper bounds of the intensity range for the color scale. Default is the range of \code{vals}.
#' @param thresh A numeric vector of length 2 indicating the lower and upper transparency thresholds. Nodes with values outside this range will be made transparent.
#' @param alpha A numeric value indicating the transparency level of the surface. The default is 1 (fully opaque). Values should be between 0 (fully transparent) and 1 (fully opaque).
#' @param add_normals Logical, indicating whether to add normals to the surface mesh. This is useful for improving the lighting effects. Default is \code{TRUE}.
#' @param viewpoint A character string specifying the initial viewpoint of the surface. Options are \code{"lateral"}, \code{"medial"}, \code{"ventral"}, or \code{"posterior"}.
#' @param specular A color in hex format or a numeric value indicating the specular reflection color used for lighting. Default is \code{"white"}.
#' @param bgcol A color or vector of colors in hex format used to shade the surface background. Default is \code{"lightgray"}.
#' @param offset A numeric vector of length 3 specifying the translation offset of the surface in the x, y, and z directions. Default is \code{c(0, 0, 0)}.
#' @param zoom A numeric value specifying the zoom factor. Default is 1 (no zoom).
#' @param new_window Logical, indicating whether to open a new RGL window for the surface display. Default is \code{TRUE}. If \code{FALSE}, the current RGL window will be cleared and reused.
#' @param spheres Optional. A data frame containing the coordinates (\code{x}, \code{y}, \code{z}), radii (\code{radius}), and optional colors (\code{color}) for spheres to be displayed on the surface. Each row represents a sphere.
#' @param ... Additional arguments passed to \code{rgl::shade3d}.
#'
#' @return An object returned by \code{rgl::shade3d} representing the rendered surface. This can be used for further manipulation of the rendered object.
#'
#' @importFrom gplots col2hex
#' @importFrom rgl open3d clear3d shade3d spheres3d view3d par3d addNormals
#'
#' @examples
#' \dontrun{
#'   # Example surface geometry object (assuming `white_surf` is preloaded)
#'   sphere_data <- data.frame(
#'     x = c(10, 20, 30),
#'     y = c(10, 20, 30),
#'     z = c(10, 20, 30),
#'     radius = c(2, 3, 4),
#'     color = c("#FF0000", "#00FF00", "#0000FF")
#'   )
#'
#'   # Display the surface with spheres
#'   view_surface(white_surf, viewpoint = "lateral", spheres = sphere_data)
#' }
#'
#' @seealso \code{\link[rgl]{shade3d}}, \code{\link[rgl]{spheres3d}}, \code{\link[rgl]{view3d}}
#' @export
view_surface <- function(surfgeom, vals=NA,
                         cmap=rainbow(256, alpha = 1),
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
#' @rdname plot
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
#' @rdname plot
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
#' @rdname plot
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
#' @rdname plot
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
#' @rdname plot
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

