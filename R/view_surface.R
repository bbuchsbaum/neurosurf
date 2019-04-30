


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
  ifelse(vals > median(vals), incol, outcol)
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


view_surface <- function(surfgeom, vals=NA,
                        cmap=rainbow(256, alpha = 1),
                        vert_clrs=NULL,
                        bgcol = "lightgray",
                        alpha=1,
                        add_normals=TRUE,
                        threshold=NULL,
                        irange=range(vals,na.rm=TRUE),
                        specular=specular,
                        viewpoint=c("lateral","medial", "ventral", "posterior"),
                        sfac=1,
                        ...) {


  if (add_normals) {
    surfgeom@mesh <- rgl::addNormals(surfgeom@mesh)
  }

  viewpoint <- match.arg(viewpoint)

  umat <- if (surfgeom@hemi == "lh") {
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
    bg_layer <- colorplane::HexColorPlane(rep(bgcol, length(nodes(surfgeom))))
  } else {
    bg_layer <- colorplane::HexColorPlane(bgcol)
  }


  if (!is.na(vals) && !is.null(vals) && is.null(vert_clrs)) {
    fg_layer <- colorplane::IntensityColorPlane(vals, cmap,alpha=1)
    fg_clrs <- colorplane::map_colors(fg_layer, alpha=alpha, threshold=threshold, irange=irange)
    combined <- colorplane::blend_colors(bg_layer, fg_clrs, alpha=alpha)
    vertex_cols <- colorplane::as_hexcol(combined)
  } else if (!is.null(vert_clrs)) {
    fg_layer <- colorplane::HexColorPlane(vert_clrs, cmap,alpha=1)
    fg_clrs <- fg_layer@clrs
    combined <- colorplane::blend_colors(bg_layer, fg_clrs, alpha=alpha)
    vertex_cols <- colorplane::as_hexcol(combined)
  } else {
    vertex_cols <- colorplane::as_hexcol(bg_layer)
  }

  if (sfac != 1) {
    umat <- umat %*% scaleMatrix(sfac,sfac,sfac)
  }

  #rgl::shade3d(surfgeom@mesh,col=vertex_cols[surfgeom@mesh$it], specular=specular, meshColor="legacy", ...)
  rgl::shade3d(surfgeom@mesh,col=vertex_cols, specular=specular, meshColor="vertices", ...)
  rgl::par3d(userMatrix = umat)



}

#' plot an image
#'
#' @rdname plot-methods
#' @param x the surface to display
#' @param vals the \code{vector} of values at each surface node.
#' @param cmap a color map consisting of a vector of colors in hex format (e.g. \code{gray(n=255)})
#' @param vert_clrs vertex colors in hex format
#' @param irange the intensity range indicating the low and high values of the color scale.
#' @param thresh a 2-element vector indicating the lower and upper transparency thresholds.
#' @param alpha the foreground trasnparency, default is 1 (opaque).
#' @param bgvol a background color or vector of colors used to shade the surface.
#' @export
#' @importFrom graphics plot
setMethod("plot", signature=signature(x="SurfaceGeometry"),
          def=function(x,vals=NA, cmap=gray(seq(0,1,length.out=255)),
                       vert_clrs=NULL,
                       irange=range(vals),
                       thresh=c(0,0),
                       alpha=1,
                       specular="black",
                       bgcol="lightgray", ...) {

            view_surface(x,vals,cmap=cmap,vert_clrs=vert_clrs, irange=irange,thresh=thresh,alpha=alpha,bgcol=bgcol,specular=specular,...)

          })


#' @export
setMethod("plot", signature=signature(x="NeuroSurface"),
          def=function(x,cmap=gray(seq(0,1,length.out=255)),
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

