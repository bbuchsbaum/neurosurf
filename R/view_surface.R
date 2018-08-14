


#' plotlySurface <- function(surfgeom, vals, col=brewer_pal(palette="RdBu")(9),
#'                           alpha=1,
#'                           add_normals=FALSE,
#'                           threshold=NULL,
#'                           irange=NULL,
#'                           bgcol="#D3D3D3") {
#'
#'
#'
#'   curv <- curvature(surfgeom)
#'
#'   cmat <- rbind(vals[surfgeom@mesh$it[1,]],vals[surfgeom@mesh$it[2,]],vals[surfgeom@mesh$it[3,]] )
#'   fvals <- colMeans(cmat)
#'
#'   fg_layer <- IntensityColorPlane(fvals, col,alpha=1)
#'   fg_clrs <- map_colors(fg_layer, alpha=alpha, threshold=threshold, irange=irange)
#'
#'   #bg_layer <- IntensityColorPlane(curv, c("#D3D3D3FF", "#A9A9A9FF", "#A9A9A9FF"),alpha=1)
#'   #bg_clrs <- map_colors(bg_layer, alpha=1)
#'
#'   #browser()
#'
#'
#'   #combined <- blend_colors(bg_clrs, fg_clrs, alpha=alpha)
#'   #face_cols <- as_hexcol(combined)
#'   face_cols <- as_hexcol(fg_clrs)
#'
#'   cds <- coords(surfgeom)
#'   p <- plot_ly(
#'     x = cds[,1], y=cds[,2], z = cds[,3],
#'     i = surfgeom@mesh$it[1,]-1, j =surfgeom@mesh$it[2,]-1, k = surfgeom@mesh$it[3,]-1,
#'     facecolor=face_cols,
#'     #vertexcolor=face_cols,
#'     #intensity=vals,
#'     type = "mesh3d",
#'     flatshading=TRUE
#'   )
#'   p
#'
#'
#' }
#'
#'
#'
#' #' @importFrom gplots col2hex
#' #' @export
#' viewSurface <- function(surfgeom, vals, col=rainbow(256, alpha = 1),
#'                         bg_col = "lightgray",
#'                         alpha=1,
#'                         add_normals=FALSE,
#'                         threshold=NULL,
#'                         irange=range(vals),
#'                         bgcol="#D3D3D3") {
#'
#'
#'   if (add_normals) {
#'     surfgeom@mesh <- rgl::addNormals(surfgeom@mesh)
#'   }
#'
#'
#'   face_vals <- rowMeans(cbind(vals[surfgeom@mesh$it[1,]],vals[surfgeom@mesh$it[2,]],vals[surfgeom@mesh$it[3,]]))
#'   fg_layer <- IntensityColorPlane(face_vals, col,alpha=1)
#'
#'   #fg_layer <- IntensityColorPlane(vals, colmap,alpha=1)
#'   fg_clrs <- map_colors(fg_layer, alpha=alpha, threshold=threshold, irange=irange)
#'
#'   if (is.character(bg_col)) {
#'     bgcol <- gplots::col2hex(bg_col)
#'   }
#'
#'   if (length(bgcol) == 1) {
#'     bg_layer <- HexColorPlane(rep(bgcol, length(face_vals)))
#'   } else {
#'     bg_layer <- HexColorPlane(bgcol)
#'   }
#'
#'   combined <- blend_colors(bg_layer, fg_clrs, alpha)
#'   vertex_cols <- as_hexcol(combined)
#'   #shade3d(surfgeom@mesh, col=rep(vertex_cols,3))
#'   rgl::shade3d(surfgeom@mesh, col=rep(vertex_cols,each=3))
#'   #shade3d(surfgeom@mesh, col=vertex_cols)
#'
#' }
#'
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

