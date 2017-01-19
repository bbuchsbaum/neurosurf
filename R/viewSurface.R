





viewSurface <- function(surfgeom, vals, col=heat.colors(128, alpha = 1),
                        bg_col = "lightgray",
                        alpha=1,
                        add_normals=FALSE,
                        zero_col="000000FF",
                        geom_col="lightgray") {


  if (add_normals) {
    surfgeom@mesh <- addNormals(surfgeom@mesh)
  }

  if (is.character(bg_col)) {
    bgcol <- col2hex(bg_col)
  }



  fg_clrs <- if (length(col) == length(nodes(surfgeom))) {
    col
  } else {
    v2 <- vals[as.vector(surfgeom@mesh$it)]
    neuroim::mapToColors(v2, col=col, alpha=alpha, zero.col=zero_col)
  }


  rgl::shade3d(surfgeom@mesh, col=fg_clrs, alpha=alpha, specular="black")
}

viewShiny <- function(surfgeom, vals=1:length(nodes(surfgeom)), col=rainbow(255, alpha = 1)) {
  options(rgl.useNULL = TRUE)

  app <- shinyApp(
    ui = fluidPage(
      registerSceneChange(),
      sidebarLayout(
        sidebarPanel(
          sliderInput("obs",
                      "Number of observations:",
                      min = 0,
                      max = 1000,
                      value = 500)
        ),
        mainPanel(h3("yolo"),
                  rgl::rglwidgetOutput("geometry", width = "100%", height = 512))
      )
    ),
    server = function(input, output, session) {
      options(rgl.useNULL=TRUE)
      open3d()

      save <- options(rgl.inShiny = TRUE)
      on.exit(options(save))

      open3d()

      viewSurface(surfgeom, vals, col)
      scene1 <- scene3d()

      session$onSessionEnded(function() {
        #rgl.set(dev)
        #rgl.close()
      })

      output$geometry <- rgl::renderRglwidget({
        rglwidget(scene1, width=500, height=500)
      })
    }
  )

  runApp(app, launch.browser=TRUE)
}



