library(shiny)
library(neurosurf)

# Assuming the read_surf function is defined in the neurosurf package


ui <- fluidPage(
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    # Include Three.js before neurosurface.js
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"),
    # Manually include the neurosurface dependencies
    tags$script(src = "index.js"),
    tags$script(src = "neurosurface.js"),
    tags$link(rel = "stylesheet", href = "neurosurface.css"),
    tags$style(HTML("
      html, body { height: 100%; }
      #neurosurfaceWidget { height: calc(100vh - 100px); }
    "))
  ),
  titlePanel("Neurosurface Widget Example"),
  fluidRow(
    column(12,
      neurosurfaceOutput("neurosurfaceWidget", width = "100%", height = "100%")
    )
  )
)

server <- function(input, output) {
  white_lh_asc <- system.file("extdata", "std.8_lh.smoothwm.asc", package="neurosurf")
  white_surf <- read_surf(white_lh_asc)
  output$neurosurfaceWidget <- renderNeurosurface({
    neurosurface(white_surf, width = "100%", height = "600px")
  })
}

shinyApp(ui = ui, server = server)
