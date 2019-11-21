## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(rgl)
library(neuroim2)
devtools::load_all()
#knitr::knit_hooks$set(webgl = hook_webgl)
inflated_lh_asc <- system.file("testdata", "std.40.lh.inflated.asc", package="neurosurf")
white_lh_asc <- system.file("testdata", "std.40.lh.smoothwm.asc", package="neurosurf")

white_rh_asc <- system.file("testdata", "std.40.rh.smoothwm.asc", package="neurosurf")
inflated_rh_asc <- system.file("testdata", "std.40.rh.inflated.asc", package="neurosurf")


