# Debugging Helper for surfwidget

This function helps diagnose issues with surfwidget rendering by
checking common problems and providing debugging information.

## Usage

``` r
debug_surfwidget(x, verbose = TRUE)
```

## Arguments

- x:

  The surface object you're trying to visualize

- verbose:

  Logical, whether to print detailed information

## Value

Invisibly returns a list of diagnostic information

## Examples

``` r
# \donttest{
# Load a surface and check it
surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package="neurosurf")
surf <- read_surf(surf_file)
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/Rtmp0Nbniq/temp_libpath80e31f94425b/neurosurf/extdata/std.8_lh.smoothwm.asc
debug_surfwidget(surf)
#> === surfwidget Diagnostic Report ===
#> 
#> Object class: SurfaceGeometry 
#> Vertices: 642 
#> Faces: 1280 
#> Hemisphere: lh 
#> 
#> OK: Basic structure looks good
#> 
#> === JavaScript Debugging ===
#> 1. Open browser developer tools (F12)
#> 2. Go to Console tab
#> 3. Run: neurosurface.setDebug(true)
#> 4. Reload the page and check console for debug messages
#> 5. Look for error messages in red
#> 
#> === Common Solutions ===
#> - If widget is blank: Check JavaScript console for errors
#> - If data doesn't show: Verify data ranges and color mapping
#> - If widget doesn't load: Check if WebGL is supported
#> - Try with a simpler surface first
# }
```
