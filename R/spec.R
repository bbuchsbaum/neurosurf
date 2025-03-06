#' @importFrom stringr str_trim
#' @importFrom stringr str_split
#' @noRd
#' @keywords internal
load_spec <- function(spec) {
  base_dir <- dirname(normalizePath(spec))

  lin <- readLines(spec)
  lin <- lapply(lin, stringr::str_trim)

  newsurf_lines <-  grep("NewSurface", lin)
  stdef_lines <- grep("StateDef", lin)

  states <- unlist(lapply(lin[stdef_lines], function(sdef) {
    str_trim(str_split(sdef, "=")[[1]][[2]])
  }))

  keyval <- lapply(newsurf_lines, function(ns) {
    print(ns)
    lnum <- ns
    vars <- list()
    keys <- list()
    count <- 1

    while (((lnum + count) < length(lin)) && lin[[lnum + count]] != "") {
      curline <- lin[[lnum + count]]
      if (curline == "NewSurface") {
        break
      }

      ret <- str_trim(str_split(lin[[lnum + count]], "=")[[1]])

      vars[[count]] <- ret[[2]]
      keys[[count]] <- ret[[1]]
      count <- count + 1
    }
    names(vars) <- unlist(keys)
    vars
  })

  meshdomain <- sapply(keyval, function(x) x$LocalDomainParent)
  meshdomain <- meshdomain[(meshdomain != "./SAME") & (meshdomain != "SAME")]
  meshdomain <- meshdomain[!sapply(meshdomain, is.null)]
  domain <- meshdomain[[1]]

  curvature <- sapply(keyval, function(x) x$LocalCurvatureParent)
  curvature <- curvature[(curvature != "./SAME") & (curvature != "SAME")]
  curvature <- curvature[!sapply(curvature, is.null)]
  curvature <- curvature[[1]]

  surfaces <- sapply(keyval, function(x) {
    if (!is.null(x[["SurfaceName"]])) {
      x[["SurfaceName"]]
    } else if (!is.null(x[["FreeSurferSurface"]])) {
      x[["FreeSurferSurface"]]
    } else {
      stop(paste("Missing SurfaceName: ", x))
    }
  })

  embedDim <- sapply(keyval, function(x) {
    if (!is.null(x[["EmbedDimension"]])) {
      x[["EmbedDimension"]]
    } else {
      3
    }
  })


}


#' @importFrom stringr str_trim
#' @importFrom stringr str_split
#' @noRd
#' @keywords internal
load_spec <- function(spec) {
  base_dir <- dirname(normalizePath(spec))

  lin <- readLines(spec)
  lin <- lapply(lin, stringr::str_trim)

  newsurf_lines <-  grep("NewSurface", lin)
  stdef_lines <- grep("StateDef", lin)

  states <- unlist(lapply(lin[stdef_lines], function(sdef) {
    str_trim(str_split(sdef, "=")[[1]][[2]])
  }))

  keyval <- lapply(newsurf_lines, function(ns) {
    print(ns)
    lnum <- ns
    vars <- list()
    keys <- list()
    count <- 1

    while (((lnum + count) < length(lin)) && lin[[lnum + count]] != "") {
      curline <- lin[[lnum + count]]
      if (curline == "NewSurface") {
        break
      }

      ret <- str_trim(str_split(lin[[lnum + count]], "=")[[1]])

      vars[[count]] <- ret[[2]]
      keys[[count]] <- ret[[1]]
      count <- count + 1
    }
    names(vars) <- unlist(keys)
    vars
  })

  meshdomain <- sapply(keyval, function(x) x$LocalDomainParent)
  meshdomain <- meshdomain[(meshdomain != "./SAME") & (meshdomain != "SAME")]
  meshdomain <- meshdomain[!sapply(meshdomain, is.null)]
  domain <- meshdomain[[1]]

  curvature <- sapply(keyval, function(x) x$LocalCurvatureParent)
  curvature <- curvature[(curvature != "./SAME") & (curvature != "SAME")]
  curvature <- curvature[!sapply(curvature, is.null)]
  curvature <- curvature[[1]]

  surfaces <- sapply(keyval, function(x) {
    if (!is.null(x[["SurfaceName"]])) {
      x[["SurfaceName"]]
    } else if (!is.null(x[["FreeSurferSurface"]])) {
      x[["FreeSurferSurface"]]
    } else {
      stop(paste("Missing SurfaceName: ", x))
    }
  })

  embedDim <- sapply(keyval, function(x) {
    if (!is.null(x[["EmbedDimension"]])) {
      x[["EmbedDimension"]]
    } else {
      3
    }
  })


}