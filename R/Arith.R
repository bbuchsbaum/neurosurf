#' @include all_class.R
#' @include all_generic.R
NULL

#' Comparison Operations for NeuroSurface Objects
#'
#' @param e1 NeuroSurface object
#' @param e2 Numeric value
#' @return NeuroSurface object with comparison results
#' @exportMethod Compare
setMethod(f="Compare", signature=signature(e1="NeuroSurface", e2="numeric"),
          def=function(e1, e2) {
            ret <- callGeneric(e1@data,e2)
            NeuroSurface(e1@geometry, e1@indices, as.numeric(ret))
          })


#' Arithmetic Operations for NeuroSurface Objects
#'
#' @param e1 NeuroSurface object or numeric value
#' @param e2 NeuroSurface object or numeric value
#' @return NeuroSurface object with arithmetic operation results
#' @importFrom assertthat assert_that
#' @exportMethod Arith
setMethod(f="Arith", signature=signature(e1="NeuroSurface", e2="NeuroSurface"),
          def=function(e1, e2) {
            assert_that(length(nodes(e1)) == length(nodes(e2)))
            res <- callGeneric(e1@data,e2@data)

            ind <- sort(union(e1@indices, e2@indices))
            NeuroSurface(geometry=e1@geometry, indices=ind, data=res)

          })

#' @rdname Arith-NeuroSurface-method
setMethod(f="Arith", signature=signature(e1="NeuroSurface", e2="numeric"),
          def=function(e1, e2) {
            ind <- 1:length(nodes(e1))
            res <- callGeneric(e1@data, e2)
            NeuroSurface(geometry=e1@geometry, indices=ind, data=res)

          })

#' @rdname Arith-NeuroSurface-method
setMethod(f="Arith", signature=signature(e1="numeric", e2="NeuroSurface"),
          def=function(e1, e2) {
            ind <- 1:length(nodes(e2))
            res <- callGeneric(e1, e2@data)
            NeuroSurface(geometry=e2@geometry, indices=ind, data=res)

          })

#' Arithmetic Operations for NeuroSurfaceVector Objects
#'
#' @param e1 NeuroSurfaceVector object or numeric value
#' @param e2 NeuroSurfaceVector object or numeric value
#' @return NeuroSurfaceVector object with arithmetic operation results
#' @importFrom assertthat assert_that
#' @exportMethod Arith
setMethod(f="Arith", signature=signature(e1="NeuroSurfaceVector", e2="NeuroSurfaceVector"),
          def=function(e1, e2) {
            assert_that(length(nodes(e1)) == length(nodes(e2)))
            assert_that(all(dim(e1@data) == dim(e2@data)))

            res <- callGeneric(e1@data,e2@data)
            ind <- sort(union(e1@indices, e2@indices))
            NeuroSurfaceVector(geometry=e1@geometry, indices=ind, mat=res)

          })

#' @rdname Arith-NeuroSurfaceVector-method
setMethod(f="Arith", signature=signature(e1="NeuroSurfaceVector", e2="numeric"),
          def=function(e1, e2) {
            res <- callGeneric(e1@data,e2)
            ind <- e1@indices
            NeuroSurfaceVector(geometry=e1@geometry, indices=ind, mat=res)
          })


#' @rdname Arith-NeuroSurfaceVector-method
setMethod(f="Arith", signature=signature(e1="numeric", e2="NeuroSurfaceVector"),
          def=function(e1, e2) {
            res <- callGeneric(e1,e2@data)
            ind <- e2@indices
            NeuroSurfaceVector(geometry=e2@geometry, indices=ind, mat=res)
          })

#' Comparison Operations for NeuroSurfaceVector Objects
#'
#' @param e1 NeuroSurfaceVector object or numeric value
#' @param e2 NeuroSurfaceVector object or numeric value
#' @return NeuroSurfaceVector object with comparison results
#' @importFrom assertthat assert_that
#' @exportMethod Compare
setMethod(f="Compare", signature=signature(e1="NeuroSurfaceVector", e2="NeuroSurfaceVector"),
          def=function(e1, e2) {
            assert_that(length(nodes(e1)) == length(nodes(e2)))
            assert_that(all(dim(e1@data) == dim(e2@data)))
            res <- callGeneric(e1@data, e2@data)
            ind <- sort(union(e1@indices, e2@indices))
            NeuroSurfaceVector(geometry=e1@geometry, indices=ind, mat=res)
          })

#' @rdname Compare-NeuroSurfaceVector-method
setMethod(f="Compare", signature=signature(e1="NeuroSurfaceVector", e2="numeric"),
          def=function(e1, e2) {
            res <- callGeneric(e1@data, e2)
            NeuroSurfaceVector(geometry=e1@geometry, indices=e1@indices, mat=res)
          })

#' @rdname Compare-NeuroSurfaceVector-method
setMethod(f="Compare", signature=signature(e1="numeric", e2="NeuroSurfaceVector"),
          def=function(e1, e2) {
            res <- callGeneric(e1, e2@data)
            NeuroSurfaceVector(geometry=e2@geometry, indices=e2@indices, mat=res)
          })

#' Comparison Operations for NeuroSurface Objects
#'
#' @param e1 NeuroSurface object
#' @param e2 NeuroSurface object
#' @return NeuroSurface object with comparison results
#' @importFrom assertthat assert_that
#' @exportMethod Compare
setMethod(f="Compare", signature=signature(e1="NeuroSurface", e2="NeuroSurface"),
          def=function(e1, e2) {
            assert_that(length(nodes(e1)) == length(nodes(e2)))
            res <- callGeneric(e1@data, e2@data)
            ind <- sort(union(e1@indices, e2@indices))
            NeuroSurface(geometry=e1@geometry, indices=ind, data=as.numeric(res))
          })

#' Arithmetic Operations between NeuroSurface and NeuroSurfaceVector
#'
#' @param e1 NeuroSurface or NeuroSurfaceVector object
#' @param e2 NeuroSurface or NeuroSurfaceVector object
#' @return NeuroSurfaceVector object with arithmetic operation results
#' @importFrom assertthat assert_that
#' @exportMethod Arith
setMethod(f="Arith", signature=signature(e1="NeuroSurface", e2="NeuroSurfaceVector"),
          def=function(e1, e2) {
            assert_that(length(nodes(e1)) == length(nodes(e2)))
            res <- callGeneric(e1@data, e2@data)
            ind <- sort(union(e1@indices, e2@indices))
            NeuroSurfaceVector(geometry=e1@geometry, indices=ind, mat=res)
          })

#' @rdname Arith-NeuroSurface-NeuroSurfaceVector-method
setMethod(f="Arith", signature=signature(e1="NeuroSurfaceVector", e2="NeuroSurface"),
          def=function(e1, e2) {
            assert_that(length(nodes(e1)) == length(nodes(e2)))
            res <- callGeneric(e1@data, e2@data)
            ind <- sort(union(e1@indices, e2@indices))
            NeuroSurfaceVector(geometry=e1@geometry, indices=ind, mat=res)
          })
