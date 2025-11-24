#' @include all_class.R
#' @include all_generic.R
NULL

.ns_same_geometry <- function(g1, g2) {
  identical(g1, g2) ||
    (identical(g1@mesh$vb, g2@mesh$vb) &&
       identical(g1@mesh$it, g2@mesh$it))
}

.ns_assert_compatible <- function(e1, e2, what = "operation") {
  assert_that(length(nodes(e1)) == length(nodes(e2)),
              msg = sprintf("%s: node counts differ", what))
  assert_that(identical(e1@indices, e2@indices),
              msg = sprintf("%s: indices differ", what))
  assert_that(.ns_same_geometry(e1@geometry, e2@geometry),
              msg = sprintf("%s: geometries differ", what))
}

#' Comparison Operations for NeuroSurface Objects
#'
#' @param e1 the left operand
#' @param e2 the right operand
#' @return NeuroSurface object with comparison results
#' @exportMethod Compare
#' @rdname Compare-NeuroSurface-numeric-method
setMethod(f="Compare", signature=signature(e1="NeuroSurface", e2="numeric"),
          def=function(e1, e2) {
            ret <- callGeneric(e1@data,e2)
            NeuroSurface(e1@geometry, e1@indices, as.numeric(ret))
          })


#' Arithmetic Operations for NeuroSurface Objects
#'
#' @param e1 the left operand
#' @param e2 the right operand
#' @return NeuroSurface object with arithmetic operation results
#' @importFrom assertthat assert_that
#' @exportMethod Arith
#' @rdname Arith-NeuroSurface-method
setMethod(f="Arith", signature=signature(e1="NeuroSurface", e2="NeuroSurface"),
          def=function(e1, e2) {
            .ns_assert_compatible(e1, e2, "Arith")
            res <- callGeneric(e1@data,e2@data)

            NeuroSurface(geometry=e1@geometry, indices=e1@indices, data=res)

          })

#' @rdname Arith-NeuroSurface-method
#' @export
setMethod(f="Arith", signature=signature(e1="NeuroSurface", e2="numeric"),
          def=function(e1, e2) {
            ind <- e1@indices
            res <- callGeneric(e1@data, e2)
            NeuroSurface(geometry=e1@geometry, indices=ind, data=res)

          })

#' @rdname Arith-NeuroSurface-method
#' @export
setMethod(f="Arith", signature=signature(e1="numeric", e2="NeuroSurface"),
          def=function(e1, e2) {
            ind <- e2@indices
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
#' @rdname Arith-NeuroSurfaceVector-method
setMethod(f="Arith", signature=signature(e1="NeuroSurfaceVector", e2="NeuroSurfaceVector"),
          def=function(e1, e2) {
            .ns_assert_compatible(e1, e2, "Arith")
            assert_that(all(dim(e1@data) == dim(e2@data)),
                        msg = "Arith: data matrices differ in shape")

            res <- callGeneric(e1@data,e2@data)
            NeuroSurfaceVector(geometry=e1@geometry, indices=e1@indices, mat=res)

          })

#' @rdname Arith-NeuroSurfaceVector-method
#' @export
setMethod(f="Arith", signature=signature(e1="NeuroSurfaceVector", e2="numeric"),
          def=function(e1, e2) {
            res <- callGeneric(e1@data,e2)
            ind <- e1@indices
            NeuroSurfaceVector(geometry=e1@geometry, indices=ind, mat=res)
          })


#' @rdname Arith-NeuroSurfaceVector-method
#' @export
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
#' @rdname Compare-NeuroSurfaceVector-method
setMethod(f="Compare", signature=signature(e1="NeuroSurfaceVector", e2="NeuroSurfaceVector"),
          def=function(e1, e2) {
            .ns_assert_compatible(e1, e2, "Compare")
            assert_that(all(dim(e1@data) == dim(e2@data)),
                        msg = "Compare: data matrices differ in shape")
            res <- callGeneric(e1@data, e2@data)
            NeuroSurfaceVector(geometry=e1@geometry, indices=e1@indices,
                               mat=Matrix::Matrix(as.numeric(res), nrow = nrow(res)))
          })

#' @rdname Compare-NeuroSurfaceVector-method
#' @export
setMethod(f="Compare", signature=signature(e1="NeuroSurfaceVector", e2="numeric"),
          def=function(e1, e2) {
            res <- callGeneric(e1@data, e2)
            NeuroSurfaceVector(geometry=e1@geometry, indices=e1@indices,
                               mat=Matrix::Matrix(as.numeric(res), nrow = nrow(res)))
          })

#' @rdname Compare-NeuroSurfaceVector-method
#' @export
setMethod(f="Compare", signature=signature(e1="numeric", e2="NeuroSurfaceVector"),
          def=function(e1, e2) {
            res <- callGeneric(e1, e2@data)
            NeuroSurfaceVector(geometry=e2@geometry, indices=e2@indices,
                               mat=Matrix::Matrix(as.numeric(res), nrow = nrow(res)))
          })


#' Comparison Operations for NeuroSurface Objects
#'
#' @param e1 NeuroSurface object
#' @param e2 NeuroSurface object
#' @return NeuroSurface object with comparison results
#' @importFrom assertthat assert_that
#' @exportMethod Compare
#' @rdname Compare-NeuroSurface-method
setMethod(f="Compare", signature=signature(e1="NeuroSurface", e2="NeuroSurface"),
          def=function(e1, e2) {
            .ns_assert_compatible(e1, e2, "Compare")
            res <- callGeneric(e1@data, e2@data)
            NeuroSurface(geometry=e1@geometry, indices=e1@indices, data=as.numeric(res))
          })


#' @rdname Arith-NeuroSurface-method
#' @export
setMethod(f="Arith", signature=signature(e1="NeuroSurface", e2="NeuroSurfaceVector"),
          def=function(e1, e2) {
            .ns_assert_compatible(e1, e2, "Arith")
            res <- callGeneric(e1@data, e2@data)
            NeuroSurfaceVector(geometry=e1@geometry, indices=e1@indices, mat=res)
          })

#' @rdname Arith-NeuroSurfaceVector-method
#' @export
setMethod(f="Arith", signature=signature(e1="NeuroSurfaceVector", e2="NeuroSurface"),
          def=function(e1, e2) {
            .ns_assert_compatible(e1, e2, "Arith")
            res <- callGeneric(e1@data, e2@data)
            NeuroSurfaceVector(geometry=e1@geometry, indices=e1@indices, mat=res)
          })
