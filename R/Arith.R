


setMethod(f="Arith", signature=signature(e1="NeuroSurface", e2="NeuroSurface"),
          def=function(e1, e2) {
            assert_that(length(nodes(e1)) == length(nodes(e2)))
            res <- callGeneric(e1@data,e2@data)

            ind <- sort(union(e1@indices, e2@indices))
            NeuroSurface(geometry=e1@geometry, indices=ind, data=res)

          })

setMethod(f="Arith", signature=signature(e1="NeuroSurface", e2="numeric"),
          def=function(e1, e2) {
            ind <- 1:length(nodes(e1))
            res <- callGeneric(e1@data, e2)
            NeuroSurface(geometry=e1@geometry, indices=ind, data=res)

          })

setMethod(f="Arith", signature=signature(e1="numeric", e2="NeuroSurface"),
          def=function(e1, e2) {
            ind <- 1:length(nodes(e2))
            res <- callGeneric(e1, e2@data)
            NeuroSurface(geometry=e2@geometry, indices=ind, data=res)

          })

#' @importFrom assertthat assert_that
setMethod(f="Arith", signature=signature(e1="NeuroSurfaceVector", e2="NeuroSurfaceVector"),
          def=function(e1, e2) {
            assert_that(length(nodes(e1)) == length(nodes(e2)))
            assert_that(all(dim(e1@data) == dim(e2@data)))

            res <- callGeneric(e1@data,e2@data)
            ind <- sort(union(e1@indices, e2@indices))
            NeuroSurfaceVector(geometry=e1@geometry, indices=ind, mat=res)

          })

#' @importFrom assertthat assert_that
setMethod(f="Arith", signature=signature(e1="NeuroSurfaceVector", e2="numeric"),
          def=function(e1, e2) {
            res <- callGeneric(e1@data,e2)
            ind <- e1@indices
            NeuroSurfaceVector(geometry=e1@geometry, indices=ind, mat=res)
          })


#' @importFrom assertthat assert_that
setMethod(f="Arith", signature=signature(e1="numeric", e2="NeuroSurfaceVector"),
          def=function(e1, e2) {
            res <- callGeneric(e1,e2@data)
            ind <- e2@indices
            NeuroSurfaceVector(geometry=e2@geometry, indices=ind, mat=res)
          })
