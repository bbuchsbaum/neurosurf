


setMethod(f="Arith", signature=signature(e1="BrainSurface", e2="BrainSurface"),
          def=function(e1, e2) {
            assert_that(length(nodes(e1)) == length(nodes(e2)))
            res <- callGeneric(e1@data,e2@data)

            ind <- sort(union(e1@indices, e2@indices))
            BrainSurface(geometry=e1@geometry, indices=ind, data=res)

          })

setMethod(f="Arith", signature=signature(e1="BrainSurface", e2="numeric"),
          def=function(e1, e2) {
            ind <- 1:length(nodes(e1))
            res <- callGeneric(e1@data, e2)
            BrainSurface(geometry=e1@geometry, indices=ind, data=res)

          })

setMethod(f="Arith", signature=signature(e1="numeric", e2="BrainSurface"),
          def=function(e1, e2) {
            ind <- 1:length(nodes(e2))
            res <- callGeneric(e1, e2@data)
            BrainSurface(geometry=e2@geometry, indices=ind, data=res)

          })

#' @importFrom assertthat assert_that
setMethod(f="Arith", signature=signature(e1="BrainSurfaceVector", e2="BrainSurfaceVector"),
          def=function(e1, e2) {
            assert_that(length(nodes(e1)) == length(nodes(e2)))
            assert_that(all(dim(e1@data) == dim(e2@data)))

            res <- callGeneric(e1@data,e2@data)
            ind <- sort(union(e1@indices, e2@indices))
            BrainSurfaceVector(geometry=e1@geometry, indices=ind, mat=res)

          })

#' @importFrom assertthat assert_that
setMethod(f="Arith", signature=signature(e1="BrainSurfaceVector", e2="numeric"),
          def=function(e1, e2) {
            res <- callGeneric(e1@data,e2)
            ind <- e1@indices
            BrainSurfaceVector(geometry=e1@geometry, indices=ind, mat=res)
          })


#' @importFrom assertthat assert_that
setMethod(f="Arith", signature=signature(e1="numeric", e2="BrainSurfaceVector"),
          def=function(e1, e2) {
            res <- callGeneric(e1,e2@data)
            ind <- e2@indices
            BrainSurfaceVector(geometry=e2@geometry, indices=ind, mat=res)
          })
