

#' @imortFrom Rspectra eigs
commute_time <- function(A, ncomp=nrow(A)) {
  D <- Matrix::rowSums(A)
  Dtilde <- 1/(sqrt(D))

  #P <- diag(1/D) %*% A

  M <- Matrix::Diagonal(x=Dtilde) %*% A %*% Matrix::Diagonal(x=Dtilde)

  decomp <- RSpectra::eigs(M, k=ncomp)
  pii <- D/sum(A)
  v <- decomp$vectors[, 2:ncomp]

  cds <- sweep(v, 2, sqrt(1 - decomp$values[2:ncomp]), "/")
  cds <- sweep(cds, 2, 1/pi)

}
