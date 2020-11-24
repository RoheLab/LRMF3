# rowSums and colSums methods for SVD objects

# note that rowSums and colSums are functions (not generics) in base R
# however, in Matrix, loaded via Depends, they are both S4 generics
# so we implement S4 generics for svd_like objects below. since svd_like
# objects are S3 objects, we use the old class trick to enable S4 dispatch
# on S3 objects

setOldClass("svd_like")

#' @export
setMethod(
  "rowSums",
  signature = c("svd_like"),
  definition = function(x, ...) {
    as.numeric(rowSums_svd_like_impl(x$u, x$d, x$v, getOption("Ncpus", 1)))
  }
)

#' @export
setMethod(
  "colSums",
  signature = c("svd_like"),
  definition = function(x, ...) {
    as.numeric(colSums_svd_like_impl(x$u, x$d, x$v, getOption("Ncpus", 1)))
  }
)
