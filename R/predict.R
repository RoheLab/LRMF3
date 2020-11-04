# methods in this file are largely experimental at the moment,
# although fastadi does depend on masked_approximation()

#' TODO
#'
#' @param object TODO
#' @param ... TODO
#' @param X TODO
#'
#' @return TODO
#' @export
#'
#' @examples
#'
#' X <- as.matrix(trees)
#' tree_mf <- as_svd_like(svd(X))
#'
#' predict(tree_mf)
#' resid(tree_mf, X)
#'
#'
predict.svd_like <- function(object, X = NULL, ...) {

  VDt <- tcrossprod(object$v, diag(object$d))

  if (is.null(X)) {
    warn_on_large_reconstruction(object$u, VDt)
    reconstruction <- object$u %*% VDt
    return(reconstruction)
  }

  mask <- methods::as(X, "TsparseMatrix")
  masked_approximation_impl(object$u, VDt, mask@i, mask@j)
}

#' Title
#'
#' @param object TODO
#' @param X TODO
#' @param ... TODO
#'
#' @return TODO
#' @export
#'
predict.fa_like <- function(object, X = NULL, ...) {

  YBt <- tcrossprod(object$Y, object$B)

  if (is.null(X)) {
    warn_on_large_reconstruction(object$Z, YBt)
    reconstruction <- object$Z %*% YBt
    return(reconstruction)
  }

  mask <- methods::as(X, "TsparseMatrix")

  masked_approximation_impl(as.matrix(object$Z), as.matrix(YBt), mask@i, mask@j)
}

#' @export
residuals.LRMF <- function(object, X, ...) {
  stats::predict(object, X) - X
}

warn_on_large_reconstruction <- function(U, V) {

  num_elements <- nrow(U) * nrow(V)

  if (num_elements > 10000)
    warning(
      glue(
        "Creating a dense reconstruction of a matrix ",
        "with {num_elements} elements."
      ),
      call. = FALSE
    )
}

#' @export
masked_approximation <- function(s, mask) {
  mask <- methods::as(mask, "TsparseMatrix")
  masked_approximation_impl(s$u, s$v %*% diag(s$d), mask@i, mask@j)
}
