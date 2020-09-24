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
#' fitted(tree_mf)
#' resid(tree_mf, X)
#'
#' masked_approximation(tree_mf, X)
#'
predict.svd_like <- function(object, X = NULL, ...) {

  if (is.null(X)) {
    warn_on_large_reconstruction(object)
    reconstruction <- object$u %*% diag(object$d) %*% t(object$v)
    return(reconstruction)
  }

  masked_approximation(object, X)
}

#' @export
#' @rdname predict.svd_like
residuals.svd_like <- function(object, X, ...) {
  stats::predict(object, X) - X
}

#' @export
#' @rdname predict.svd_like
fitted.svd_like <- predict.svd_like

#' TODO
#'
#' TODO: this needs to become a generic so it
#' can also work for factor analysis objects
#'
#' @param object
#'
#' @param mask
#'
#' @export
masked_approximation <- function(object, mask) {
  mask <- methods::as(mask, "TsparseMatrix")
  masked_approximation_impl(object$u, object$d, object$v, mask@i, mask@j)
}

warn_on_large_reconstruction <- function(mf) {

  num_elements <- nrow(mf$u) * nrow(mf$v)

  if (num_elements > 10000)
    warning(
      glue(
        "Creating a dense reconstruction of a matrix ",
        "with {num_elements} elements."
      ),
      call. = FALSE
    )
}
