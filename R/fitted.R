#' TODO
#'
#' @param object TODO
#' @param mask TODO
#' @param ... TODO
#'
#' @return
#' @export
#'
#' @examples
#'
#' X <- as.matrix(trees)
#' tree_mf <- as_mf(svd(X))
#'
#' predict(tree_mf)
#' fitted(tree_mf)
#' resid(tree_mf, X)
#'
#'
#' masked_approximation(tree_mf, X)
#'
predict.LRMF <- function(object, X = NULL, ...) {

  if (is.null(X)) {
    warn_on_large_reconstruction(object)
    reconstruction <- object$u %*% diag(object$d) %*% t(object$v)
    return(reconstruction)
  }

  masked_approximation(object, X)
}

#' @export
#' @rdname predict.LRMF
residuals.LRMF <- function(object, X, ...) {
  predict(object, X) - X
}

#' @export
#' @rdname predict.LRMF
fitted.LRMF <- predict.LRMF

#' @export
masked_approximation <- function(object, mask) {
  mask <- as(mask, "TsparseMatrix")
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
