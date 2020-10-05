#' Create a SVD-like low rank matrix factorization object
#'
#' A low rank matrix factorization of a matrix `X` is
#' parameterized by `X ~= u %*% diag(d) %*% t(v)`. The
#' object is "svd-like" because the middle matrix in
#' the decomposition must be strictly diagonal.
#'
#' @param u A *matrix* "left singular-ish" vectors.
#'
#' @param d A *vector* of "singular-ish" values.
#'
#' @param v A *matrix* of "right singular-ish" vectors.
#'
#' @param subclasses A character vector of subclasses.
#'   Optional, defaults to `NULL`.
#'
#' @param ... Optional additional items to pass to the constructor.
#'
#' @examples
#'
#' s <- svd(as.matrix(trees))
#'
#' # using the constructor directly
#' svd_like(s$u, s$d, s$v)
#'
#' # coercing svd-like lists
#' as_svd_like(s)
#'
#' @export
svd_like <- function(u, d, v, subclasses = NULL, ...) {

  mf <- new_svd_like(
    u = u,
    d = d,
    v = v,
    rank = as.integer(ncol(u)),
    subclasses = subclasses,
    ...
  )

  validate_svd_like(mf)
}

new_svd_like <- function(u, d, v, rank, subclasses = NULL, ...) {

  mf <- list(
    u = u,
    d = d,
    v = v,
    rank = rank,
    ...
  )

  class(mf) <- c(subclasses, "svd_like", "LRMF")
  mf
}

validate_svd_like <- function(mf) {

  ### object type validation

  if (!(inherits(mf$u, "matrix")))
    stop("`u` must be a matrix object.", call. = FALSE)

  # d needs to be a numeric vector

  if (!is.numeric(mf$d) || !is.vector(mf$d))
    stop("`d` must be a numeric vector.", call. = FALSE)

  if (!(inherits(mf$v, "matrix")))
    stop("`v` must be a matrix object.", call. = FALSE)

  if (!is.integer(mf$rank))
    stop("`rank` must be an integer.", call. = FALSE)

  ### dimension validation

  if (ncol(mf$u) != length(mf$d))
    stop("Dimensions of `u` and `d` must match.", call. = FALSE)

  if (length(mf$d) != ncol(mf$v))
    stop("Dimensions of `d` and `v` must match.", call. = FALSE)

  if (length(mf$d) != mf$rank)
    stop("`rank` does not match `u`, `d`, and `v`.", call. = FALSE)

  mf
}

#' Coerce an object to LRMF class
#'
#' @param x Object to coerce
#' @param ... Ignored.
#'
#' @return Object as [svd_like()] object.
#' @export
as_svd_like <- function(x, ...) {
  UseMethod("as_svd_like")
}

#' @rdname as_svd_like
#' @export
as_svd_like.list <- function(x, ...) {

  if (!(all(c("u", "d", "v") %in% names(x))))
    stop(
      "Cannot coerce lists without elements `u`, `d`, and `v`.",
      call. = FALSE
    )

  svd_like(x$u, x$d, x$v)
}

#' @method print svd_like
#' @export
print.svd_like <- function(x, ...) {
  cat("Low Rank Matrix Factorization\n")
  cat("-----------------------------\n\n")


  cat(glue("Rank: {x$rank}"), sep = "\n\n")

  cat(glue("Rows: {nrow(x$u)}"), sep = "\n")
  cat(glue("Cols: {nrow(x$v)}"), sep = "\n\n")

  cat(glue("d[rank]: {x$d[x$rank]}"), sep = "\n\n")

  cat("Components\n\n")

  cat("u:", dim_and_class(x$u), "\n")
  cat("d:", dim_and_class(x$d), "\n")
  cat("v:", dim_and_class(x$v), "\n")
}
