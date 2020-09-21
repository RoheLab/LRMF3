#' Create a low rank matrix factorization object
#'
#' A low rank matrix factorization of a matrix `X` is
#' parameterized by `X ~= u %*% diag(d) %*% t(v)`.
#'
#' @param u The "left singular-ish" vectors.
#'
#' @param d The "singular-ish" values.
#'
#' @param v The "right singular-ish" vectors.
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
#' mf(s$u, s$d, s$v)
#'
#' # coercing svd-like lists
#' as_mf(s)
#'
#' @export
mf <- function(u, d, v, subclasses = NULL, ...) {

  mf <- new_mf(
    u = u,
    d = d,
    v = v,
    rank = as.integer(ncol(u)),
    subclasses = subclasses,
    ...
  )

  validate_mf(mf)
}


#' @export
new_mf <- function(u, d, v, rank, subclasses = NULL, ...) {

  object <- list(
    u = u,
    d = d,
    v = v,
    rank = rank,
    ...
  )

  class(object) <- c(subclasses, "LRMF")
  object
}

validate_mf <- function(mf) {

  ### object type validation

  if (!(inherits(mf$u, "matrix") || inherits(mf$u, "Matrix")))
    stop("`u` must be a matrix or Matrix object.", call. = FALSE)

  # d needs to be a numeric vector

  if (!is.numeric(mf$d) || !is.vector(mf$d))
    stop("`d` must be a numeric vector.", call. = FALSE)

  if (!(inherits(mf$v, "matrix") || inherits(mf$v, "Matrix")))
    stop("`v` must be a matrix or Matrix object.", call. = FALSE)

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

#' @export
print.LRMF <- function(x, ...) {
  cat("Low Rank Matrix Factorization\n")
  cat("-----------------------------\n\n")


  cat(glue("Rank: {x$rank}"), sep = "\n\n")

  cat(glue("Rows: {nrow(x$u)}"), sep = "\n")
  cat(glue("Cols: {nrow(x$v)}"), sep = "\n\n")

  cat(glue("d[rank]: {x$d[x$rank]}"), sep = "\n\n")

  cat("Components\n\n")

  dim_and_class <- function(x) {
    if (is.vector(x))
      paste0(length(x), "      [", class(x)[1], "]")
    else
      # is a matrix
      paste0(nrow(x), " x ", ncol(x), " [", class(x)[1], "]")
  }

  cat("u:", dim_and_class(x$u), "\n")
  cat("d:", dim_and_class(x$d), "\n")
  cat("v:", dim_and_class(x$v), "\n")
}
