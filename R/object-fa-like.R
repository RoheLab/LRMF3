#' Create a Factor Analysis-like low rank matrix factorization object
#'
#' A low rank matrix factorization of a matrix `X` is
#' parameterized by `X ~= X %*% B %*% t(Y)`. The
#' object is "factor analysis-like" because the middle
#' matrix in the decomposition is arbitrary rather than
#' diagonal.
#'
#' @param Z A *matrix* of embeddings for each observation.
#'
#' @param B A mixing *matrix* describing how observation embeddings
#'   and topics interact. Does not have to be diagonal!
#'
#' @param Y A *matrix* describing the compositions of various topics
#'   or factors.
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
#' fa_like(s$u, diag(s$d), s$v)
#'
#' @export
fa_like <- function(Z, B, Y, subclasses = NULL, ...) {

  all_dims <- c(dim(Z), dim(B), dim(Y))

  fa <- new_fa_like(
    Z = Z,
    B = B,
    Y = Y,
    rank = as.integer(min(all_dims)),
    subclasses = subclasses,
    ...
  )

  validate_fa_like(fa)
}

new_fa_like <- function(Z, B, Y, rank, subclasses = NULL, ...) {

  object <- list(
    Z = Z,
    B = B,
    Y = Y,
    rank = rank,
    ...
  )

  class(object) <- c(subclasses, "fa_like", "LRMF")
  object
}

validate_fa_like <- function(fa) {

  ### object type validation

  if (!(inherits(fa$Z, "matrix")))
    stop("`Z` must be a matrix object.", call. = FALSE)

  if (!(inherits(fa$B, "matrix")))
    stop("`B` must be a matrix object.", call. = FALSE)

  if (!(inherits(fa$Y, "matrix")))
    stop("`Y` must be a matrix object.", call. = FALSE)

  ### dimension validation

  if (ncol(fa$Z) != nrow(fa$B))
    stop("Dimensions of `Z` and `B` must match.", call. = FALSE)

  if (ncol(fa$B) != ncol(fa$Y))
    stop("Dimensions of `B` and `Y` must match.", call. = FALSE)

  if (fa$rank != min(c(dim(fa$Z), dim(fa$B), dim(fa$Y))))
    stop(
      "`rank` must match smallest dimension of `Z`, `B`, and `Y`.",
      call. = FALSE
    )

  fa
}

#' Coerce an object to a factor analysis like factorization
#'
#' @param x Object to coerce
#' @param ... Ignored.
#'
#' @return Object as [svd_like()] object.
#' @export
as_fa_like <- function(x, ...) {
  UseMethod("as_fa_like")
}

#' @rdname as_fa_like
#' @export
as_fa_like.list <- function(x, ...) {

  if (!(all(c("u", "d", "v") %in% names(x))))
    stop(
      "Cannot coerce lists without elements `u`, `d`, and `v`.",
      call. = FALSE
    )

  fa_like(x$u, diag(x$d), x$v)
}

#' @method print fa_like
#' @export
print.fa_like <- function(x, ...) {
  cat("Factor Analysis-Like Matrix Factorization\n")
  cat("---------------------------------------\n\n")


  cat(glue("Rank: {x$rank}"), sep = "\n\n")

  cat(glue("Rows: {nrow(x$Z)}"), sep = "\n")
  cat(glue("Cols: {nrow(x$Y)}"), sep = "\n\n")

  cat("Components\n\n")

  cat("Z:", dim_and_class(x$Z), "\n")
  cat("B:", dim_and_class(x$B), "\n")
  cat("Y:", dim_and_class(x$Y), "\n")
}
