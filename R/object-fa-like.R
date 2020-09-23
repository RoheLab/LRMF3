#' Create a low rank matrix factorization object
#'
#' A low rank matrix factorization of a matrix `X` is
#' parameterized by `X ~= Z %*% B %*% t(Y)`.
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
#' # TODO
#'
#' @export
fa <- function(Z, B, Y, subclasses = NULL, ...) {

  all_dims <- c(dims(Z), dims(B), dims(Y))

  fa <- new_fa(
    Z = Z,
    B = B,
    Y = Y,
    rank = as.integer(min(all_dims)),
    subclasses = subclasses,
    ...
  )

  validate_fa(fa)
}


#' @export
new_fa <- function(Z, B, Y, rank, subclasses = NULL, ...) {

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

validate_fa <- function(fa) {

  # ### object type validation
  #
  # if (!(inherits(fa$u, "matrix") || inherits(fa$u, "Matrix")))
  #   stop("`u` must be a matrix or Matrix object.", call. = FALSE)
  #
  # # d needs to be a numeric vector
  #
  # if (!is.numeric(fa$d) || !is.vector(fa$d))
  #   stop("`d` must be a numeric vector.", call. = FALSE)
  #
  # if (!(inherits(fa$v, "matrix") || inherits(fa$v, "Matrix")))
  #   stop("`v` must be a matrix or Matrix object.", call. = FALSE)
  #
  # if (!is.integer(fa$rank))
  #   stop("`rank` must be an integer.", call. = FALSE)
  #
  # ### dimension validation
  #
  # if (ncol(fa$u) != length(fa$d))
  #   stop("Dimensions of `u` and `d` must match.", call. = FALSE)
  #
  # if (length(fa$d) != ncol(fa$v))
  #   stop("Dimensions of `d` and `v` must match.", call. = FALSE)
  #
  # if (length(fa$d) != fa$rank)
  #   stop("`rank` does not match `u`, `d`, and `v`.", call. = FALSE)

  fa
}

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
