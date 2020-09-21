#' @export
new_LRMF <- function(
  u, d, v, rank, alpha, subclasses = NULL) {

  object <- list(
    u = u,
    d = d,
    v = v,
    rank = rank,
    alpha =  alpha
  )

  class(object) <- c(subclasses, "LRMF")
  object
}

#' @export
print.LRMF <- function(x, ...) {
  cat("Low Rank Matrix Factorization\n\n")


  cat(glue("Rank: {x$rank}"), sep = "\n\n")

  cat(glue("Rows: {nrow(x$u)}"), sep = "\n")
  cat(glue("Cols: {nrow(x$v)}"), sep = "\n\n")

  cat(glue("d[rank]: {x$d[x$rank]}"), sep = "\n")
  cat(glue("alpha:   {x$alpha}"), sep = "\n")

  cat("\nPre-Processing\n\n")

  cat(" - Centered: FALSE \n")
  cat(" - Scaled:   FALSE \n\n")

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
