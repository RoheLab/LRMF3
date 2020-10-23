#' Description array size and type
#'
#' @param x Matrix or vector
#'
#' @return Description as character vector
#' @export
dim_and_class <- function(x) {
  if (is.vector(x))
    paste0(length(x), "      [", class(x)[1], "]")
  else
    # is a matrix
    paste0(nrow(x), " x ", ncol(x), " [", class(x)[1], "]")
}
