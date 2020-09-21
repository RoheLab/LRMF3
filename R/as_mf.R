
#' Coerce an object to LRMF class
#'
#' @param x Object to coerce
#' @param ... Ignored.
#'
#' @return
#' @export
#'
#' @rdname mf
as_mf <- function(x, ...) {
  UseMethod("as_mf")
}

#' @export
as_mf.list <- function(x, ...) {

  if (!(all(c("u", "d", "v") %in% names(x))))
    stop(
      "Cannot coerce lists without elements `u`, `d`, and `v`.",
      call. = FALSE
    )

  mf(x$u, x$d, x$v)
}
