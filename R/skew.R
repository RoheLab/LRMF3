
# return +1 when skew positive, -1 when skew negative
skew_sign <- function(x) {
  sign(sum((x - mean(x))^3))
}

#' @export
make_skew_positive <- function(fa) {

  if (!inherits(fa, "fa_like"))
    stop("`make_skew_positive` is only intended for `fa_like` objects.")

  Z_column_skew_signs <- apply(fa$Z, 2, skew_sign)
  Y_column_skew_signs <- apply(fa$Y, 2, skew_sign)

  S_Z <- Diagonal(n = ncol(fa$Z), x = Z_column_skew_signs)
  S_Y <- Diagonal(n = ncol(fa$Y), x = Y_column_skew_signs)

  # note that S_Z and S_Y are their own inverses

  fa$Z <- fa$Z %*% S_Z
  fa$B <- S_Z %*% fa$B %*% S_Y
  fa$Y <- fa$Y %*% S_Y

  stopifnot(all(apply(fa$Z, 2, skew_sign) > 0))
  stopifnot(all(apply(fa$Y, 2, skew_sign) > 0))

  fa
}
