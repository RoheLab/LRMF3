library(testthat)

skip_if_not_installed("Matrix")
skip_if_not_installed("RSpectra")

library(Matrix)
library(RSpectra)

test_that("rowSums svd_like method, 1 thread(s)", {

  set.seed(27)

  # matrix to build an SVD from
  A <- rsparsematrix(30, 40, nnz = 50)
  s <- svds(A, 5)

  rs_impl <- as.numeric(rowSums_svd_like_impl(s$u, s$d, s$v, 1))

  Z <- s$u %*% diag(s$d) %*% t(s$v)

  expect_equal(rs_impl, rowSums(Z))

  mf <- as_svd_like(s)

  expect_equal(rowSums(mf), rowSums(Z))
})

test_that("rowSums svd_like method, 2 thread(s)", {

  set.seed(27)

  # matrix to build an SVD from
  A <- rsparsematrix(30, 40, nnz = 50)
  s <- svds(A, 5)

  rs_impl <- as.numeric(rowSums_svd_like_impl(s$u, s$d, s$v, 2))

  Z <- s$u %*% diag(s$d) %*% t(s$v)

  expect_equal(rs_impl, rowSums(Z))

  mf <- as_svd_like(s)

  expect_equal(rowSums(mf), rowSums(Z))
})

