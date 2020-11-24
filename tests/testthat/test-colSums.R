library(testthat)

skip_if_not_installed("Matrix")
skip_if_not_installed("RSpectra")

library(Matrix)
library(RSpectra)

test_that("colSums svd_like method, 1 thread(s)", {

  set.seed(27)

  # matrix to build an SVD from
  A <- rsparsematrix(30, 40, nnz = 50)
  s <- svds(A, 5)

  cs_impl <- as.numeric(colSums_svd_like_impl(s$u, s$d, s$v, 1))

  Z <- s$u %*% diag(s$d) %*% t(s$v)

  expect_equal(cs_impl, colSums(Z))

  mf <- as_svd_like(s)

  expect_equal(colSums(mf), colSums(Z))
})

test_that("colSums svd_like method, 2 thread(s)", {

  set.seed(27)

  # matrix to build an SVD from
  A <- rsparsematrix(30, 40, nnz = 50)
  s <- svds(A, 5)

  cs_impl <- as.numeric(colSums_svd_like_impl(s$u, s$d, s$v, 2))

  Z <- s$u %*% diag(s$d) %*% t(s$v)

  expect_equal(cs_impl, colSums(Z))

  mf <- as_svd_like(s)

  expect_equal(colSums(mf), colSums(Z))
})

