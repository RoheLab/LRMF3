#include <RcppArmadillo.h>

#include <omp.h>
// [[Rcpp::plugins(openmp)]]

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec colSums_svd_like_impl(
    const arma::mat& U,
    const arma::rowvec& d,
    const arma::mat& V,
    const int num_threads) {

  int m = V.n_rows;
  arma::mat UD = U * arma::diagmat(d);

  arma::vec cs = arma::zeros<arma::vec>(m);

  omp_set_num_threads(num_threads);

  #pragma omp parallel for shared(U)
  for (int j = 0; j < m; j++) {
    cs(j) = arma::accu(UD * V.row(j).t());
  }

  return cs;
}
