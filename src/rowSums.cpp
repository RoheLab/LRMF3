#include <RcppArmadillo.h>

#include <omp.h>
// [[Rcpp::plugins(openmp)]]

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec rowSums_svd_like_impl(
    const arma::mat& U,
    const arma::rowvec& d,
    const arma::mat& V,
    const int num_threads) {

  int n = U.n_rows;
  arma::mat DVt = arma::diagmat(d) * V.t();

  arma::vec rs = arma::zeros<arma::vec>(n);

  omp_set_num_threads(num_threads);

  #pragma omp parallel for shared(U)
  for (int i = 0; i < n; i++) {
    rs(i) = arma::accu(U.row(i) * DVt);
  }

  return rs;
}
