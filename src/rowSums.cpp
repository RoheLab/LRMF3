// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;
using namespace Rcpp;
using namespace arma;
using namespace std;

// use RcppParallel for parallel

struct rowSum_worker : public Worker
{
  const arma::mat &U;
  const arma::mat &DVt;
  const int &n;
  arma::vec res; // output

  rowSum_worker(const arma::mat &U,
                const arma::mat &DVt,
                const int &n) : U(U), DVt(DVt), n(n),
                                res(arma::vec(n, fill::zeros)) {}

  void operator()(std::size_t begin, std::size_t end)
  {
    for (int i = begin; i < end; ++i)
    {
      res(i) = arma::accu(U.row(i) * DVt);
    }
  }
};

// [[Rcpp::export]]
arma::vec rowSums_svd_like_impl_cpp(
    const arma::mat &U,
    const arma::rowvec &d,
    const arma::mat &V )
{

  int n = U.n_rows;
  arma::mat DVt = arma::diagmat(d) * V.t();

  rowSum_worker hardworking(U, DVt, n);
  parallelFor(0, n, hardworking);
  return (hardworking.res);
}
