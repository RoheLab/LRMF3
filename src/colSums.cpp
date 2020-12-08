// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;
using namespace Rcpp;
using namespace arma;
using namespace std;

// use RcppParallel for parallel

struct colSum_worker : public Worker
{
  const arma::mat &UD;
  const arma::mat &V;
  const int &m;
  arma::vec res; // output

  colSum_worker(const arma::mat &UD,
                const arma::mat &V,
                const int &m) : UD(UD), V(V), m(m),
                                res(arma::vec(m, fill::zeros)) {}

  void operator()(std::size_t begin, std::size_t end)
  {
    for (int i = begin; i < end; ++i)
    {
      res(i) = arma::accu(UD * V.row(i).t());
    }
  }
};

// [[Rcpp::export]]
arma::vec colSums_svd_like_impl_cpp(
    const arma::mat &U,
    const arma::rowvec &d,
    const arma::mat &V )
{

  int m = V.n_rows;
  arma::mat UD = U * arma::diagmat(d);

  colSum_worker hardworking(UD, V, m);
  parallelFor(0, m, hardworking);
  return (hardworking.res);
}
