colSums_svd_like_impl <- function(U, d, V, num_threads) {
    RcppParallel::setThreadOptions(numThreads = num_threads)
    res <- colSums_svd_like_impl_cpp(U, d, V)
    RcppParallel::defaultNumThreads()
    return(res)
}

rowSums_svd_like_impl <- function(U, d, V, num_threads) {
    RcppParallel::setThreadOptions(numThreads = num_threads)
    res <- rowSums_svd_like_impl_cpp(U, d, V)
    RcppParallel::defaultNumThreads()
    return(res)
}