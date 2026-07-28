#' Calculate the covariance matrix of a set of stocks
#'
#' Computes the sample covariance matrix of log-adjusted returns for all
#' tickers in \code{stock_data}. The diagonal contains each asset's variance,
#' while off-diagonal elements represent pairwise covariances.
#'
#' The covariance matrix is a required input for portfolio optimization
#' under Modern Portfolio Theory (Markowitz, 1952).
#'
#' @param stock_data A tibble returned by \code{\link{get_stocks}}, containing
#'   one or more tickers.
#' @param na_method Character. How to handle missing observations.
#'   One of "intersection" (default), "pairwise", or "locf".
#'   See \code{.prepare_returns_matrix} for details.
#' @param freq_data Character. Frequency at which returns are aggregated
#'   before computing covariance. One of "daily", "weekly", or
#'   \code{"monthly"} (default). Matches the default of
#'   \code{\link{calc_efficient_frontier}} so that composing these functions
#'   manually (see examples) produces consistent results out of the box.
#'
#' @return A named numeric matrix of dimensions \eqn{n \times n}, where
#'   \eqn{n} is the number of unique tickers.
#'
#' @details
#' If you pass the result into \code{\link{calc_efficient_frontier}} via its
#' \code{cov_matrix} argument, make sure to use the same \code{freq_data} in
#' both calls -- otherwise annualization will be inconsistent (a warning is
#' raised in that case).
#'
#' When \code{na_method = "pairwise"}, each cell of the matrix is computed
#' from the dates common to that specific pair of assets, which can use more
#' of each asset's history than \code{"intersection"}. Be aware this does not
#' guarantee the resulting matrix is positive semi-definite, since different
#' cells may be estimated from different subsets of dates -- this can lead to
#' an invalid covariance structure and unstable optimization results
#' downstream. Inspect the matrix (e.g. \code{eigen(result)$values}) before
#' relying on it, or prefer \code{"intersection"} if in doubt.
#'
#' @examples
#' \dontrun{
#' acoes <- get_stocks(c("PETR4", "VALE3", "ITUB4"))
#' calc_covariance_matrix(acoes)
#' }
#'
#' @export
calc_covariance_matrix <- function(stock_data, na_method = "intersection",
                                   freq_data = "monthly") {
  freq_data <- match.arg(freq_data, c("daily", "weekly", "monthly"))

  ret_matrix <- .prepare_returns_matrix(stock_data, na_method = na_method)
  if (freq_data != "daily") {
    ret_matrix <- .aggregate_returns(ret_matrix, freq = freq_data,
                                     dates = attr(ret_matrix, "dates"))
  }

  # na_method = "pairwise" deliberately leaves NAs in ret_matrix (see
  # .prepare_returns_matrix()); stats::cov()'s default use = "everything"
  # does NOT do pairwise deletion on its own -- without this, any leftover
  # NA silently propagates to the entire matrix.
  use_arg <- switch(na_method,
                    intersection = "everything",
                    pairwise     = "pairwise.complete.obs",
                    locf         = "complete.obs"  # drops any row LOCF couldn't fill (e.g. leading NAs)
  )

  resultado <- stats::cov(ret_matrix, use = use_arg)
  attr(resultado, "freq_data") <- freq_data
  resultado
}
