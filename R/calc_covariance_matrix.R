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
#'
#' @return A named numeric matrix of dimensions \eqn{n \times n}, where
#'   \eqn{n} is the number of unique tickers.
#'
#' @examples
#' \dontrun{
#' acoes <- get_stocks(c("PETR4", "VALE3", "ITUB4"))
#' calc_covariance_matrix(acoes)
#' }
#'
#' @export
calc_covariance_matrix <- function(stock_data, na_method = "intersection") {
  ret_matrix <- .prepare_returns_matrix(stock_data, na_method = na_method)
  stats::cov(ret_matrix)
}
