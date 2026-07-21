#' Calculate the correlation matrix of a set of stocks
#'
#' Computes the sample Pearson correlation matrix of log-adjusted returns for
#' all tickers in \code{stock_data}. All values are bounded between -1 and 1,
#' making it easier to interpret than the covariance matrix.
#'
#' \describe{
#'   \item{1}{Assets move perfectly together}
#'   \item{0}{No linear relationship}
#'   \item{-1}{Assets move in opposite directions}
#' }
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
#' calc_correlation_matrix(acoes)
#' }
#'
#' @export
calc_correlation_matrix <- function(stock_data, na_method = "intersection") {
  ret_matrix <- .prepare_returns_matrix(stock_data, na_method = na_method)
  stats::cor(ret_matrix)
}
