#' Calculate the return variance of one or more stocks
#'
#' Computes the sample variance of log-adjusted returns for each ticker.
#' Variance is the primary measure of individual asset risk in portfolio theory.
#'
#' @param stock_data A tibble returned by \code{\link{get_stocks}}.
#' @param na_method Character. How to handle missing observations.
#'   One of "intersection" (default), "pairwise", or "locf".
#'   See \code{.prepare_returns_matrix} for details.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{ticker}{Ticker identifier}
#'   \item{variance}{Sample variance of log returns for the period}
#' }
#'
#' @details
#' Variance is computed at the frequency of the input data (daily by default).
#' To annualize, multiply by the number of trading periods per year
#' (252 for daily, 52 for weekly, 12 for monthly).
#'
#' @examples
#' \dontrun{
#' acoes <- get_stocks(c("PETR4", "VALE3"))
#' calc_variance(acoes)
#' }
#'
#' @export
calc_variance <- function(stock_data, na_method = "intersection") {
  ret_matrix <- .prepare_returns_matrix(stock_data, na_method = na_method)

  data.frame(
    ticker = colnames(ret_matrix),
    variance = apply(ret_matrix, 2, stats::var, na.rm = TRUE)
  )
}
