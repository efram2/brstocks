#' Calculate the expected return of one or more stocks
#'
#' Estimates the expected return for each ticker as the historical mean of
#' log-adjusted returns. This is the maximum likelihood estimator commonly
#' used in quantitative finance and portfolio theory.
#'
#' @param stock_data A tibble returned by \code{\link{get_stocks}}.
#' @param na_method Character. How to handle missing observations.
#'   One of "intersection" (default), "pairwise", or "locf".
#'   See \code{.prepare_returns_matrix} for details.
#' @param freq_data Character. Frequency at which returns are aggregated
#'   before averaging. One of "daily", "weekly", or \code{"monthly"}
#'   (default). Matches the default of \code{\link{calc_efficient_frontier}}
#'   so that composing these functions manually (see examples) produces
#'   consistent results out of the box.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{ticker}{Ticker identifier}
#'   \item{expected_return}{Mean log return for the period}
#' }
#'
#' @details
#' Returns are computed at the frequency given by \code{freq_data}. To
#' annualize, multiply by the number of periods per year (252 for daily,
#' 52 for weekly, 12 for monthly). If you pass the result into
#' \code{\link{calc_efficient_frontier}} via its \code{expected_returns}
#' argument, make sure to use the same \code{freq_data} in both calls --
#' otherwise annualization will be inconsistent (a warning is raised in
#' that case).
#'
#' @examples
#' \dontrun{
#' acoes <- get_stocks(c("PETR4", "VALE3"))
#' calc_expected_return(acoes)
#' }
#'
#' @export
calc_expected_return <- function(stock_data,
                                 na_method = "intersection",
                                 freq_data = "monthly") {

  freq_data <- match.arg(freq_data, c("daily", "weekly", "monthly"))

  ret_matrix <- .prepare_returns_matrix(stock_data, na_method = na_method)

  if (freq_data != "daily") {
    ret_matrix <- .aggregate_returns(ret_matrix, freq = freq_data,
                                     dates = attr(ret_matrix, "dates"))
  }

  resultado <- data.frame(
    ticker = colnames(ret_matrix),
    expected_return = colMeans(ret_matrix, na.rm = TRUE)
  )
  attr(resultado, "freq_data") <- freq_data
  resultado
}
