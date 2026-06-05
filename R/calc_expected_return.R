#' Calculate the expected return of one or more stocks
#'
#' Estimates the expected return for each ticker as the historical mean of
#' log-adjusted returns. This is the maximum likelihood estimator commonly
#' used in quantitative finance and portfolio theory.
#'
#' @param stock_data A tibble returned by \code{\link{get_stocks}}.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{ticker}{Ticker identifier}
#'   \item{expected_return}{Mean log return for the period}
#' }
#'
#' @details
#' Returns are computed at the frequency of the input data (daily by default).
#' To annualize, multiply by the number of trading periods per year
#' (252 for daily, 52 for weekly, 12 for monthly).
#'
#' @examples
#' \dontrun{
#' acoes <- get_stocks(c("PETR4", "VALE3"))
#' calc_expected_return(acoes)
#' }
#'
#' @export
calc_expected_return <- function(stock_data) {

  stock_data %>%
    dplyr::filter(!is.na(ret_adjusted_prices)) %>%
    dplyr::group_by(ticker) %>%
    dplyr::summarise(
      expected_return = mean(ret_adjusted_prices)
    )
}
