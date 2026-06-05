#' Calculate the return variance of one or more stocks
#'
#' Computes the sample variance of log-adjusted returns for each ticker.
#' Variance is the primary measure of individual asset risk in portfolio theory.
#'
#' @param stock_data A tibble returned by \code{\link{get_stocks}}.
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
calc_variance <- function(stock_data) {

  stock_data %>%
    dplyr::filter(!is.na(ret_adjusted_prices)) %>%
    dplyr::group_by(ticker) %>%
    dplyr::summarise(
      variance = var(ret_adjusted_prices)
    )
}
