#' Calculate the beta of one or more stocks
#'
#' Computes the beta coefficient for each ticker in \code{stock_data} relative
#' to a benchmark index. Beta measures how much an asset moves in relation to
#' the market: a beta greater than 1 indicates higher volatility than the
#' market, while a beta less than 1 indicates lower volatility.
#'
#' The formula used is:
#' \deqn{\beta = \frac{Cov(R_i, R_m)}{Var(R_m)}}
#'
#' Dates are aligned via an inner join, so only trading days present in both
#' datasets are used in the calculation.
#'
#' @param stock_data A tibble returned by \code{\link{get_stocks}}.
#' @param benchmark_data A tibble returned by \code{\link{get_benchmark}}.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{ticker}{Ticker identifier}
#'   \item{beta}{Beta coefficient relative to the benchmark}
#' }
#'
#' @examples
#' \dontrun{
#' acoes <- get_stocks(c("PETR4", "VALE3", "ITUB4"))
#' ibov  <- get_benchmark()
#' calc_beta(acoes, ibov)
#' }
#'
#' @export
calc_beta <- function(stock_data, benchmark_data) {

  benchmark_clean <- benchmark_data %>%
    dplyr::filter(!is.na(ret_adjusted_prices)) %>%
    dplyr::select(ref_date, ret_benchmark = ret_adjusted_prices)

  stock_data %>%
    dplyr::filter(!is.na(ret_adjusted_prices)) %>%
    dplyr::inner_join(benchmark_clean, by = "ref_date") %>%
    dplyr::group_by(ticker) %>%
    dplyr::summarise(
      beta = cov(ret_adjusted_prices, ret_benchmark) / var(ret_benchmark)
    )
}
