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
calc_correlation_matrix <- function(stock_data) {

  retornos <- stock_data %>%
    dplyr::filter(!is.na(ret_adjusted_prices)) %>%
    dplyr::select(ticker, ref_date, ret_adjusted_prices) %>%
    tidyr::pivot_wider(
      names_from  = ticker,
      values_from = ret_adjusted_prices
    ) %>%
    dplyr::select(-ref_date)

  cor(retornos)
}
