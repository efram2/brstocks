#' Get historical stock data from Yahoo Finance
#'
#' Retrieves historical OHLCV price data and log returns for one or more
#' stocks using Yahoo Finance as the data source, via the yfR package.
#'
#' @param tickers Character vector of stock tickers (e.g. \code{"PETR4"} or
#'   \code{c("PETR4", "VALE3")}).
#' @param from Start date. Accepts \code{Date} or character in \code{"YYYY-MM-DD"}
#'   format. Defaults to 365 days before today.
#' @param to End date. Accepts \code{Date} or character in \code{"YYYY-MM-DD"}
#'   format. Defaults to today.
#' @param freq Data frequency. One of \code{"daily"} (default), \code{"weekly"},
#'   \code{"monthly"}, or \code{"yearly"}.
#' @param add_sa Logical. If \code{TRUE} (default), automatically appends the
#'   \code{".SA"} suffix required for B3-listed tickers. Tickers that already
#'   end in \code{".SA"} or start with \code{"^"} (indices) are left unchanged.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{ticker}{Ticker identifier}
#'   \item{ref_date}{Reference date}
#'   \item{price_open}{Opening price}
#'   \item{price_high}{Highest price of the period}
#'   \item{price_low}{Lowest price of the period}
#'   \item{price_close}{Closing price}
#'   \item{volume}{Trading volume}
#'   \item{price_adjusted}{Adjusted price (splits and dividends)}
#'   \item{ret_adjusted_prices}{Log return of adjusted prices}
#'   \item{ret_closing_prices}{Log return of closing prices}
#'   \item{cumret_adjusted_prices}{Cumulative return (base = 1)}
#' }
#'
#' @examples
#' \dontrun{
#' get_stocks("PETR4")
#' get_stocks(c("PETR4", "VALE3"), from = "2020-01-01", to = "2024-01-01")
#' get_stocks("PETR4", freq = "monthly")
#' }
#'
#' @export
get_stocks <- function(tickers,
                       from   = NULL,
                       to     = NULL,
                       freq   = "daily",
                       add_sa = TRUE) {

  # Appends ".SA" suffix for B3 tickers (e.g. "PETR4" -> "PETR4.SA")
  # Skips tickers that already have ".SA" or start with "^" (indices)
  if (add_sa) {
    tickers <- ifelse(
      grepl("\\.SA$", tickers) | grepl("^\\^", tickers),
      tickers,
      paste0(tickers, ".SA")
    )
  }

  # Default date range: last 365 days
  if (is.null(from)) from <- Sys.Date() - 365
  if (is.null(to))   to   <- Sys.Date()

  # Fetch data via yfR, which internally uses quantmod::getSymbols
  # type_return = "log": log returns are standard in quantitative finance
  yfR::yf_get(
    tickers     = tickers,
    first_date  = from,
    last_date   = to,
    freq_data   = freq,
    type_return = "log",
    be_quiet    = TRUE
  )
}
