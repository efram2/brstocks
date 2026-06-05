#' Get historical benchmark index data
#'
#' Retrieves historical data for a market benchmark index. Supports the
#' Brazilian market (Ibovespa) and the US market (S&P 500). Internally
#' calls \code{\link{get_stocks}} with \code{add_sa = FALSE}.
#'
#' @param market Character. Market to retrieve the benchmark for.
#'   Use \code{"BR"} (default) for Ibovespa (\code{^BVSP}) or
#'   \code{"US"} for S&P 500 (\code{^GSPC}).
#' @param from Start date. Accepts \code{Date} or character in \code{"YYYY-MM-DD"}
#'   format. Defaults to 365 days before today.
#' @param to End date. Accepts \code{Date} or character in \code{"YYYY-MM-DD"}
#'   format. Defaults to today.
#' @param freq Data frequency. One of \code{"daily"} (default), \code{"weekly"},
#'   \code{"monthly"}, or \code{"yearly"}.
#'
#' @return A tibble with the same structure as \code{\link{get_stocks}}.
#'
#' @examples
#' \dontrun{
#' get_benchmark()
#' get_benchmark(market = "US", from = "2020-01-01")
#' get_benchmark(market = "BR", freq = "monthly")
#' }
#'
#' @export
get_benchmark <- function(market = "BR",
                          from   = NULL,
                          to     = NULL,
                          freq   = "daily") {

  ticker <- switch(market,
    "BR" = "^BVSP",
    "US" = "^GSPC",
    stop("Unsupported market. Use 'BR' or 'US'.")
  )

  get_stocks(ticker,
             from   = from,
             to     = to,
             freq   = freq,
             add_sa = FALSE)
}
