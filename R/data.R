#' Get historical stock data from Yahoo Finance
#'
#' Downloads historical OHLCV price data and log returns for one or more
#' financial assets using Yahoo Finance as the data source via the
#' \pkg{yfR} package.
#'
#' The function automatically attempts to resolve ticker symbols. It first
#' searches for the ticker exactly as provided (e.g. \code{"GOLD11"}). If no
#' data are found, it retries using the \code{".SA"} suffix
#' (e.g. \code{"PETR4"} \eqn{\rightarrow} \code{"PETR4.SA"}). If neither
#' version is available, a warning is issued and the ticker is skipped.
#' Tickers already prefixed with \code{"^"} (indices, e.g. \code{"^BVSP"})
#' skip the \code{.SA} fallback entirely, since that suffix never applies to
#' index tickers.
#'
#' Date inputs are flexible and may be supplied as complete dates
#' (\code{"YYYY-MM-DD"}), year-month (\code{"YYYY-MM"}) or year only
#' (\code{"YYYY"}). Missing dates are automatically replaced by sensible
#' defaults.
#'
#' @param tickers Character vector of ticker symbols (e.g.
#'   \code{"PETR4"}, \code{"VALE3"}, \code{"BOVA11"},
#'   \code{"GOLD11"}, \code{"IVVB11"}).
#'
#' @param from Start date. Accepts \code{NULL}, \code{"YYYY"},
#'   \code{"YYYY-MM"} or \code{"YYYY-MM-DD"}.
#'
#'   If \code{NULL} (default), the download starts 365 days before the
#'   current date.
#'
#' @param to End date. Accepts \code{NULL}, \code{"YYYY"},
#'   \code{"YYYY-MM"} or \code{"YYYY-MM-DD"}.
#'
#'   If \code{NULL} (default), the current date is used.
#'
#' @param freq Data frequency. One of
#'   \code{"daily"} (default),
#'   \code{"weekly"},
#'   \code{"monthly"},
#'   or \code{"yearly"}.
#'
#' @details
#' Yahoo Finance coverage varies across securities and markets.
#' Some assets may have shorter trading histories or different trading
#' calendars due to local holidays. When a ticker cannot be retrieved,
#' the function issues a warning but continues downloading the remaining
#' requested assets.
#'
#' @return
#' A tibble with one row per asset and trading date containing:
#'
#' \describe{
#'   \item{ticker}{Ticker identifier.}
#'   \item{ref_date}{Trading date.}
#'   \item{price_open}{Opening price.}
#'   \item{price_high}{Highest price during the period.}
#'   \item{price_low}{Lowest price during the period.}
#'   \item{price_close}{Closing price.}
#'   \item{volume}{Trading volume.}
#'   \item{price_adjusted}{Adjusted closing price.}
#'   \item{ret_adjusted_prices}{Log return based on adjusted prices.}
#'   \item{ret_closing_prices}{Log return based on closing prices.}
#'   \item{cumret_adjusted_prices}{Cumulative return (base = 1).}
#' }
#'
#' The returned tibble also carries a \code{"download_report"} attribute --
#' a data frame with one row per requested ticker, its resolved Yahoo symbol
#' (or \code{NA} if not found), and a status column -- so callers can inspect
#' which tickers failed without having to parse warnings.
#'
#' @examples
#' \dontrun{
#'
#' # Brazilian stocks
#' get_stocks(c("PETR4", "VALE3"))
#'
#' # ETFs traded on B3
#' get_stocks(c("BOVA11", "IVVB11", "GOLD11"))
#'
#' # Custom date range
#' get_stocks(
#'   c("PETR4", "BOVA11"),
#'   from = "2020",
#'   to = "2024"
#' )
#'
#' # Monthly data
#' get_stocks(
#'   "IVVB11",
#'   from = "2021-01",
#'   freq = "monthly"
#' )
#'
#' # Default behaviour (last 365 days)
#' get_stocks("PETR4")
#'
#' }
#'
#' @export
get_stocks <- function(tickers,
                       from = NULL,
                       to = NULL,
                       freq = "daily") {

  # Normalize dates
  from <- if (is.null(from)) {
    Sys.Date() - 365
  } else {
    .normalize_date(from, is_start = TRUE)
  }

  to <- if (is.null(to)) {
    Sys.Date()
  } else {
    .normalize_date(to, is_start = FALSE)
  }

  dados <- list()
  relatorio <- list()

  for (ticker in tickers) {

    candidatos <- c(
      ticker,
      if (!grepl("\\.SA$", ticker) && !grepl("^\\^", ticker))
        paste0(ticker, ".SA")
    )

    candidatos <- unique(candidatos)

    download <- NULL
    ticker_utilizado <- NA_character_

    for (cand in candidatos) {

      download <- tryCatch(

        yfR::yf_get(
          tickers = cand,
          first_date = from,
          last_date = to,
          freq_data = freq,
          type_return = "log",
          thresh_bad_data = 0,
          be_quiet = TRUE
        ),

        error = function(e) NULL

      )

      if (!is.null(download) && nrow(download) > 0) {

        ticker_utilizado <- cand
        break

      }

    }

    if (is.null(download) || nrow(download) == 0) {

      warning(
        sprintf(
          "Ticker '%s' could not be retrieved from Yahoo Finance.",
          ticker
        ),
        call. = FALSE
      )

      relatorio[[length(relatorio) + 1]] <- data.frame(
        requested = ticker,
        resolved = NA_character_,
        status = "not_found"
      )

      next

    }

    download$ticker <- ticker

    dados[[length(dados) + 1]] <- download

    relatorio[[length(relatorio) + 1]] <- data.frame(
      requested = ticker,
      resolved = ticker_utilizado,
      status = "ok"
    )

  }

  if (length(dados) == 0) {

    stop(
      "None of the requested tickers could be retrieved from Yahoo Finance.",
      call. = FALSE
    )

  }

  dados <- dplyr::bind_rows(dados)

  attr(dados, "download_report") <- dplyr::bind_rows(relatorio)

  dados

}

# ---------------------------------------------------------------------
# Supported benchmarks: short code -> Yahoo ticker + display name.
# Kept as a named list (not a bare switch()) so adding a new benchmark is a
# one-line change here, and so plot_benchmark() can read the display name
# back from an attribute instead of re-deriving it from the code.
# ---------------------------------------------------------------------
.brstocks_benchmark_map <- list(
  "BR"     = list(ticker = "^BVSP",  nome = "Ibovespa"),
  "US"     = list(ticker = "^GSPC",  nome = "S&P 500"),
  "NASDAQ" = list(ticker = "^IXIC",  nome = "Nasdaq Composite"),
  "DOW"    = list(ticker = "^DJI",   nome = "Dow Jones Industrial Average"),
  "SMALL"  = list(ticker = "SMAL11", nome = "ETF Small Caps Brasil (SMAL11)"),
  "WORLD"  = list(ticker = "URTH",   nome = "MSCI World (ETF URTH)")
)

#' Get historical benchmark index data
#'
#' Retrieves historical data for a market benchmark index or reference ETF.
#' Internally calls \code{\link{get_stocks}}.
#'
#' @param market Character. Benchmark to retrieve. One of \code{"BR"}
#'   (default, Ibovespa), \code{"US"} (S&P 500), \code{"NASDAQ"} (Nasdaq
#'   Composite), \code{"DOW"} (Dow Jones), \code{"SMALL"} (Brazilian small
#'   caps, via the SMAL11 ETF), or \code{"WORLD"} (MSCI World, via the URTH
#'   ETF).
#' @param from Start date. Accepts \code{Date} or character in \code{"YYYY-MM-DD"}
#'   format. Defaults to 365 days before today.
#' @param to End date. Accepts \code{Date} or character in \code{"YYYY-MM-DD"}
#'   format. Defaults to today.
#' @param freq Data frequency. One of \code{"daily"} (default), \code{"weekly"},
#'   \code{"monthly"}, or \code{"yearly"}.
#'
#' @return A tibble with the same structure as \code{\link{get_stocks}}. The
#'   chosen benchmark's short code and display name are attached as the
#'   \code{"benchmark_market"} and \code{"benchmark_nome"} attributes, so
#'   downstream functions like \code{\link{plot_benchmark}} can label plots
#'   correctly without re-implementing this mapping.
#'
#' @examples
#' \dontrun{
#' get_benchmark()
#' get_benchmark(market = "US", from = "2020-01-01")
#' get_benchmark(market = "SMALL", freq = "monthly")
#' }
#'
#' @export
get_benchmark <- function(market = "BR",
                          from   = NULL,
                          to     = NULL,
                          freq   = "daily") {

  if (!market %in% names(.brstocks_benchmark_map)) {
    stop(
      "Unsupported 'market'. Use one of: ",
      paste(names(.brstocks_benchmark_map), collapse = ", "),
      call. = FALSE
    )
  }

  info <- .brstocks_benchmark_map[[market]]

  dados <- get_stocks(info$ticker, from = from, to = to, freq = freq)

  attr(dados, "benchmark_market") <- market
  attr(dados, "benchmark_nome")   <- info$nome

  dados
}
