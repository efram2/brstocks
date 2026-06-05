#' Plot benchmark index performance
#'
#' Visualizes the historical performance of a market benchmark index, with an
#' optional moving average overlay. Supports two display modes: accumulated
#' return (index) or adjusted price.
#'
#' @param data A tibble returned by \code{\link{get_benchmark}}.
#' @param market Character. Used to set the chart title label.
#'   Use \code{"BR"} (default) for Ibovespa or \code{"US"} for S&P 500.
#' @param type Character. Display mode. Use \code{"index"} (default) to plot
#'   cumulative return (base = 1), or \code{"price"} to plot adjusted price.
#' @param ma_window Integer. Moving average window in trading days.
#'   Default is \code{200}. Set to a smaller value (e.g. \code{50}) for a
#'   shorter-term average.
#'
#' @return A \code{ggplot2} object.
#'
#' @examples
#' \dontrun{
#' ibov <- get_benchmark(from = "2020-01-01")
#' plot_benchmark(ibov)
#' plot_benchmark(ibov, type = "price", ma_window = 50)
#'
#' sp500 <- get_benchmark(market = "US", from = "2020-01-01")
#' plot_benchmark(sp500, market = "US")
#' }
#'
#' @export
plot_benchmark <- function(data,
                           market    = "BR",
                           type      = "index",
                           ma_window = 200) {

  # Select the y-axis column based on display type
  if (type == "index") {
    data$coluna_y <- data$cumret_adjusted_prices
    data$ma       <- zoo::rollmean(data$cumret_adjusted_prices,
                                   k = ma_window, fill = NA, align = "right")
  } else if (type == "price") {
    data$coluna_y <- data$price_adjusted
    data$ma       <- zoo::rollmean(data$price_adjusted,
                                   k = ma_window, fill = NA, align = "right")
  }

  market_label <- switch(market,
    "BR" = "Ibovespa",
    "US" = "S&P 500"
  )

  ggplot2::ggplot(data, ggplot2::aes(x = ref_date)) +
    ggplot2::geom_line(ggplot2::aes(y = coluna_y), color = "#1f77b4") +
    ggplot2::geom_line(ggplot2::aes(y = ma), color = "#d62728") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title    = paste0(market_label, " - ",
                        ifelse(type == "index", "Accumulated Return", "Adjusted Price")),
      subtitle = ifelse(type == "index",
                        "Base = 1 (first observation)", "In local currency"),
      x        = "Date",
      y        = ifelse(type == "index",
                        "Accumulated Return (base = 1)", "Adjusted Price"),
      caption  = "Source: Yahoo Finance via yfR"
    )
}
