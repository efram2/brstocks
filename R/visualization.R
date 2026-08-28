#' Plot the efficient frontier
#'
#' Visualizes the efficient frontier as a scatter plot of simulated portfolios,
#' colored by their Sharpe ratio. Each point represents a randomly generated
#' portfolio. The upper-left boundary of the point cloud is the efficient
#' frontier -- the set of portfolios with the highest return for each level
#' of risk.
#'
#' Portfolios with higher Sharpe ratios (blue) represent better risk-adjusted
#' returns. The portfolio with the highest Sharpe ratio is known as the
#' tangency portfolio.
#'
#' @param efficient_frontier_data A tibble returned by
#'   \code{\link{calc_efficient_frontier}}.
#'
#' @return A \code{ggplot2} object.
#'
#' @details
#' The subtitle reports the return frequency (\code{freq_data}) and whether
#' values are annualized, read from attributes attached by
#' \code{\link{calc_efficient_frontier}} -- since that choice materially
#' changes the scale of the axes, it's surfaced on the plot rather than left
#' implicit.
#'
#' @examples
#' \dontrun{
#' acoes <- get_stocks(c("PETR4", "VALE3", "ITUB4", "BBDC4"))
#' fronteira <- calc_efficient_frontier(acoes, n_portfolios = 10000)
#' plot_efficient_frontier(fronteira)
#' }
#'
#' @export
plot_efficient_frontier <- function(efficient_frontier_data) {

  freq_data  <- attr(efficient_frontier_data, "freq_data")
  annualized <- attr(efficient_frontier_data, "annualized")

  subtitulo <- if (!is.null(freq_data)) {
    freq_label <- c(daily = "daily", weekly = "weekly", monthly = "monthly")[[freq_data]]
    paste0("Based on ", freq_label, " returns",
           if (isTRUE(annualized)) " (annualized)" else " (not annualized)")
  } else {
    NULL
  }

  ggplot2::ggplot(efficient_frontier_data,
                  ggplot2::aes(x = risco, y = retorno, color = sharpe)) +
    ggplot2::geom_point(alpha = 0.5, size = 0.8) +
    ggplot2::scale_color_gradient(low = brstocks_cor_vermelho, high = brstocks_cor_azul) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title    = "Efficient Frontier",
      subtitle = subtitulo,
      x        = "Risk (Standard Deviation)",
      y        = "Expected Return",
      color    = "Sharpe Ratio",
      caption  = "Source: Yahoo Finance via yfR"
    )
}

#' Plot a heatmap of the correlation matrix
#'
#' Visualizes a pairwise correlation matrix of log-adjusted returns as a
#' color-coded heatmap. Correlation values are displayed inside each cell,
#' rounded to two decimal places.
#'
#' Colors follow the standard financial convention:
#' \itemize{
#'   \item Blue: high positive correlation
#'   \item White: no correlation
#'   \item Red: negative correlation
#' }
#'
#' @param x Either (a) a correlation matrix previously computed with
#'   \code{\link{calc_correlation_matrix}}, or (b) a raw \code{stock_data}
#'   tibble returned by \code{\link{get_stocks}}, containing two or more
#'   tickers. Passing a matrix you already computed avoids recalculating the
#'   correlation from scratch, and lets you reuse the same matrix in other
#'   calculations (e.g. as a sanity check alongside
#'   \code{\link{calc_covariance_matrix}}).
#'
#' @return A \code{ggplot2} object.
#'
#' @examples
#' \dontrun{
#' acoes <- get_stocks(c("PETR4", "VALE3", "ITUB4", "BBDC4"))
#'
#' # Recommended: compute once, reuse the variable
#' cor_mat <- calc_correlation_matrix(acoes)
#' plot_correlation_matrix(cor_mat)
#'
#' # Also supported: pass raw stock_data directly (computed internally)
#' plot_correlation_matrix(acoes)
#' }
#'
#' @export
plot_correlation_matrix <- function(x) {

  cor_matrix <- if (is.matrix(x)) x else calc_correlation_matrix(x)

  cor_long <- as.data.frame(cor_matrix) %>%
    tibble::rownames_to_column(var = "ativo1") %>%
    tidyr::pivot_longer(
      cols      = -ativo1,
      names_to  = "ativo2",
      values_to = "correlacao"
    )

  ggplot2::ggplot(cor_long, ggplot2::aes(x = ativo1, y = ativo2, fill = correlacao)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(
      low      = brstocks_cor_vermelho,
      mid      = "white",
      high     = brstocks_cor_azul,
      midpoint = 0,
      limits   = c(-1, 1)
    ) +
    ggplot2::geom_text(ggplot2::aes(label = round(correlacao, 2)), size = 3) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title   = "Correlation Matrix",
      x       = NULL,
      y       = NULL,
      fill    = "Correlation",
      caption = "Source: Yahoo Finance via yfR"
    )
}

#' Plot benchmark index performance
#'
#' Visualizes the historical performance of a market benchmark index, with an
#' optional moving average overlay. Supports two display modes: accumulated
#' return (index) or adjusted price.
#'
#' @param data A tibble returned by \code{\link{get_benchmark}}.
#' @param market Character. Optional override for the chart title label. By
#'   default (\code{NULL}) the label is read from the \code{"benchmark_nome"}
#'   attribute that \code{\link{get_benchmark}} attaches to its output, so
#'   any of its supported benchmarks (not just \code{"BR"}/\code{"US"}) label
#'   correctly without needing this argument. Pass a value here only to
#'   override that label, or when plotting data that didn't come from
#'   \code{\link{get_benchmark}}.
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
#' plot_benchmark(sp500)
#'
#' # NASDAQ and other newer benchmarks label correctly too, with no extra work
#' nasdaq <- get_benchmark(market = "NASDAQ", from = "2020-01-01")
#' plot_benchmark(nasdaq)
#' }
#'
#' @export
plot_benchmark <- function(data,
                           market    = NULL,
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

  # Prefer an explicit `market` override; otherwise fall back to the
  # "benchmark_nome" attribute get_benchmark() attaches. This is what fixes
  # the label for any benchmark beyond the original "BR"/"US" pair -- the
  # old hardcoded switch() silently produced NA/NULL titles for anything
  # else.
  market_label <- if (!is.null(market)) {
    switch(market,
          "BR" = "Ibovespa",
          "US" = "S&P 500",
          market)
  } else {
    attr(data, "benchmark_nome")
  }
  if (is.null(market_label)) market_label <- "Benchmark"

  ggplot2::ggplot(data, ggplot2::aes(x = ref_date)) +
    ggplot2::geom_line(ggplot2::aes(y = coluna_y), color = brstocks_cor_azul) +
    ggplot2::geom_line(ggplot2::aes(y = ma), color = brstocks_cor_vermelho) +
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
