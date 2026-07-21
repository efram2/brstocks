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
