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
#' @examples
#' \dontrun{
#' acoes <- get_stocks(c("PETR4", "VALE3", "ITUB4", "BBDC4"))
#' fronteira <- calc_efficient_frontier(acoes, n_portfolios = 10000)
#' plot_efficient_frontier(fronteira)
#' }
#'
#' @export
plot_efficient_frontier <- function(efficient_frontier_data) {

  ggplot2::ggplot(efficient_frontier_data,
                  ggplot2::aes(x = risco, y = retorno, color = sharpe)) +
    ggplot2::geom_point(alpha = 0.5, size = 0.8) +
    ggplot2::scale_color_gradient(low = "#d62728", high = "#1f77b4") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title   = "Efficient Frontier",
      x       = "Risk (Standard Deviation)",
      y       = "Expected Return",
      color   = "Sharpe Ratio",
      caption = "Source: Yahoo Finance via yfR"
    )
}
