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
