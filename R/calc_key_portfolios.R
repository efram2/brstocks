#' Identify key portfolios on the efficient frontier
#'
#' Given the output of \code{\link{calc_efficient_frontier}}, extracts the
#' simulated portfolios with (1) minimum variance, (2) maximum Sharpe ratio,
#' and (3) maximum return -- the reference points typically highlighted on an
#' efficient frontier chart.
#'
#' @param fronteira A tibble returned by \code{\link{calc_efficient_frontier}}
#'   (must include a \code{pesos} list-column).
#'
#' @return A tibble with 3 rows and a \code{tipo} column identifying each:
#'   \code{"Minima Variancia"}, \code{"Maximo Sharpe"}, \code{"Maximo Retorno"}.
#'
#' @details
#' Because these points come from Monte Carlo simulation (not a closed-form
#' optimizer), they are the *best approximation found among the simulated
#' portfolios* -- not the true mathematical optimum. Increasing
#' \code{n_portfolios} in \code{\link{calc_efficient_frontier}} improves the
#' approximation. For an exact solution, an analytical quadratic-programming
#' approach (e.g. via the \code{quadprog} package) would be a natural next
#' step.
#'
#' @examples
#' \dontrun{
#' acoes <- get_stocks(c("PETR4", "VALE3", "ITUB4"))
#' fronteira <- calc_efficient_frontier(acoes)
#' calc_key_portfolios(fronteira)
#' }
#'
#' @export
calc_key_portfolios <- function(fronteira) {

  idx_min_var    <- which.min(fronteira$risco)
  idx_max_sharpe <- which.max(fronteira$sharpe)
  idx_max_ret    <- which.max(fronteira$retorno)

  resultado <- fronteira[c(idx_min_var, idx_max_sharpe, idx_max_ret), ]
  resultado$tipo <- c("Minima Variancia", "Maximo Sharpe", "Maximo Retorno")

  # Reordenar colunas sem usar relocate
  resultado <- resultado[, c("tipo", setdiff(names(resultado), "tipo"))]

  return(resultado)
}
