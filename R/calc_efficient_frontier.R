#' Simulate portfolios to build the efficient frontier
#'
#' Uses Monte Carlo simulation to generate a large number of random portfolios
#' and compute their expected return, risk, and Sharpe ratio. The resulting
#' dataset can be plotted with \code{\link{plot_efficient_frontier}} to
#' visualize the efficient frontier.
#'
#' @param stock_data A tibble returned by \code{\link{get_stocks}}, containing
#'   two or more tickers.
#' @param n_portfolios Integer. Number of random portfolios to simulate.
#'   Default is \code{10000}. Higher values produce smoother frontiers.
#'
#' @return A tibble with \code{n_portfolios} rows and the following columns:
#' \describe{
#'   \item{retorno}{Expected return of the portfolio}
#'   \item{risco}{Portfolio risk (standard deviation)}
#'   \item{sharpe}{Sharpe ratio (return / risk)}
#' }
#'
#' @details
#' Portfolio weights are drawn from a uniform distribution and normalized to
#' sum to 1, ensuring full investment with no short-selling. Portfolio risk is
#' computed as:
#' \deqn{\sigma_p = \sqrt{w^T \Sigma w}}
#' where \eqn{w} is the weight vector and \eqn{\Sigma} is the covariance matrix.
#'
#' @examples
#' \dontrun{
#' acoes <- get_stocks(c("PETR4", "VALE3", "ITUB4", "BBDC4"))
#' fronteira <- calc_efficient_frontier(acoes, n_portfolios = 10000)
#' plot_efficient_frontier(fronteira)
#' }
#'
#' @export
calc_efficient_frontier <- function(stock_data, n_portfolios = 10000) {

  # Step 1: compute expected returns and covariance matrix
  retornos <- calc_expected_return(stock_data)
  cov_mat  <- calc_covariance_matrix(stock_data)

  n_ativos <- length(retornos$ticker)

  # Step 2: simulate n_portfolios random portfolios
  resultados <- purrr::map_dfr(1:n_portfolios, function(i) {

    # Random weights that sum to 1 (no short-selling)
    pesos <- runif(n_ativos)
    pesos <- pesos / sum(pesos)

    # Portfolio return, risk and Sharpe ratio
    retorno <- sum(pesos * retornos$expected_return)
    risco   <- sqrt(t(pesos) %*% as.matrix(cov_mat) %*% pesos)
    sharpe  <- retorno / risco

    dplyr::tibble(
      retorno = retorno,
      risco   = as.numeric(risco),
      sharpe  = as.numeric(sharpe)
    )
  })

  return(resultados)
}
