#' Simulate portfolios to build the efficient frontier
#'
#' Uses Monte Carlo simulation to generate a large number of random portfolios
#' and compute their expected return, risk, Sharpe ratio, and asset weights.
#' The resulting dataset can be plotted with \code{\link{plot_efficient_frontier}}
#' to visualize the efficient frontier, or used to recover the weights of any
#' simulated portfolio (e.g. the one with the highest Sharpe ratio).
#'
#' @param stock_data A tibble returned by \code{\link{get_stocks}}, containing
#'   two or more tickers. Can be omitted if both \code{expected_returns} and
#'   \code{cov_matrix} are supplied directly.
#' @param n_portfolios Integer. Number of random portfolios to simulate.
#'   Default is \code{10000}. Higher values produce smoother frontiers.
#' @param risk_free Numeric. Risk-free rate used in the Sharpe ratio, at the
#'   same frequency as \code{stock_data}'s returns (daily by default).
#'   Default is \code{0}.
#' @param annualize Logical. If \code{TRUE}, annualizes \code{retorno} and
#'   \code{risco} assuming \code{periods_per_year} trading periods.
#'   Default is \code{FALSE}.
#' @param periods_per_year Integer. Used only when \code{annualize = TRUE}.
#'   Default is \code{252} (daily data).
#' @param expected_returns Optional. A tibble previously computed with
#'   \code{\link{calc_expected_return}}. Supplying this avoids recomputing it
#'   from \code{stock_data}, and lets you reuse the same variable across
#'   multiple calls (e.g. while trying different \code{risk_free} values).
#' @param cov_matrix Optional. A matrix previously computed with
#'   \code{\link{calc_covariance_matrix}}. Same rationale as
#'   \code{expected_returns}.
#'
#' @return A tibble with \code{n_portfolios} rows and the following columns:
#' \describe{
#'   \item{retorno}{Expected return of the portfolio}
#'   \item{risco}{Portfolio risk (standard deviation)}
#'   \item{sharpe}{Sharpe ratio ((return - risk_free) / risk)}
#'   \item{pesos}{List-column: named numeric vector of weights per ticker}
#' }
#'
#' @details
#' Portfolio weights are drawn from a uniform distribution and normalized to
#' sum to 1, ensuring full investment with no short-selling. Portfolio risk is
#' computed as:
#' \deqn{\sigma_p = \sqrt{w^T \Sigma w}}
#' where \eqn{w} is the weight vector and \eqn{\Sigma} is the covariance matrix.
#'
#' Weights are generated all at once as a matrix (vectorized), rather than in
#' a per-iteration loop, which is considerably faster for large
#' \code{n_portfolios}.
#'
#' @examples
#' \dontrun{
#' acoes <- get_stocks(c("PETR4", "VALE3", "ITUB4", "BBDC4"))
#'
#' # Recommended: compute inputs once, reuse the variables
#' retornos_esperados <- calc_expected_return(acoes)
#' matriz_cov         <- calc_covariance_matrix(acoes)
#' fronteira <- calc_efficient_frontier(
#'   acoes, n_portfolios = 10000,
#'   expected_returns = retornos_esperados, cov_matrix = matriz_cov
#' )
#' plot_efficient_frontier(fronteira)
#'
#' # Also supported: pass only stock_data (computed internally)
#' fronteira <- calc_efficient_frontier(acoes, n_portfolios = 10000)
#'
#' # Weights of the highest-Sharpe simulated portfolio
#' fronteira[which.max(fronteira$sharpe), ]$pesos[[1]]
#' }
#'
#' @export
calc_efficient_frontier <- function(stock_data       = NULL,
                                    n_portfolios     = 10000,
                                    risk_free        = 0,
                                    annualize        = FALSE,
                                    periods_per_year = 252,
                                    expected_returns = NULL,
                                    cov_matrix       = NULL) {

  if (is.null(expected_returns) || is.null(cov_matrix)) {
    if (is.null(stock_data)) {
      stop("Provide either 'stock_data', or both 'expected_returns' and 'cov_matrix'.",
           call. = FALSE)
    }
  }

  # Step 1: expected returns and covariance matrix -- reuse if supplied,
  # otherwise compute from stock_data
  retornos <- if (is.null(expected_returns)) calc_expected_return(stock_data) else expected_returns
  cov_mat  <- if (is.null(cov_matrix)) as.matrix(calc_covariance_matrix(stock_data)) else as.matrix(cov_matrix)

  tickers  <- retornos$ticker
  n_ativos <- length(tickers)

  # Step 2: generate all random weights at once (vectorized) instead of
  # looping n_portfolios times -- each row is normalized to sum to 1
  # Random weights are drawn from Exponential(1) and normalized to sum to 1.
  # This is equivalent to sampling from a Dirichlet(1, ..., 1) distribution,
  # which is uniform over the portfolio-weight simplex. Sampling from
  # Uniform(0, 1) and normalizing (a common shortcut) is NOT uniform over the
  # simplex: it under-samples corner allocations (portfolios concentrated in
  # a single asset) relative to balanced ones.
  pesos_brutos <- matrix(stats::rexp(n_portfolios * n_ativos, rate = 1), nrow = n_portfolios)
  pesos_mat    <- pesos_brutos / rowSums(pesos_brutos)
  colnames(pesos_mat) <- tickers

  # Step 3: portfolio return and risk for every simulated portfolio
  vetor_retornos <- retornos$expected_return
  ret_vec  <- as.numeric(pesos_mat %*% vetor_retornos)
  risco_vec <- sqrt(rowSums((pesos_mat %*% cov_mat) * pesos_mat))

  if (annualize) {
    ret_vec   <- ret_vec * periods_per_year
    risco_vec <- risco_vec * sqrt(periods_per_year)
  }

  sharpe_vec <- (ret_vec - risk_free) / risco_vec

  dplyr::tibble(
    retorno = ret_vec,
    risco   = risco_vec,
    sharpe  = sharpe_vec,
    pesos   = lapply(seq_len(n_portfolios), function(i) pesos_mat[i, ])
  )
}
