#' Simulate portfolios to build the efficient frontier
#'
#' @param stock_data A tibble returned by \code{\link{get_stocks}}.
#' @param n_portfolios Integer. Number of random portfolios to simulate.
#' @param risk_free Numeric. Risk-free rate at the same frequency as returns.
#' @param freq_data Character. Frequency for portfolio analysis.
#'   Default is \code{"monthly"} as recommended for robust covariance
#'   estimation and efficient frontier construction (less noise than
#'   daily data, more observations than yearly).
#' @param annualize Logical. If \code{TRUE}, annualizes return and risk.
#' @param periods_per_year Integer. Default 252 for daily, 12 for monthly.
#' @param na_method Character. Passed to \code{.prepare_returns_matrix()}.
#' @param expected_returns Optional. Pre-computed expected returns.
#' @param cov_matrix Optional. Pre-computed covariance matrix.
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
calc_efficient_frontier <- function(stock_data = NULL,
                                    n_portfolios = 10000,
                                    risk_free = 0,
                                    freq_data = "monthly",
                                    annualize = TRUE,
                                    periods_per_year = NULL,
                                    na_method = "intersection",
                                    expected_returns = NULL,
                                    cov_matrix = NULL) {

  # Validação de frequência
  freq_data <- match.arg(freq_data, c("daily", "weekly", "monthly"))

  # Define periods_per_year automaticamente se não fornecido
  if (is.null(periods_per_year)) {
    periods_per_year <- switch(freq_data,
                               daily = 252,
                               weekly = 52,
                               monthly = 12)
  }

  if (is.null(expected_returns) || is.null(cov_matrix)) {
    if (is.null(stock_data)) {
      stop("Provide either 'stock_data', or both 'expected_returns' and 'cov_matrix'.",
           call. = FALSE)
    }
  }

  # Prepara a matriz de retornos, alinhando datas
  ret_matrix <- .prepare_returns_matrix(stock_data, na_method = na_method)

  # Agrega para frequência desejada se necessário
  if (freq_data != "daily") {
    ret_matrix <- .aggregate_returns(ret_matrix,
                                     freq = freq_data,
                                     dates = attr(ret_matrix, "dates"))
  }

  # Cálculo dos retornos esperados e covariância a partir da matriz preparada
  if (is.null(expected_returns)) {
    expected_returns <- .calc_expected_returns_from_matrix(ret_matrix)
  }

  if (is.null(cov_matrix)) {
    cov_matrix <- stats::cov(ret_matrix)
  }

  tickers <- expected_returns$ticker
  n_ativos <- length(tickers)
  vetor_retornos <- expected_returns$expected_return

  # Gera todas as carteiras aleatórias de uma vez (vetorizado)
  # Usa distribuição Exponencial(1) normalizada -> Dirichlet(1,...,1)
  pesos_brutos <- matrix(stats::rexp(n_portfolios * n_ativos, rate = 1),
                         nrow = n_portfolios)
  pesos_mat <- pesos_brutos / rowSums(pesos_brutos)
  colnames(pesos_mat) <- tickers

  # Retorno e risco para cada carteira
  ret_vec <- as.numeric(pesos_mat %*% vetor_retornos)
  risco_vec <- sqrt(rowSums((pesos_mat %*% cov_matrix) * pesos_mat))

  if (annualize) {
    ret_vec <- ret_vec * periods_per_year
    risco_vec <- risco_vec * sqrt(periods_per_year)
  }

  sharpe_vec <- (ret_vec - risk_free) / risco_vec

  dplyr::tibble(
    retorno = ret_vec,
    risco = risco_vec,
    sharpe = sharpe_vec,
    pesos = lapply(seq_len(n_portfolios), function(i) pesos_mat[i, ])
  )
}

#' Agrega retornos para frequência especificada
#'
#' @keywords internal
.aggregate_returns <- function(ret_matrix, freq = "monthly", dates = NULL) {

  # Se não temos datas, usamos índices
  if (is.null(dates)) {
    dates <- seq.Date(from = Sys.Date() - nrow(ret_matrix) + 1,
                      to = Sys.Date(),
                      by = "day")
  }

  # Cria agrupadores baseados na frequência
  grupos <- switch(freq,
                   weekly = .week_grouping(dates),
                   monthly = .month_grouping(dates),
                   stop("Invalid frequency."))

  ret_aggregated <- stats::aggregate(ret_matrix,
                                     by = list(grupos),
                                     FUN = sum,
                                     na.rm = TRUE)

  # Remove a coluna de grupo e converte para matrix
  ret_aggregated <- as.matrix(ret_aggregated[, -1, drop = FALSE])

  # Guarda as datas agregadas como atributo
  attr(ret_aggregated, "dates") <- grupos[!duplicated(grupos)]

  return(ret_aggregated)
}

#' Cria agrupamento semanal
#' @keywords internal
.week_grouping <- function(dates) {
  format(dates, "%Y-W%V")
}

#' Cria agrupamento mensal
#' @keywords internal
.month_grouping <- function(dates) {
  format(dates, "%Y-%m")
}

#' Calcula retornos esperados a partir da matriz
#' @keywords internal
.calc_expected_returns_from_matrix <- function(ret_matrix) {
  retornos <- colMeans(ret_matrix, na.rm = TRUE)

  data.frame(
    ticker = colnames(ret_matrix),
    expected_return = retornos
  )
}
