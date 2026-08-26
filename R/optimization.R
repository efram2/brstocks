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
#'   Note: with \code{"pairwise"}, the resulting covariance matrix is not
#'   guaranteed to be positive semi-definite, which can destabilize the
#'   optimization -- prefer \code{"intersection"} if in doubt.
#' @param expected_returns Optional. Pre-computed expected returns, e.g. from
#'   \code{\link{calc_expected_return}}. Must be computed at the same
#'   \code{freq_data} as this call, or annualization will be inconsistent
#'   (a warning is raised if a frequency mismatch is detected).
#' @param cov_matrix Optional. Pre-computed covariance matrix, e.g. from
#'   \code{\link{calc_covariance_matrix}}. Same frequency caveat as
#'   \code{expected_returns} applies.
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
#' This remains a Monte Carlo approximation, not a closed-form optimizer --
#' see the note in \code{\link{calc_key_portfolios}} for what that implies
#' for the reported optimum.
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
#' # Also supported: pass only the precomputed inputs, no stock_data
#' fronteira <- calc_efficient_frontier(
#'   expected_returns = retornos_esperados, cov_matrix = matriz_cov
#' )
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

  # Avisa se os inputs pré-computados vieram de uma frequência diferente da
  # usada aqui -- essa era exatamente a causa do bug de anualização
  # silenciosamente errada quando se compõe calc_expected_return()/
  # calc_covariance_matrix() manualmente com calc_efficient_frontier().
  freq_er  <- attr(expected_returns, "freq_data")
  freq_cov <- attr(cov_matrix, "freq_data")
  if (!is.null(freq_er) && freq_er != freq_data) {
    warning(sprintf(
      "expected_returns was computed with freq_data = '%s' but calc_efficient_frontier() is using freq_data = '%s'; annualization will be inconsistent.",
      freq_er, freq_data), call. = FALSE)
  }
  if (!is.null(freq_cov) && freq_cov != freq_data) {
    warning(sprintf(
      "cov_matrix was computed with freq_data = '%s' but calc_efficient_frontier() is using freq_data = '%s'; annualization will be inconsistent.",
      freq_cov, freq_data), call. = FALSE)
  }

  # Só monta e agrega a matriz de retornos quando de fato precisamos dela --
  # ou seja, quando pelo menos um de expected_returns/cov_matrix não foi
  # fornecido. Isso evita recomputação desnecessária quando os dois já vêm
  # prontos, e evita chamar .prepare_returns_matrix(NULL, ...) quando
  # stock_data não foi passado (uso documentado e válido nesse caso).
  if (is.null(expected_returns) || is.null(cov_matrix)) {
    if (is.null(stock_data)) {
      stop("Provide either 'stock_data', or both 'expected_returns' and 'cov_matrix'.",
           call. = FALSE)
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
      use_arg <- switch(na_method,
                        intersection = "everything",
                        pairwise     = "pairwise.complete.obs",
                        locf         = "complete.obs"
      )
      cov_matrix <- stats::cov(ret_matrix, use = use_arg)
    }
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

  resultado <- dplyr::tibble(
    retorno = ret_vec,
    risco = risco_vec,
    sharpe = sharpe_vec,
    pesos = lapply(seq_len(n_portfolios), function(i) pesos_mat[i, ])
  )
  attr(resultado, "freq_data") <- freq_data
  attr(resultado, "annualized") <- annualize
  resultado
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
#' step -- deliberately left out for now to keep the optimization surface
#' small while it's just this one Monte Carlo path.
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
