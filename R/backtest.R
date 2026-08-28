#' Walk-forward backtest of an exact portfolio strategy
#'
#' Simulates holding a portfolio through time with periodic rebalancing,
#' where the weights decided at each rebalance are estimated \emph{only}
#' from data available up to and including that date -- never from data
#' that lies in the future relative to the decision being made.
#'
#' @param stock_data A tibble returned by \code{\link{get_stocks}}.
#' @param portfolio_type Character. Which portfolio to target at each
#'   rebalance: \code{"max_sharpe"} (default), \code{"min_variance"}, or
#'   \code{"max_return"}. Solved exactly at each rebalance via
#'   \code{\link{.exact_portfolio_weights}}.
#' @param rebalance_every Integer. Number of \strong{trading days} (business
#'   days as observed in \code{stock_data} -- weekends and market holidays
#'   are already absent from it) between rebalances. Default \code{21}
#'   (roughly monthly).
#' @param min_window Integer. Minimum number of trading days of history
#'   required before the first rebalance can happen. Default \code{252}
#'   (roughly 1 trading year) -- chosen so that, once aggregated under the
#'   default \code{freq_data = "monthly"}, the first estimation window has
#'   about a year's worth of monthly observations, consistent with
#'   \code{\link{calc_efficient_frontier}}'s own default period.
#' @param turnover_cost Numeric, in \code{[0, 1]}. Cost charged as a
#'   fraction of the portfolio's value each time it's rebalanced, scaled by
#'   how much the weights actually moved (the standard turnover metric:
#'   \eqn{\text{turnover} = \frac{1}{2}\sum_i |w_i^{new} - w_i^{old}|}).
#'   E.g. \code{turnover_cost = 0.01} means a full 100\%-turnover
#'   rebalance would cost 1\% of portfolio value; a 20\%-turnover
#'   rebalance costs 0.2\%. Default \code{0} (no cost). The very first
#'   rebalance -- the initial purchase, going from no position to the
#'   first set of weights -- is never charged a cost, since that's not
#'   really "rebalancing" an existing portfolio.
#' @param risk_free Numeric. \strong{Daily} risk-free rate (the same daily
#'   scale used everywhere else in the package, e.g.
#'   \code{\link{calc_avg_risk_free}}'s output) -- regardless of
#'   \code{freq_data}. Internally rescaled to match \code{freq_data}'s
#'   periodicity before being used in the tangency calculation, so you
#'   never need to convert it yourself. Used only when
#'   \code{portfolio_type = "max_sharpe"}. If no asset's expected return
#'   beats \code{risk_free} within a given window -- which can legitimately
#'   happen, e.g. during a bad year for the assets in \code{stock_data} --
#'   the tangency portfolio is undefined for that window; the backtest
#'   falls back to the minimum-variance portfolio for that single rebalance
#'   (with a \code{warning()}) rather than aborting the whole run.
#' @param freq_data Character. Frequency the return series is aggregated to
#'   \strong{before estimating} expected returns and the covariance matrix
#'   at each rebalance: one of \code{"monthly"} (default), \code{"weekly"},
#'   or \code{"daily"}. Matches \code{\link{calc_efficient_frontier}}'s own
#'   default and rationale -- monthly aggregation reduces noise and
#'   mitigates correlation distortions between assets on different trading
#'   calendars. This only affects how weights are \emph{decided}; the
#'   resulting equity curve is still simulated day by day using daily
#'   returns (see Details).
#' @param na_method Character. Passed to \code{.prepare_returns_matrix()}.
#'
#' @return A tibble with columns \code{ref_date} and \code{value} (portfolio
#'   value, base = 1 at the first rebalance), covering only the period from
#'   the first rebalance onward (the \code{min_window} warm-up period has no
#'   defined portfolio yet and is dropped). Two attributes are attached:
#'   \describe{
#'     \item{\code{turnover}}{A data frame with one row per rebalance:
#'       \code{ref_date}, \code{turnover} (the metric above), and
#'       \code{custo} (the resulting cost as a fraction of portfolio value).}
#'     \item{\code{pesos_por_rebalance}}{A named list of weight vectors, one
#'       per rebalance date.}
#'   }
#'
#' @details
#' This is the walk-forward-correct counterpart to computing a single
#' \code{\link{calc_exact_portfolios}} (or \code{\link{calc_efficient_frontier}})
#' over an entire period and then plotting that portfolio's performance
#' across the \emph{same} period. That approach lets the weights "see" the
#' whole period, including data that would have been in the future at
#' earlier points in the backtest, and is not a valid backtest. Here, the
#' estimation window used at each rebalance always ends \emph{before} the
#' holding period it governs starts -- weights decided using data through
#' trading day \eqn{i} are applied starting at day \eqn{i+1}, never at day
#' \eqn{i} itself.
#'
#' The estimation window grows over time (\emph{expanding window}, using all
#' history from the start of \code{stock_data} up to the rebalance date) --
#' as opposed to a fixed-size rolling window that would discard older data.
#' At each rebalance, that expanding daily window is aggregated to
#' \code{freq_data} (see above) before \eqn{\mu} and \eqn{\Sigma} are
#' estimated from it -- the last period in the window is typically partial
#' (the rebalance date rarely falls exactly on a month boundary), which is
#' fine: it's still only ever real, already-observed data.
#'
#' Regardless of \code{freq_data}, the holding-period equity curve itself is
#' always simulated day by day: within a holding period, daily portfolio
#' returns are computed as the weight-averaged sum of each asset's daily log
#' return -- the same approximation already used elsewhere in this package
#' (e.g. the dashboard's "Portfolio vs Benchmark" chart) rather than
#' compounding simple returns day by day. This is a reasonable approximation
#' at typical rebalancing frequencies (daily deviations are small) but will
#' increasingly understate true compounding as \code{rebalance_every} grows
#' and individual asset weights drift further from target between
#' rebalances.
#'
#' @examples
#' \dontrun{
#' acoes <- get_stocks(c("PETR4", "VALE3", "ITUB4", "BBDC4"), from = "2015-01-01")
#'
#' bt <- calc_walkforward_backtest(
#'   acoes,
#'   portfolio_type = "max_sharpe",
#'   rebalance_every = 21,     # roughly monthly
#'   min_window = 252,         # ~1 year of history before the first rebalance
#'   turnover_cost = 0.001,    # 0.1% cost per unit of turnover
#'   risk_free = 0.0004        # e.g. average daily CDI rate
#' )
#'
#' plot(bt$ref_date, bt$value, type = "l")
#'
#' # Total transaction cost paid over the course of the backtest
#' sum(attr(bt, "turnover")$custo)
#' }
#'
#' @export
calc_walkforward_backtest <- function(stock_data,
                                      portfolio_type = "max_sharpe",
                                      rebalance_every = 21,
                                      min_window = 252,
                                      turnover_cost = 0,
                                      risk_free = 0,
                                      freq_data = "monthly",
                                      na_method = "intersection") {
  
  portfolio_type <- match.arg(portfolio_type, c("max_sharpe", "min_variance", "max_return"))
  freq_data <- match.arg(freq_data, c("daily", "weekly", "monthly"))
  
  if (turnover_cost < 0 || turnover_cost > 1) {
    stop("'turnover_cost' must be between 0 and 1 (it's a fraction of portfolio value).", call. = FALSE)
  }
  
  ret_matrix <- .prepare_returns_matrix(stock_data, na_method = na_method)
  dates   <- attr(ret_matrix, "dates")
  n       <- nrow(ret_matrix)
  tickers <- colnames(ret_matrix)
  
  if (min_window >= n) {
    stop(sprintf(
      "'min_window' (%d) must be smaller than the number of trading days available (%d).",
      min_window, n
    ), call. = FALSE)
  }
  if (rebalance_every < 1) {
    stop("'rebalance_every' must be a positive integer.", call. = FALSE)
  }
  
  use_arg <- switch(na_method, intersection = "everything",
                    pairwise = "pairwise.complete.obs", locf = "complete.obs")
  
  # risk_free chega sempre na escala DIARIA (mesma convencao usada no resto
  # do pacote, ex: calc_avg_risk_free()); aqui e' reescalado pra escala de
  # freq_data (mensal por padrao) via multiplicacao simples -- mesma
  # convencao de anualizacao ja usada em calc_exact_portfolios() e
  # calc_efficient_frontier() -- porque mu/cov sao estimados em freq_data
  # dentro do loop, entao a carteira tangente precisa de um risk_free na
  # MESMA escala pra fazer sentido.
  periods_per_year <- switch(freq_data, daily = 252, weekly = 52, monthly = 12)
  rf_periodo <- risk_free * (periods_per_year / 252)
  
  # Datas de rebalanceamento: a cada `rebalance_every` dias uteis de
  # negociacao (os proprios dias presentes em stock_data -- ja excluem
  # fins de semana e feriados, porque vieram da B3/Yahoo assim), comecando
  # assim que houver `min_window` dias de historico para estimar a
  # matriz de covariancia.
  rebalance_idx <- seq(min_window, n - 1, by = rebalance_every)
  
  pesos_atual <- stats::setNames(rep(0, length(tickers)), tickers)
  valor       <- rep(NA_real_, n)
  valor_atual <- 1
  turnover_log <- vector("list", length(rebalance_idx))
  pesos_log    <- list()
  
  for (k in seq_along(rebalance_idx)) {
    
    i <- rebalance_idx[k]
    
    # Janela expansivel: TODA a historia de 1 ate i (inclusive). As
    # observacoes em i+1 em diante nunca entram aqui -- e exatamente isso
    # que evita o vies de "olhar o futuro para otimizar o presente".
    janela_diaria <- ret_matrix[seq_len(i), , drop = FALSE]
    
    janela_estimacao <- if (freq_data == "daily") {
      janela_diaria
    } else {
      .aggregate_returns(janela_diaria, freq = freq_data, dates = dates[seq_len(i)])
    }
    
    mu  <- colMeans(janela_estimacao, na.rm = TRUE)
    cov <- stats::cov(janela_estimacao, use = use_arg)
    
    # Se o tipo pedido for "max_sharpe", a carteira tangente pode nao
    # existir nesta janela especifica (nenhum ativo bateu risk_free no
    # periodo ate aqui -- perfeitamente possivel num ano ruim, por
    # exemplo). Isso e' esperado, nao um bug: cair para minima variancia
    # SO nesse rebalanceamento (com aviso) e' melhor do que abortar um
    # backtest inteiro de dezenas de rebalanceamentos por causa de uma
    # unica janela.
    pesos_novo <- tryCatch(
      .exact_portfolio_weights(mu, cov, risk_free = rf_periodo, type = portfolio_type),
      error = function(e) {
        if (portfolio_type == "max_sharpe" && grepl("tangency", conditionMessage(e))) {
          warning(sprintf(
            "%s: no asset beat risk_free in this window; falling back to the minimum-variance portfolio for this rebalance only.",
            format(dates[i])
          ), call. = FALSE)
          .exact_portfolio_weights(mu, cov, risk_free = rf_periodo, type = "min_variance")
        } else {
          stop(e)  # qualquer outro erro (ex: matriz singular de verdade) continua parando o backtest
        }
      }
    )
    
    turnover <- sum(abs(pesos_novo - pesos_atual)) / 2
    custo <- if (k == 1) 0 else turnover * turnover_cost  # 1a compra nao e "rebalanceamento"
    valor_atual <- valor_atual * (1 - custo)
    valor[i] <- valor_atual
    
    turnover_log[[k]] <- data.frame(ref_date = dates[i], turnover = turnover, custo = custo)
    pesos_log[[as.character(dates[i])]] <- pesos_novo
    
    # Periodo de posse: do dia seguinte ao rebalanceamento ate o dia
    # anterior ao proximo (ou ate o fim dos dados, no ultimo rebalanceamento).
    fim_periodo <- if (k < length(rebalance_idx)) rebalance_idx[k + 1] else n
    
    for (t in seq.int(i + 1, fim_periodo)) {
      ret_dia <- sum(ret_matrix[t, ] * pesos_novo, na.rm = TRUE)
      valor_atual <- valor_atual * exp(ret_dia)
      valor[t] <- valor_atual
    }
    
    pesos_atual <- pesos_novo
  }
  
  resultado <- dplyr::tibble(ref_date = dates, value = valor) |>
    dplyr::filter(!is.na(value))
  
  attr(resultado, "turnover")            <- dplyr::bind_rows(turnover_log)
  attr(resultado, "pesos_por_rebalance") <- pesos_log
  attr(resultado, "portfolio_type")      <- portfolio_type
  attr(resultado, "rebalance_every")     <- rebalance_every
  attr(resultado, "turnover_cost")       <- turnover_cost
  attr(resultado, "freq_data")           <- freq_data
  
  resultado
}

#' Walk-forward backtest with periodic contributions (dollar-cost averaging)
#'
#' Like \code{\link{calc_walkforward_backtest}}, but simulates periodic
#' contributions into the portfolio (e.g. an initial deposit plus a monthly
#' top-up) on top of the same walk-forward, no-lookahead rebalancing.
#'
#' @inheritParams calc_walkforward_backtest
#' @param initial_contribution Numeric, \eqn{\geq 0}. Amount invested on the
#'   first day of the returned series (the first rebalance date). Default
#'   \code{1000}.
#' @param periodic_contribution Numeric, \eqn{\geq 0}. Amount added at the
#'   start of each new period, per \code{contribution_frequency}. Default
#'   \code{200}.
#' @param contribution_frequency Character. One of \code{"weekly"},
#'   \code{"monthly"} (default), \code{"quarterly"}, or \code{"semiannual"}.
#'
#' @return A tibble with columns \code{ref_date}, \code{saldo} (portfolio
#'   balance, in the same currency units as the contributions) and
#'   \code{investido} (cumulative amount contributed so far) -- matching the
#'   column names already used by brstocks's dashboard contribution
#'   simulator, so it's a drop-in replacement there. Same attributes as
#'   \code{\link{calc_walkforward_backtest}}.
#'
#' @details
#' Weights are re-estimated walk-forward exactly as in
#' \code{\link{calc_walkforward_backtest}} -- see its documentation for the
#' no-lookahead guarantee, the \code{freq_data} aggregation used for
#' estimation, and the \code{max_sharpe} fallback behavior. Contributions
#' are added to the balance at the start of each new period (per
#' \code{contribution_frequency}), \emph{before} that day's return is
#' applied -- same convention as the dashboard's existing (non-rebalancing)
#' contribution simulator.
#'
#' @examples
#' \dontrun{
#' acoes <- get_stocks(c("PETR4", "VALE3", "ITUB4", "BBDC4"), from = "2015-01-01")
#'
#' aportes <- calc_walkforward_contributions(
#'   acoes,
#'   portfolio_type = "max_sharpe",
#'   rebalance_every = 21,
#'   min_window = 252,
#'   turnover_cost = 0.001,
#'   risk_free = 0.0004,
#'   initial_contribution = 1000,
#'   periodic_contribution = 200,
#'   contribution_frequency = "monthly"
#' )
#'
#' plot(aportes$ref_date, aportes$saldo, type = "l")
#' lines(aportes$ref_date, aportes$investido, col = "gray")
#' }
#'
#' @export
calc_walkforward_contributions <- function(stock_data,
                                           portfolio_type = "max_sharpe",
                                           rebalance_every = 21,
                                           min_window = 252,
                                           turnover_cost = 0,
                                           risk_free = 0,
                                           freq_data = "monthly",
                                           na_method = "intersection",
                                           initial_contribution = 1000,
                                           periodic_contribution = 200,
                                           contribution_frequency = "monthly") {
  
  portfolio_type <- match.arg(portfolio_type, c("max_sharpe", "min_variance", "max_return"))
  freq_data <- match.arg(freq_data, c("daily", "weekly", "monthly"))
  contribution_frequency <- match.arg(contribution_frequency,
                                      c("weekly", "monthly", "quarterly", "semiannual"))
  
  if (turnover_cost < 0 || turnover_cost > 1) {
    stop("'turnover_cost' must be between 0 and 1 (it's a fraction of portfolio value).", call. = FALSE)
  }
  if (initial_contribution < 0 || periodic_contribution < 0) {
    stop("'initial_contribution' and 'periodic_contribution' must be >= 0.", call. = FALSE)
  }
  
  ret_matrix <- .prepare_returns_matrix(stock_data, na_method = na_method)
  dates   <- attr(ret_matrix, "dates")
  n       <- nrow(ret_matrix)
  tickers <- colnames(ret_matrix)
  
  if (min_window >= n) {
    stop(sprintf(
      "'min_window' (%d) must be smaller than the number of trading days available (%d).",
      min_window, n
    ), call. = FALSE)
  }
  if (rebalance_every < 1) {
    stop("'rebalance_every' must be a positive integer.", call. = FALSE)
  }
  
  use_arg <- switch(na_method, intersection = "everything",
                    pairwise = "pairwise.complete.obs", locf = "complete.obs")
  
  periods_per_year <- switch(freq_data, daily = 252, weekly = 52, monthly = 12)
  rf_periodo <- risk_free * (periods_per_year / 252)
  
  # Identificador de periodo de contribuicao -- mesma logica do simulador
  # de aportes original do dashboard (app.R's simular_aportes_fn()).
  id_periodo <- switch(contribution_frequency,
                       weekly     = format(dates, "%Y-%U"),
                       monthly    = format(dates, "%Y-%m"),
                       quarterly  = paste0(format(dates, "%Y"), "-T", ceiling(as.numeric(format(dates, "%m")) / 3)),
                       semiannual = paste0(format(dates, "%Y"), "-S", ceiling(as.numeric(format(dates, "%m")) / 6))
  )
  
  rebalance_idx <- seq(min_window, n - 1, by = rebalance_every)
  
  pesos_atual <- stats::setNames(rep(0, length(tickers)), tickers)
  saldo     <- rep(NA_real_, n)
  investido <- rep(NA_real_, n)
  
  saldo_atual      <- initial_contribution
  total_investido  <- initial_contribution
  periodo_anterior <- id_periodo[min_window]
  
  turnover_log <- vector("list", length(rebalance_idx))
  pesos_log    <- list()
  
  for (k in seq_along(rebalance_idx)) {
    
    i <- rebalance_idx[k]
    
    janela_diaria <- ret_matrix[seq_len(i), , drop = FALSE]
    
    janela_estimacao <- if (freq_data == "daily") {
      janela_diaria
    } else {
      .aggregate_returns(janela_diaria, freq = freq_data, dates = dates[seq_len(i)])
    }
    
    mu  <- colMeans(janela_estimacao, na.rm = TRUE)
    cov <- stats::cov(janela_estimacao, use = use_arg)
    
    pesos_novo <- tryCatch(
      .exact_portfolio_weights(mu, cov, risk_free = rf_periodo, type = portfolio_type),
      error = function(e) {
        if (portfolio_type == "max_sharpe" && grepl("tangency", conditionMessage(e))) {
          warning(sprintf(
            "%s: no asset beat risk_free in this window; falling back to the minimum-variance portfolio for this rebalance only.",
            format(dates[i])
          ), call. = FALSE)
          .exact_portfolio_weights(mu, cov, risk_free = rf_periodo, type = "min_variance")
        } else {
          stop(e)
        }
      }
    )
    
    turnover <- sum(abs(pesos_novo - pesos_atual)) / 2
    custo <- if (k == 1) 0 else turnover * turnover_cost
    saldo_atual <- saldo_atual * (1 - custo)
    
    if (k == 1) {
      saldo[i]     <- saldo_atual
      investido[i] <- total_investido
    }
    
    turnover_log[[k]] <- data.frame(ref_date = dates[i], turnover = turnover, custo = custo)
    pesos_log[[as.character(dates[i])]] <- pesos_novo
    
    fim_periodo <- if (k < length(rebalance_idx)) rebalance_idx[k + 1] else n
    
    for (t in seq.int(i + 1, fim_periodo)) {
      
      if (id_periodo[t] != periodo_anterior) {
        saldo_atual <- saldo_atual + periodic_contribution
        total_investido <- total_investido + periodic_contribution
        periodo_anterior <- id_periodo[t]
      }
      
      ret_dia <- sum(ret_matrix[t, ] * pesos_novo, na.rm = TRUE)
      saldo_atual <- saldo_atual * exp(ret_dia)
      saldo[t]     <- saldo_atual
      investido[t] <- total_investido
    }
    
    pesos_atual <- pesos_novo
  }
  
  resultado <- dplyr::tibble(ref_date = dates, saldo = saldo, investido = investido) |>
    dplyr::filter(!is.na(saldo))
  
  attr(resultado, "turnover")            <- dplyr::bind_rows(turnover_log)
  attr(resultado, "pesos_por_rebalance") <- pesos_log
  attr(resultado, "portfolio_type")      <- portfolio_type
  attr(resultado, "rebalance_every")     <- rebalance_every
  attr(resultado, "turnover_cost")       <- turnover_cost
  attr(resultado, "freq_data")           <- freq_data
  
  resultado
}