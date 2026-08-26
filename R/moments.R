#' Calculate the expected return of one or more stocks
#'
#' Estimates the expected return for each ticker as the historical mean of
#' log-adjusted returns. This is the maximum likelihood estimator commonly
#' used in quantitative finance and portfolio theory.
#'
#' @param stock_data A tibble returned by \code{\link{get_stocks}}.
#' @param na_method Character. How to handle missing observations.
#'   One of "intersection" (default), "pairwise", or "locf".
#'   See \code{.prepare_returns_matrix} for details.
#' @param freq_data Character. Frequency at which returns are aggregated
#'   before averaging. One of "daily", "weekly", or \code{"monthly"}
#'   (default). Matches the default of \code{\link{calc_efficient_frontier}}
#'   so that composing these functions manually (see examples) produces
#'   consistent results out of the box.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{ticker}{Ticker identifier}
#'   \item{expected_return}{Mean log return for the period}
#' }
#'
#' @details
#' Returns are computed at the frequency given by \code{freq_data}. To
#' annualize, multiply by the number of periods per year (252 for daily,
#' 52 for weekly, 12 for monthly). If you pass the result into
#' \code{\link{calc_efficient_frontier}} via its \code{expected_returns}
#' argument, make sure to use the same \code{freq_data} in both calls --
#' otherwise annualization will be inconsistent (a warning is raised in
#' that case).
#'
#' @examples
#' \dontrun{
#' acoes <- get_stocks(c("PETR4", "VALE3"))
#' calc_expected_return(acoes)
#' }
#'
#' @export
calc_expected_return <- function(stock_data,
                                 na_method = "intersection",
                                 freq_data = "monthly") {

  freq_data <- match.arg(freq_data, c("daily", "weekly", "monthly"))

  ret_matrix <- .prepare_returns_matrix(stock_data, na_method = na_method)

  if (freq_data != "daily") {
    ret_matrix <- .aggregate_returns(ret_matrix, freq = freq_data,
                                     dates = attr(ret_matrix, "dates"))
  }

  resultado <- data.frame(
    ticker = colnames(ret_matrix),
    expected_return = colMeans(ret_matrix, na.rm = TRUE)
  )
  attr(resultado, "freq_data") <- freq_data
  resultado
}

#' Calculate the return variance of one or more stocks
#'
#' Computes the sample variance of log-adjusted returns for each ticker.
#' Variance is the primary measure of individual asset risk in portfolio theory.
#'
#' @param stock_data A tibble returned by \code{\link{get_stocks}}.
#' @param na_method Character. How to handle missing observations.
#'   One of "intersection" (default), "pairwise", or "locf".
#'   See \code{.prepare_returns_matrix} for details.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{ticker}{Ticker identifier}
#'   \item{variance}{Sample variance of log returns for the period}
#' }
#'
#' @details
#' Variance is computed at the frequency of the input data (daily by default).
#' To annualize, multiply by the number of trading periods per year
#' (252 for daily, 52 for weekly, 12 for monthly).
#'
#' @examples
#' \dontrun{
#' acoes <- get_stocks(c("PETR4", "VALE3"))
#' calc_variance(acoes)
#' }
#'
#' @export
calc_variance <- function(stock_data, 
                          na_method = "intersection") {
  
  ret_matrix <- .prepare_returns_matrix(stock_data, na_method = na_method)

  data.frame(
    ticker = colnames(ret_matrix),
    variance = apply(ret_matrix, 2, stats::var, na.rm = TRUE)
  )
}

#' Calculate the covariance matrix of a set of stocks
#'
#' Computes the sample covariance matrix of log-adjusted returns for all
#' tickers in \code{stock_data}. The diagonal contains each asset's variance,
#' while off-diagonal elements represent pairwise covariances.
#'
#' The covariance matrix is a required input for portfolio optimization
#' under Modern Portfolio Theory (Markowitz, 1952).
#'
#' @param stock_data A tibble returned by \code{\link{get_stocks}}, containing
#'   one or more tickers.
#' @param na_method Character. How to handle missing observations.
#'   One of "intersection" (default), "pairwise", or "locf".
#'   See \code{.prepare_returns_matrix} for details.
#' @param freq_data Character. Frequency at which returns are aggregated
#'   before computing covariance. One of "daily", "weekly", or
#'   \code{"monthly"} (default). Matches the default of
#'   \code{\link{calc_efficient_frontier}} so that composing these functions
#'   manually (see examples) produces consistent results out of the box.
#'
#' @return A named numeric matrix of dimensions \eqn{n \times n}, where
#'   \eqn{n} is the number of unique tickers.
#'
#' @details
#' If you pass the result into \code{\link{calc_efficient_frontier}} via its
#' \code{cov_matrix} argument, make sure to use the same \code{freq_data} in
#' both calls -- otherwise annualization will be inconsistent (a warning is
#' raised in that case).
#'
#' When \code{na_method = "pairwise"}, each cell of the matrix is computed
#' from the dates common to that specific pair of assets, which can use more
#' of each asset's history than \code{"intersection"}. Be aware this does not
#' guarantee the resulting matrix is positive semi-definite, since different
#' cells may be estimated from different subsets of dates -- this can lead to
#' an invalid covariance structure and unstable optimization results
#' downstream. Inspect the matrix (e.g. \code{eigen(result)$values}) before
#' relying on it, or prefer \code{"intersection"} if in doubt.
#'
#' @examples
#' \dontrun{
#' acoes <- get_stocks(c("PETR4", "VALE3", "ITUB4"))
#' calc_covariance_matrix(acoes)
#' }
#'
#' @export
calc_covariance_matrix <- function(stock_data, 
                                   na_method = "intersection",
                                   freq_data = "monthly") {
  
  freq_data <- match.arg(freq_data, c("daily", "weekly", "monthly"))

  ret_matrix <- .prepare_returns_matrix(stock_data, na_method = na_method)
  if (freq_data != "daily") {
    ret_matrix <- .aggregate_returns(ret_matrix, freq = freq_data,
                                     dates = attr(ret_matrix, "dates"))
  }

  # na_method = "pairwise" deliberately leaves NAs in ret_matrix (see
  # .prepare_returns_matrix()); stats::cov()'s default use = "everything"
  # does NOT do pairwise deletion on its own -- without this, any leftover
  # NA silently propagates to the entire matrix.
  use_arg <- switch(na_method,
                    intersection = "everything",
                    pairwise     = "pairwise.complete.obs",
                    locf         = "complete.obs"  # drops any row LOCF couldn't fill (e.g. leading NAs)
  )

  resultado <- stats::cov(ret_matrix, use = use_arg)
  attr(resultado, "freq_data") <- freq_data
  resultado
}

#' Calculate the correlation matrix of a set of stocks
#'
#' Computes the sample Pearson correlation matrix of log-adjusted returns for
#' all tickers in \code{stock_data}. All values are bounded between -1 and 1,
#' making it easier to interpret than the covariance matrix.
#'
#' \describe{
#'   \item{1}{Assets move perfectly together}
#'   \item{0}{No linear relationship}
#'   \item{-1}{Assets move in opposite directions}
#' }
#'
#' @param stock_data A tibble returned by \code{\link{get_stocks}}, containing
#'   one or more tickers.
#' @param na_method Character. How to handle missing observations.
#'   One of "intersection" (default), "pairwise", or "locf".
#'   See \code{.prepare_returns_matrix} for details.
#' @param freq_data Character. Frequency at which returns are aggregated
#'   before computing correlation. One of "daily", "weekly", or
#'   \code{"monthly"} (default). Matches the default of
#'   \code{\link{calc_efficient_frontier}}.
#'
#' @return A named numeric matrix of dimensions \eqn{n \times n}, where
#'   \eqn{n} is the number of unique tickers.
#'
#' @details
#' Daily correlation between assets that trade on different exchanges (e.g.
#' B3 vs. NYSE) tends to be understated, because information from one market
#' does not reach the other instantaneously (the "Epps effect"). Aggregating
#' to a lower frequency, as done by default here, mitigates this.
#'
#' When \code{na_method = "pairwise"}, each cell of the matrix is computed
#' from the dates common to that specific pair of assets. Be aware this does
#' not guarantee the resulting matrix is positive semi-definite -- inspect it
#' (e.g. \code{eigen(result)$values}) before relying on it downstream, or
#' prefer \code{"intersection"} if in doubt.
#'
#' @examples
#' \dontrun{
#' acoes <- get_stocks(c("PETR4", "VALE3", "ITUB4", "BBDC4"))
#' calc_correlation_matrix(acoes)
#' }
#'
#' @export
calc_correlation_matrix <- function(stock_data, 
                                    na_method = "intersection",
                                    freq_data = "monthly") {
  
  freq_data <- match.arg(freq_data, c("daily", "weekly", "monthly"))

  ret_matrix <- .prepare_returns_matrix(stock_data, na_method = na_method)
  if (freq_data != "daily") {
    ret_matrix <- .aggregate_returns(ret_matrix, freq = freq_data,
                                     dates = attr(ret_matrix, "dates"))
  }

  use_arg <- switch(na_method,
                    intersection = "everything",
                    pairwise     = "pairwise.complete.obs",
                    locf         = "complete.obs"
  )

  resultado <- stats::cor(ret_matrix, use = use_arg)
  attr(resultado, "freq_data") <- freq_data
  resultado
}

#' Calculate the beta of one or more stocks
#'
#' Computes the beta coefficient for each ticker in \code{stock_data} relative
#' to a benchmark index. Beta measures how much an asset moves in relation to
#' the market: a beta greater than 1 indicates higher volatility than the
#' market, while a beta less than 1 indicates lower volatility.
#'
#' The formula used is:
#' \deqn{\beta = \frac{Cov(R_i, R_m)}{Var(R_m)}}
#'
#' Dates are aligned via an inner join, so only trading days present in both
#' datasets are used in the calculation.
#'
#' @param stock_data A tibble returned by \code{\link{get_stocks}}.
#' @param benchmark_data A tibble returned by \code{\link{get_benchmark}}.
#' @param na_method Character. How to handle missing observations.
#'   One of "intersection" (default), "pairwise", or "locf".
#'   See \code{.prepare_returns_matrix} for details.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{ticker}{Ticker identifier}
#'   \item{beta}{Beta coefficient relative to the benchmark}
#' }
#'
#' @examples
#' \dontrun{
#' acoes <- get_stocks(c("PETR4", "VALE3", "ITUB4"))
#' ibov  <- get_benchmark()
#' calc_beta(acoes, ibov)
#' }
#'
#' @export
calc_beta <- function(stock_data, 
                      benchmark_data, 
                      na_method = "intersection") {

  # Prepara a matriz de retornos dos ativos
  ret_matrix <- .prepare_returns_matrix(stock_data, na_method = na_method)
  tickers <- colnames(ret_matrix)

  # Prepara a matriz de retornos do benchmark
  benchmark_clean <- benchmark_data %>%
    dplyr::filter(!is.na(ret_adjusted_prices)) %>%
    dplyr::select(ref_date, ret_benchmark = ret_adjusted_prices)

  # Pega as datas dos ativos
  datas_ativos <- attr(ret_matrix, "dates")

  # Cria data frame com os retornos dos ativos e do benchmark alinhados
  result <- data.frame(ticker = character(), beta = numeric())

  for (ticker in tickers) {
    # Cria data frame com os dados do ativo
    ret_asset <- data.frame(
      ref_date = datas_ativos,
      ret_asset = ret_matrix[, ticker]
    )

    # Junta com o benchmark
    dados_combinados <- ret_asset %>%
      dplyr::inner_join(benchmark_clean, by = "ref_date")

    # Calcula beta
    beta_val <- stats::cov(dados_combinados$ret_asset,
                           dados_combinados$ret_benchmark) /
      stats::var(dados_combinados$ret_benchmark)

    result <- rbind(result, data.frame(ticker = ticker, beta = beta_val))
  }

  return(result)
}
