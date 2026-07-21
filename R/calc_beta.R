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
calc_beta <- function(stock_data, benchmark_data, na_method = "intersection") {

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
