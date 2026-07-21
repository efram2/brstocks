#' Get the Brazilian risk-free interest rate series (CDI or SELIC)
#'
#' Thin wrapper around \pkg{brfinance}'s \code{get_cdi_rate()} and
#' \code{get_selic_rate()}, which fetch these series from the Brazilian
#' Central Bank's open data API (SGS -- Sistema Gerenciador de Séries
#' Temporais). Useful as the \code{risk_free} input for
#' \code{\link{calc_efficient_frontier}}.
#'
#' @param rate Character. \code{"cdi"} (default) or \code{"selic"}.
#' @param from Start date. Defaults to 365 days before today.
#' @param to End date. Defaults to today.
#'
#' @return A tibble with columns \code{ref_date} and \code{taxa} (daily rate,
#'   as a decimal -- e.g. \code{0.0004} means 0.04% a.d.).
#'
#' @details
#' brstocks used to hit the BCB/SGS API directly for this. It now delegates
#' to \pkg{brfinance} instead, which already wraps that API (with its own
#' internal series-fetching helper) and is a dependency shared with
#' brstocks' sibling package.
#'
#' @examples
#' \dontrun{
#' cdi <- get_risk_free("cdi", from = "2024-01-01")
#' mean(cdi$taxa)  # taxa média diária no período
#' }
#'
#' @export
get_risk_free <- function(rate = "cdi", from = NULL, to = NULL) {

  rate <- match.arg(rate, c("cdi", "selic"))

  if (is.null(from)) from <- Sys.Date() - 365
  if (is.null(to))   to   <- Sys.Date()

  from_chr <- as.character(as.Date(from))
  to_chr   <- as.character(as.Date(to))

  brutos <- switch(rate,
                   "cdi"   = brfinance::get_cdi_rate(start_date = from_chr, end_date = to_chr,
                                                     language = "eng", labels = FALSE),
                   "selic" = brfinance:::.get_sgs_series(11, start_date = from_chr, end_date = to_chr)
  )

  dplyr::transmute(
    brutos,
    ref_date = as.Date(date),
    taxa     = value / 100  # brfinance retorna em % ao dia, igual à API crua da SGS
  )
}

#' Average daily risk-free rate for a period
#'
#' Convenience wrapper around \code{\link{get_risk_free}} that returns a
#' single average daily rate -- ready to plug into the \code{risk_free}
#' argument of \code{\link{calc_efficient_frontier}}.
#'
#' @inheritParams get_risk_free
#' @return A single numeric value (average daily rate as a decimal).
#' @export
calc_avg_risk_free <- function(rate = "cdi", from = NULL, to = NULL) {
  serie <- get_risk_free(rate, from, to)
  mean(serie$taxa, na.rm = TRUE)
}
