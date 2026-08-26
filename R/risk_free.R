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
#' brstocks used to hit the BCB/SGS API directly for the SELIC series (via an
#' internal \code{.get_sgs_series()} helper), while CDI already delegated to
#' \pkg{brfinance}. Both rates now delegate to \pkg{brfinance} --
#' \code{brfinance::get_selic_rate()} already returns daily SELIC data, so
#' there is no longer any reason to keep a second, unmaintained copy of the
#' SGS-fetching logic (with its own date chunking, retries, etc.) inside
#' brstocks. \pkg{brfinance} owns that complexity now.
#'
#' @examples
#' \dontrun{
#' cdi <- get_risk_free("cdi", from = "2024-01-01")
#' mean(cdi$taxa)  # taxa média diária no período
#'
#' selic <- get_risk_free("selic", from = "2024-01-01")
#' mean(selic$taxa)
#' }
#'
#' @export
get_risk_free <- function(rate = "cdi", 
                          from = NULL, 
                          to = NULL) {

  rate <- match.arg(rate, c("cdi", "selic"))

  from <- .normalize_date(from, TRUE)
  to   <- .normalize_date(to, FALSE)

  from_chr <- as.character(from)
  to_chr   <- as.character(to)

  brutos <- switch(rate,
    "cdi"   = brfinance::get_cdi_rate(start_date = from_chr, end_date = to_chr,
                                      language = "eng", labels = FALSE),
    "selic" = brfinance::get_selic_rate(start_date = from_chr, end_date = to_chr,
                                        language = "eng", labels = FALSE)
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
calc_avg_risk_free <- function(rate = "cdi", 
                               from = NULL, 
                               to = NULL) {
  
  serie <- get_risk_free(rate, from, to)
  mean(serie$taxa, na.rm = TRUE)
}
