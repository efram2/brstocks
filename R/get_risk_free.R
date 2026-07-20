#' Get the Brazilian risk-free interest rate series (CDI or SELIC)
#'
#' Retrieves the daily CDI or SELIC rate series from the Brazilian Central
#' Bank's open data API (SGS -- Sistema Gerenciador de Séries Temporais).
#' Useful as the \code{risk_free} input for \code{\link{calc_efficient_frontier}}.
#'
#' @param rate Character. \code{"cdi"} (default, SGS series 12) or
#'   \code{"selic"} (SGS series 11).
#' @param from Start date. Defaults to 365 days before today.
#' @param to End date. Defaults to today.
#'
#' @return A tibble with columns \code{ref_date} and \code{taxa} (daily rate,
#'   as a decimal -- e.g. \code{0.0004} means 0.04% a.d.).
#'
#' @examples
#' \dontrun{
#' cdi <- get_risk_free("cdi", from = "2024-01-01")
#' mean(cdi$taxa)  # taxa média diária no período
#' }
#'
#' @export
get_risk_free <- function(rate = "cdi", from = NULL, to = NULL) {

  serie <- switch(rate,
    "cdi"   = 12,
    "selic" = 11,
    stop("Unsupported rate. Use 'cdi' or 'selic'.")
  )

  if (is.null(from)) from <- Sys.Date() - 365
  if (is.null(to))   to   <- Sys.Date()

  url <- sprintf(
    "https://api.bcb.gov.br/dados/serie/bcdata.sgs.%d/dados?formato=json&dataInicial=%s&dataFinal=%s",
    serie,
    format(as.Date(from), "%d/%m/%Y"),
    format(as.Date(to),   "%d/%m/%Y")
  )

  resp   <- httr2::request(url) |> httr2::req_perform()
  brutos <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  dplyr::tibble(
    ref_date = as.Date(brutos$data, format = "%d/%m/%Y"),
    taxa     = as.numeric(brutos$valor) / 100  # BCB retorna em % ao dia
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
