#' Launch the brstocks interactive dashboard
#'
#' Opens a Shiny dashboard where the user can pick B3 tickers, simulate the
#' efficient frontier, and explore portfolio composition interactively.
#'
#' This function requires the \code{shiny}, \code{bslib}, \code{plotly}, and
#' \code{DT} packages, which are listed under \code{Suggests} rather than
#' \code{Imports} because the dashboard is optional functionality: most uses
#' of \pkg{brstocks} (fetching and analyzing stock data) do not require a
#' full Shiny stack to be installed.
#'
#' @return No return value, called for its side effect of launching a Shiny
#'   application in the user's browser.
#'
#' @examples
#' \dontrun{
#' run_dashboard()
#' }
#'
#' @export
run_dashboard <- function() {
  pacotes_necessarios <- c("shiny", "bslib", "plotly", "DT")
  faltando <- pacotes_necessarios[!vapply(pacotes_necessarios, requireNamespace,
                                          quietly = TRUE, FUN.VALUE = logical(1))]

  if (length(faltando) > 0) {
    stop(
      "The following packages are required to run the dashboard but are not installed: ",
      paste(faltando, collapse = ", "),
      ".\nInstall them with: install.packages(c(", paste0('"', faltando, '"', collapse = ", "), "))",
      call. = FALSE
    )
  }

  app_dir <- system.file("shiny", "dashboard", package = "brstocks")
  if (app_dir == "") {
    stop("Could not find the dashboard directory. Try reinstalling {brstocks}.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
