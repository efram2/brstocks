#' Normalize user-supplied dates
#'
#' Internal helper function used throughout the package to normalize
#' user-supplied dates into \code{Date} objects.
#'
#' Accepted formats are:
#' \itemize{
#'   \item \code{NULL}
#'   \item \code{"YYYY"}
#'   \item \code{"YYYY-MM"}
#'   \item \code{"YYYY-MM-DD"}
#' }
#'
#' @param x Date supplied by the user.
#' @param is_start Logical. If \code{TRUE}, incomplete dates are interpreted
#'   as the beginning of the period. Otherwise they are interpreted as the end
#'   of the period.
#'
#' @return A \code{Date} object.
#'
#' @keywords internal

.normalize_date <- function(x, is_start = TRUE) {

  # Handle NULL: start = distant past, end = today
  if (is.null(x)) {
    return(if (is_start) as.Date("1900-01-01") else Sys.Date())
  }

  # Ensure character
  x <- as.character(x)

  # Year only: "2020"
  if (nchar(x) == 4 && grepl("^\\d{4}$", x)) {
    return(as.Date(paste0(x, if (is_start) "-01-01" else "-12-31")))
  }

  # Year-month: "2020-06"
  if (nchar(x) == 7 && grepl("^\\d{4}-\\d{2}$", x)) {
    if (is_start) {
      return(as.Date(paste0(x, "-01")))
    } else {
      # Simple approach: use day 28 (safe for all months in BCB API)
      return(as.Date(paste0(x, "-28")))
    }
  }

  # Full date: "2020-06-15" (or any other valid format)
  tryCatch(
    as.Date(x),
    error = function(e) {
      stop(
        sprintf("Invalid date format: '%s'. Use: NULL, YYYY, YYYY-MM, or YYYY-MM-DD", x),
        call. = FALSE
      )
    }
  )
}
