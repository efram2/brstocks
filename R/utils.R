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
#' @details
#' This used to be defined twice in the package (once here, once inside the
#' old \code{download_sgs.R}, which has been removed now that SELIC also
#' delegates to \pkg{brfinance}) -- harmless in practice since both copies
#' were identical, but worth not repeating.
#'
#' @keywords internal
.normalize_date <- function(x, 
                            is_start = TRUE) {

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

#' Prepare return matrix for portfolio calculations
#'
#' Internal helper used by portfolio functions.
#'
#' Converts the output of get_stocks() into a numeric matrix where each
#' column represents an asset and each row represents a trading date.
#'
#' Missing observations are handled according to `na_method`.
#'
#' Available methods:
#'
#' "intersection"
#'   Keeps only dates where every asset has an available return.
#'   This is the default and recommended approach for portfolio analysis,
#'   ensuring all assets are evaluated over the same time period.
#'
#' "pairwise"
#'   Keeps missing values. Functions supporting pairwise calculations
#'   may use the available observations for each asset pair.
#'
#' "locf"
#'   Applies Last Observation Carried Forward (LOCF) to fill missing
#'   observations before returning the matrix.
#'
#' When using "intersection", the function automatically detects whether
#' observations were discarded. If so, a warning is emitted informing
#' that the return series were aligned using their common history.
#'
#' @param stock_data Output from get_stocks().
#' @param na_method Character indicating how missing observations should
#'   be handled. One of "intersection", "pairwise" or "locf".
#'
#' @return A numeric matrix of asset returns.
#'
#' @keywords internal
.prepare_returns_matrix <- function(stock_data, 
                                    na_method = "intersection") {

  retornos <- stock_data |>
    dplyr::filter(!is.na(ret_adjusted_prices)) |>
    dplyr::select(ref_date, ticker, ret_adjusted_prices) |>
    tidyr::pivot_wider(
      names_from = ticker,
      values_from = ret_adjusted_prices
    ) |>
    dplyr::arrange(ref_date)

  # GUARDA AS DATAS
  datas <- retornos$ref_date

  retornos <- retornos |>
    dplyr::select(-ref_date)

  if (na_method == "intersection") {
    complete_rows <- stats::complete.cases(retornos)
    removed_obs <- sum(!complete_rows)

    if (removed_obs > 0) {
      datas_after <- datas[complete_rows]

      warning(
        sprintf(
          "Asset return series were aligned using their common history (\"%s\" to \"%s\"). %d observation(s) were removed. %d observation(s) remain.",
          min(datas_after), max(datas_after), removed_obs, sum(complete_rows)
        ),
        call. = FALSE
      )
    }

    retornos <- retornos[complete_rows, ]
    datas <- datas[complete_rows]
  } else if (na_method == "pairwise") {
    # Keep missing values
  } else if (na_method == "locf") {
    retornos <- tidyr::fill(retornos, dplyr::everything(), .direction = "down")
  } else {
    stop("Unknown 'na_method'. Use 'intersection', 'pairwise' or 'locf'.", call. = FALSE)
  }

  # RETORNA COM ATRIBUTO DE DATAS
  resultado <- as.matrix(retornos)
  attr(resultado, "dates") <- datas

  return(resultado)
}
