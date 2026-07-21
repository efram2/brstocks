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
.prepare_returns_matrix <- function(stock_data, na_method = "intersection") {

  retornos <- stock_data |>
    dplyr::filter(!is.na(ret_adjusted_prices)) |>
    dplyr::select(ref_date, ticker, ret_adjusted_prices) |>
    tidyr::pivot_wider(
      names_from = ticker,
      values_from = ret_adjusted_prices
    ) |>
    dplyr::arrange(ref_date)

  # 🔥 GUARDA AS DATAS
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

  # 🔥 RETORNA COM ATRIBUTO DE DATAS
  resultado <- as.matrix(retornos)
  attr(resultado, "dates") <- datas

  return(resultado)
}
