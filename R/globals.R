utils::globalVariables(c(
  "ticker", "ref_date", "ret_adjusted_prices", "ret_benchmark",
  "ativo1", "ativo2", "correlacao", "coluna_y", "ma",
  "risco", "retorno", "sharpe", "tipo", "value",
  # Funções internas que o check não encontra
  ".normalize_date",
  ".prepare_returns_matrix",
  ".aggregate_returns",
  ".week_grouping",
  ".month_grouping",
  ".calc_expected_returns_from_matrix"
))
