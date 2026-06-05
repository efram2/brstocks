get_stocks <- function(tickers,
                       from = NULL,
                       to = NULL,
                       freq = "daily",
                       add_sa = TRUE) {

  # Adiciona .SA automaticamente se necessario (padrao B3)
  if (add_sa) {
    tickers <- ifelse(
      grepl("\\.SA$", tickers) | grepl("^\\^", tickers),
      tickers,
      paste0(tickers, ".SA")
    )
  }

  # Ausência de informações
  if(is.null(from)){
    from <- Sys.Date() - 365
  }
  if(is.null(to)){
    to <- Sys.Date()
  }

  dados <- yfR::yf_get(
    tickers    = tickers,
    first_date = from,
    last_date  = to,
    freq_data = freq,
    type_return = "log",  # log return é padrão em finanças quant
    be_quiet = TRUE
  )

  return(dados)
  }
