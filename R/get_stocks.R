install.packages(c("yfR", "tidyverse", "ggplot2"), quiet = TRUE)

library(yfR)
library(tidyverse)

# A funCcao yf_get() do pacote yf tem uma funCcao que utiliza o quantmod
# para recuperar os dados histCoricos das acoes da bolsa brasileira (B3)
# desse modo, vamos reaproveita-lo na funCcao get_stocks

get_stocks <- function(tickers, 
                       from = NULL, 
                       to = NULL, 
                       freq = "daily",
                       add_sa = TRUE) {
  
  # Adiciona .SA automaticamente se necessário (padrão B3)
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