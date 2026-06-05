get_benchmark <- function(market = "BR",
                          from = NULL,
                          to = NULL,
                          freq = "daily") {

  ticker <- switch(market,
                   "BR" = "^BVSP",
                   "US" = "^GSPC",
                   stop("Mercado não suportado. Use 'BR' ou 'US'.")
  )

  get_stocks(ticker,
             from = from,
             to = to,
             freq = freq,
             add_sa = FALSE)
}
