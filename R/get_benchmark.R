library(zoo)
library(ggplot2)

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


## gráfico da função de plot

plot_benchmark <- function(data,
                           market = "BR",
                           type = "index", 
                           ma_window = 200) {
  
  # 1. escolher a coluna certa dependendo do type 
  # 2. calcular a média móvel
  
  if(type == "index") {
    data$coluna_y <- data$cumret_adjusted_prices
    data$ma  <- zoo::rollmean(data$cumret_adjusted_prices, k = ma_window, fill = NA, align = "right")
    
  } else if (type == "price") {
    data$coluna_y <- data$price_adjusted
    data$ma  <- zoo::rollmean(data$price_adjusted, k = ma_window, fill = NA, align = "right")
    
  }
  
  # 2. plotar
  
  market_label <- switch(market,
                         "BR" = "Ibovespa",
                         "US" = "S&P 500")
  
  ggplot(data, aes(x = ref_date)) +
    geom_line(aes(y = coluna_y), color = "blue") +

    geom_line(aes(y = ma), color = "darkred") +
    theme_minimal() +
    labs(
      title    = paste0(market_label, " — ", ifelse(type == "index", "Accumulated Return", "Adjusted Price")),
      subtitle = ifelse(type == "index", "Base = 1 (first observation)", "In local currency"),
      x = "Date",
      y = ifelse(type == "index", "Accumulated Return (base = 1)", "Adjusted Price"),
      caption = "Source: Yahoo Finance via yfR"
    )
}