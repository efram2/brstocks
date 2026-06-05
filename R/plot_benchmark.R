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

  ggplo2::ggplot(data, aes(x = ref_date)) +
    ggplo2::geom_line(aes(y = coluna_y), color = "#1f77b4") +

    ggplo2::geom_line(aes(y = ma), color = "#d62728") +
    ggplo2::theme_minimal() +
    ggplo2::labs(
      title    = paste0(market_label, " — ", ifelse(type == "index", "Accumulated Return", "Adjusted Price")),
      subtitle = ifelse(type == "index", "Base = 1 (first observation)", "In local currency"),
      x = "Date",
      y = ifelse(type == "index", "Accumulated Return (base = 1)", "Adjusted Price"),
      caption = "Source: Yahoo Finance via yfR"
    )
}
