# brstocks 📈🇧🇷

brstocks is an R package for retrieving and analyzing Brazilian stock market data from Yahoo Finance, with tools for quantitative finance and portfolio analysis.

If you work with B3 stocks, ETFs, BDRs, or need to build and visualize portfolios, this package is for you.

## Why brstocks?

With brstocks, you can:

- Retrieve historical price data for any asset listed on B3 (stocks, ETFs, BDRs)
- Access market benchmark data (Ibovespa and S&P 500)
- Calculate key risk and return metrics: beta, expected return, variance
- Build covariance and correlation matrices ready for portfolio optimization
- Simulate and visualize the efficient frontier (Markowitz, 1952)

No manual API handling. No data wrangling required.

## Installation

```r
# Development version from GitHub
install.packages("devtools")
devtools::install_github("efram2/brstocks")

library(brstocks)
```

## 🚀 Quick Start

**Historical stock data**

```r
get_stocks(c("PETR4", "VALE3", "ITUB4"), from = "2020-01-01")
```

**Benchmark (Ibovespa)**

```r
ibov <- get_benchmark()
plot_benchmark(ibov)
```

**Beta**

```r
acoes <- get_stocks(c("PETR4", "VALE3", "ITUB4"))
ibov  <- get_benchmark()

calc_beta(acoes, ibov)
```

**Correlation matrix**

```r
plot_correlation_matrix(acoes)
```

**Efficient frontier**

```r
fronteira <- calc_efficient_frontier(acoes, n_portfolios = 10000)
plot_efficient_frontier(fronteira)
```

## Function Reference

**Data retrieval (`get_*`)**

- `get_stocks()` → Historical OHLCV data for B3-listed stocks, ETFs and BDRs
- `get_benchmark()` → Historical data for Ibovespa (`BR`) or S&P 500 (`US`)

All `get_*` functions support flexible date inputs: `"YYYY-MM-DD"`, `Date`, or `NULL` (defaults to last 365 days).

**Analytics (`calc_*`)**

- `calc_beta()` → Beta coefficient relative to a benchmark
- `calc_expected_return()` → Mean historical log return per ticker
- `calc_variance()` → Return variance per ticker
- `calc_covariance_matrix()` → Pairwise covariance matrix
- `calc_correlation_matrix()` → Pairwise Pearson correlation matrix
- `calc_efficient_frontier()` → Monte Carlo portfolio simulation

**Visualization (`plot_*`)**

- `plot_benchmark()` → Price or index chart with configurable moving average
- `plot_correlation_matrix()` → Correlation heatmap
- `plot_efficient_frontier()` → Efficient frontier colored by Sharpe ratio

## Data Source

All data is retrieved from [Yahoo Finance](https://finance.yahoo.com/) via the [yfR](https://github.com/ropensci/yfR) package (Perlin, 2022).

> **Note:** Yahoo Finance data is suitable for academic research and personal analysis.
> For production use in research papers or dissertations, consider professional
> data sources such as Economática or EODHD.

## Contribution

Suggestions, feature requests, and pull requests are welcome!

## Author

João Paulo dos Santos Pereira Barbosa — Economics student at PUC-Rio.  
Also the author of [brfinance](https://github.com/efram2/brfinance), an R package for Brazilian macroeconomic data.
