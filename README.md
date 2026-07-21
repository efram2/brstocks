# brstocks 📈🇧🇷

[![CRAN Status](https://www.r-pkg.org/badges/version/brstocks)](https://cran.r-project.org/package=brstocks)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/efram2/brstocks/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/efram2/brstocks/actions/workflows/R-CMD-check.yaml)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/brstocks)](https://cran.r-project.org/package=brstocks)
[![Downloads](https://cranlogs.r-pkg.org/badges/brstocks)](https://cran.r-project.org/package=brstocks)
[![GitHub stars](https://img.shields.io/github/stars/efram2/brstocks.svg)](https://github.com/efram2/brstocks/stargazers)
[![GitHub forks](https://img.shields.io/github/forks/efram2/brstocks.svg)](https://github.com/efram2/brstocks/network)

**brstocks** is an R package for retrieving and analyzing data from the
Brazilian stock market (B3), with tools for quantitative finance and
portfolio analysis — including an interactive dashboard for exploratory
portfolio simulation.

If you are new to the Brazilian market, start with the primer below before
jumping into the code.

---

## A primer on the Brazilian market (for newcomers)

If you're used to US or European markets, a few Brazil-specific terms show
up throughout this package and its dashboard:

**B3** is the Brazilian stock exchange (short for *Brasil, Bolsa, Balcão*),
formed in 2017 from the merger of BM&FBOVESPA (the former São Paulo stock
exchange) and Cetip (a securities-clearing company). Every stock, ETF, and
BDR mentioned in this README trades on B3. On Yahoo Finance, B3 tickers
carry a `.SA` suffix (e.g. `PETR4.SA`) — `get_stocks()` adds this suffix
for you automatically.

**Ibovespa** (`^BVSP`) is B3's main stock index, the Brazilian equivalent of
the S&P 500 — a capitalization-weighted basket of the most traded stocks on
the exchange.

**SELIC** is Brazil's benchmark base interest rate, set roughly every 45
days by the Copom (the Central Bank's Monetary Policy Committee). It is the
main tool of Brazilian monetary policy and, historically, one of the
highest base rates among major economies.

**CDI** (*Certificado de Depósito Interbancário*) is the interbank lending
rate — the rate banks charge each other for very short-term loans. It
tracks the SELIC rate closely (typically within a few basis points) and is,
in practice, *the* reference rate used across the Brazilian financial
industry: mutual funds report performance as "% of CDI," and it is the
standard proxy for a risk-free rate in Brazilian portfolio analysis — the
same role US Treasury bills play in a US context. `get_risk_free()` fetches
this series directly from the Central Bank's public data API.

With that vocabulary in hand, the rest of this README should read like any
other R package documentation.

---

## Why brstocks?

With brstocks, you can:

- Retrieve historical price data for any asset listed on B3 (stocks, ETFs, BDRs)
- Access market benchmark data (Ibovespa and S&P 500)
- Fetch Brazil's risk-free rate series (CDI or SELIC) directly from the Central Bank
- Calculate key risk and return metrics: beta, expected return, variance
- Build covariance and correlation matrices ready for portfolio optimization
- Simulate and visualize the Markowitz (1952) efficient frontier
- Explore all of the above interactively through a built-in Shiny dashboard

brstocks is a sibling package to
[brfinance](https://github.com/efram2/brfinance), which covers Brazilian
macroeconomic data (including exchange rates via `get_exchange_rate()`).

## Installation

```r
# Development version from GitHub
install.packages("devtools")
devtools::install_github("efram2/brstocks")

library(brstocks)
```

To use the interactive dashboard, also install its (optional) dependencies:

```r
install.packages(c("shiny", "bslib", "plotly", "DT", "scales", "purrr"))
```

These are listed under `Suggests`, not `Imports` — fetching and analyzing
stock data does not require a Shiny stack to be installed; only
`run_dashboard()` does.

## 🚀 Quick start: how the functions connect

brstocks functions are designed to be **composed**, not called in
isolation. The general pattern is: fetch data once, assign it to a
variable, and reuse that variable across every downstream calculation and
plot — rather than re-fetching or recomputing from scratch each time.

```r
library(brstocks)

# Step 1: fetch once, store in a variable
acoes <- get_stocks(c("PETR4", "VALE3", "ITUB4", "BBDC4"), from = "2022-01-01")
ibov  <- get_benchmark(market = "BR", from = "2022-01-01")

# Step 2: reuse `acoes` and `ibov` across independent analyses
calc_beta(acoes, ibov)
calc_expected_return(acoes)
calc_variance(acoes)

# Step 3: compute the covariance/correlation matrices once,
# then reuse THAT variable for plotting and for the efficient frontier --
# instead of passing raw stock_data to every downstream function again
cov_mat <- calc_covariance_matrix(acoes)
cor_mat <- calc_correlation_matrix(acoes)

plot_correlation_matrix(cor_mat)   # reuses cor_mat directly, no recomputation

retornos_esperados <- calc_expected_return(acoes)
fronteira <- calc_efficient_frontier(
  acoes,
  n_portfolios     = 10000,
  expected_returns = retornos_esperados,  # reuses the variable above
  cov_matrix       = cov_mat              # reuses the variable above
)
plot_efficient_frontier(fronteira)
```

> **Note on frequency:** `calc_expected_return()`, `calc_covariance_matrix()`,
> and `calc_efficient_frontier()` all default to `freq_data = "monthly"`,
> so the pattern above is safe out of the box. If you override `freq_data`
> in one of the calls, override it consistently in all three — a mismatch
> now raises a warning rather than silently producing a wrong annualized
> return.

Every `plot_*` and `calc_efficient_frontier()` function also accepts raw
`stock_data` directly and will compute what it needs internally — so the
package works fine for quick one-off exploration too. The point of the
pattern above is to avoid redundant recomputation, and to make it explicit
*which* covariance matrix, expected-return estimate, etc. is feeding into
each step of your analysis — important once you start comparing scenarios
(different risk-free rates, different date ranges) side by side.

## Function reference

**Data retrieval (`get_*`)**

- `get_stocks()` → Historical OHLCV data and log returns for B3-listed stocks, ETFs and BDRs
- `get_benchmark()` → Historical data for Ibovespa (`BR`) or S&P 500 (`US`); internally calls `get_stocks()`
- `get_risk_free()` → Daily CDI or SELIC series from the Central Bank of Brazil's public API

All `get_*` functions support flexible date inputs: `"YYYY-MM-DD"`, `Date`, or `NULL` (defaults to the last 365 days).

**Analytics (`calc_*`)**

- `calc_beta()` → Beta coefficient relative to a benchmark
- `calc_expected_return()` → Mean historical log return per ticker; aggregates to monthly returns by default (`freq_data = "monthly"`)
- `calc_variance()` → Return variance per ticker
- `calc_covariance_matrix()` → Pairwise covariance matrix; aggregates to monthly returns by default (`freq_data = "monthly"`)
- `calc_correlation_matrix()` → Pairwise Pearson correlation matrix; aggregates to monthly returns by default (`freq_data = "monthly"`)
- `calc_avg_risk_free()` → Average daily CDI/SELIC rate over a period
- `calc_efficient_frontier()` → Monte Carlo portfolio simulation (Markowitz, 1952); accepts precomputed `expected_returns`/`cov_matrix`; aggregates to monthly returns by default (`freq_data = "monthly"`)
- `calc_key_portfolios()` → Extracts the minimum-variance, maximum-Sharpe, and maximum-return portfolios from a simulated frontier

All `calc_*` functions above that take multi-asset `stock_data` — `calc_beta()`,
`calc_expected_return()`, `calc_variance()`, `calc_covariance_matrix()`,
`calc_correlation_matrix()`, and `calc_efficient_frontier()` — align the
input series internally via an `na_method` argument (`"intersection"` by
default, or `"pairwise"` / `"locf"`) before computing anything. See
[Date alignment across assets](#date-alignment-across-assets) below.

`calc_expected_return()`, `calc_covariance_matrix()`, `calc_correlation_matrix()`,
and `calc_efficient_frontier()` share a `freq_data` argument
(`"daily"` / `"weekly"` / `"monthly"`, default `"monthly"`) and attach it as
an attribute to their output. If you compose them manually — e.g. pass a
`calc_expected_return()` result into `calc_efficient_frontier()`'s
`expected_returns` argument — a mismatched `freq_data` between the two
calls now raises a warning instead of silently producing a wrong
annualized return. `calc_beta()` and `calc_variance()` still operate on
daily data only; they'll get the same `freq_data` argument in a future
update.

### Date alignment across assets

Assets can have different trading days for several reasons: local holidays
(B3 vs. NYSE), trading suspensions, recently launched ETFs, or missing
historical data for a given ticker. To keep every calculation consistent,
the functions listed above align the input series internally before
computing anything, controlled by `na_method`:

- `"intersection"` (default) — keeps only the dates where *all* selected
  assets have data. Safest option; may reduce the number of observations.
- `"pairwise"` — computes each pairwise statistic (e.g. each cell of a
  correlation matrix) over the dates common to that specific pair of
  assets, rather than the intersection across all assets at once.
- `"locf"` — last observation carried forward, filling gaps instead of
  dropping dates.

When observations are dropped, a warning reports how many were removed.

**Visualization (`plot_*`)**

- `plot_benchmark()` → Price or index chart with configurable moving average
- `plot_correlation_matrix()` → Correlation heatmap; accepts a precomputed matrix or raw stock data
- `plot_efficient_frontier()` → Efficient frontier colored by Sharpe ratio

## The interactive dashboard

brstocks ships with a Shiny dashboard that wraps the functions above into
an exploratory tool — pick tickers, simulate the efficient frontier, and
see portfolio composition without writing any code.

```r
brstocks::run_dashboard()
```

🔗 **Try it online without installing anything:** https://joaobarbosa.shinyapps.io/brstocks/

**Tabs:**

- **Efficient Frontier** — simulated portfolios, colored by Sharpe ratio, with the minimum-variance / maximum-Sharpe / maximum-return portfolios highlighted. Hover over any point to see its composition.
- **Correlation** — heatmap of pairwise return correlations.
- **Beta** — beta of each selected asset relative to the chosen benchmark.
- **Portfolio vs Benchmark** — cumulative return of a chosen key portfolio against the benchmark.
- **Contributions** — simulates periodic contributions (weekly/monthly/quarterly/semi-annual) into a chosen portfolio, compared against the same contributions made into the benchmark or into the CDI.
- **Learn** — a plain-language explainer of the efficient frontier and diversification, plus a reference table of common B3 assets across different asset classes (equities, ETFs tracking indices, fixed income, crypto, REITs/FIIs, BDRs).

### Model assumptions (read before drawing conclusions)

The dashboard makes several simplifying assumptions that are appropriate
for exploratory analysis but should not be mistaken for investment advice
or a production-grade backtest:

1. **Constant-mix rebalancing.** The "Efficient Frontier" and "Contributions"
   tabs assume the portfolio is rebalanced daily to keep fixed weights at
   the chosen point on the frontier. Real-world rebalancing at that
   frequency would incur brokerage costs and taxes that are not modeled
   here.
2. **Monte Carlo approximation, not an exact optimizer.** The frontier is
   built from randomly sampled portfolios (weights drawn from a
   Dirichlet(1, ..., 1) distribution — see `calc_efficient_frontier()`
   details), not a closed-form quadratic-programming solution. The
   "key portfolios" are the best approximations *found among the
   simulated set*, not the true mathematical optimum.
3. **CDI as the risk-free proxy**, fetched directly from the Central
   Bank's series rather than from a specific traded fund, to avoid
   contaminating the comparison with fund fees or tracking error.
4. **Annualized return in the "Contributions" tab is an approximation**
   (total return raised to `252 / trading_days_elapsed`), not a
   money-weighted internal rate of return (XIRR). It ignores the exact
   timing of individual contributions and should be read as an
   order-of-magnitude comparison, not an exact fund-style return figure.
5. **Log returns throughout**, annualized using 252 trading days per year
   (not 365 calendar days), consistent with standard quantitative-finance
   convention.
6. **Portfolio construction runs on monthly-aggregated returns by default.**
   `calc_expected_return()`, `calc_covariance_matrix()`,
   `calc_correlation_matrix()`, and `calc_efficient_frontier()` all default
   to `freq_data = "monthly"`, aggregating daily log returns into monthly
   ones (log returns are additive, so this is a simple sum) before
   computing anything. This reduces noise and produces more stable
   estimates than raw daily data — and mitigates the "Epps effect" (daily
   correlation between assets on different exchanges, e.g. B3 vs. NYSE,
   tends to be understated because information doesn't reach both markets
   instantly). Because a year of daily data only yields ~12 monthly
   observations, `run_dashboard()` defaults to a 5-year lookback (~60
   monthly observations) rather than 1 year — widen it further for more
   stable covariance estimates as you add more assets. `calc_beta()` and
   `calc_variance()` still operate on daily data only, for now.

## Data Source

Stock and benchmark data is retrieved from [Yahoo Finance](https://finance.yahoo.com/)
via the [yfR](https://github.com/ropensci/yfR) package (Perlin, 2022).
Risk-free rate data (CDI/SELIC) is retrieved from the
[Central Bank of Brazil's SGS API](https://www3.bcb.gov.br/sgspub/).

> **Note:** Yahoo Finance data is suitable for academic research and personal analysis.
> For production use in research papers or dissertations, consider professional
> data sources such as Economática or EODHD.

## References

- Markowitz, H. (1952). Portfolio Selection. *The Journal of Finance*, 7(1), 77–91.
- Sharpe, W. F. (1966). Mutual Fund Performance. *The Journal of Business*, 39(1), 119–138.
- Perlin, M. (2022). *yfR: Downloads and Organizes Financial Data from Yahoo Finance*. R package.
- Banco Central do Brasil. *Sistema Gerenciador de Séries Temporais (SGS)* — CDI (series 12) and SELIC (series 11). https://www3.bcb.gov.br/sgspub/

## Contribution

Suggestions, feature requests, and pull requests are welcome!

## Author

João Paulo dos Santos Pereira Barbosa — Economics student at PUC-Rio.
Also the author of [brfinance](https://github.com/efram2/brfinance), an R package for Brazilian macroeconomic data.
