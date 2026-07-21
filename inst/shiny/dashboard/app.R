# ============================================================
# app.R - brstocks Shiny Dashboard
# ============================================================

# Carregar pacotes necessários
devtools::load_all()
library(brstocks)
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)
library(scales)

# ---------------------------------------------------------------------
# Color palette. Chosen to evoke an institutional financial-market
# aesthetic (navy blue for trust/primary data, crimson for risk/alerts,
# gold for highlighted reference points) rather than default chart colors.
# ---------------------------------------------------------------------
COR_AZUL <- "#0B3D91"
COR_VERM <- "#C81E3A"
COR_OURO <- "#C9A227"

# ---------------------------------------------------------------------
# Footer constants.
# ---------------------------------------------------------------------
GITHUB_URL <- "https://github.com/efram2/brstocks"

SVG_LINKEDIN <- '<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path d="M20.45 20.45h-3.56v-5.57c0-1.33-.02-3.04-1.85-3.04-1.86 0-2.14 1.45-2.14 2.94v5.67H9.34V9h3.41v1.56h.05c.48-.9 1.64-1.85 3.38-1.85 3.6 0 4.27 2.37 4.27 5.46v6.28zM5.34 7.43a2.06 2.06 0 1 1 0-4.12 2.06 2.06 0 0 1 0 4.12zM7.12 20.45H3.56V9h3.56v11.45z"/></svg>'
SVG_GITHUB <- '<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path d="M12 2C6.48 2 2 6.58 2 12.21c0 4.5 2.87 8.32 6.84 9.67.5.1.68-.22.68-.49 0-.24-.01-.88-.01-1.72-2.78.62-3.37-1.37-3.37-1.37-.46-1.2-1.11-1.52-1.11-1.52-.91-.64.07-.63.07-.63 1 .07 1.53 1.05 1.53 1.05.89 1.56 2.34 1.11 2.91.85.09-.66.35-1.11.63-1.37-2.22-.26-4.56-1.14-4.56-5.07 0-1.12.39-2.03 1.03-2.75-.1-.26-.45-1.32.1-2.75 0 0 .84-.28 2.75 1.05a9.3 9.3 0 0 1 5 0c1.9-1.33 2.75-1.05 2.75-1.05.55 1.43.2 2.49.1 2.75.64.72 1.03 1.63 1.03 2.75 0 3.94-2.34 4.8-4.57 5.06.36.32.68.94.68 1.9 0 1.37-.01 2.47-.01 2.81 0 .27.18.6.69.49A10.02 10.02 0 0 0 22 12.21C22 6.58 17.52 2 12 2z"/></svg>'

# ---------------------------------------------------------------------
# Language dictionary. Provides a lightweight PT/EN toggle for dashboard
# labels, help text, and plot titles. Function and package names remain
# in English throughout, per standard R package convention; this
# dictionary governs only the user-facing dashboard layer.
# ---------------------------------------------------------------------
DIC <- list(
  pt = list(
    titulo_dashboard = "brstocks - Simulação de Fronteira Eficiente de Markowitz",
    tickers_label = "Ativos (separados por vírgula)",
    periodo_label = "Período",
    benchmark_label = "Benchmark",
    n_port_label = "Nº de carteiras simuladas",
    rf_label = "Taxa livre de risco",
    rf_manual_label = "Valor manual (freq. diária)",
    anualizar_label = "Anualizar retorno/risco (x252)",
    simular_label = "Simular carteira",
    hover_help = "Passe o mouse sobre qualquer ponto da fronteira para ver a composição daquela carteira.",
    carteiras_chave = "Carteiras-chave",
    comparar_label = "Comparar qual carteira?",
    titulo_fronteira = "Fronteira Eficiente",
    titulo_correlacao = "Matriz de Correlação",
    titulo_vs_bench = "Retorno Acumulado — Carteira vs. Benchmark",
    eixo_risco = "Risco", eixo_retorno = "Retorno",
    aprenda_titulo = "Guia rápido: o que simular",
    aprenda_intro = "Não sabe por onde começar? Aqui vão alguns ativos comuns pra testar no simulador:",
    col_ticker = "Ticker", col_oque = "O que é", col_categoria = "Categoria",
    markowitz_titulo = "O que é a Fronteira de Markowitz?",
    markowitz_texto = "A Fronteira Eficiente de Markowitz mostra, para cada nível de risco, a carteira com o maior retorno esperado possível combinando os ativos escolhidos. A ideia central da Teoria Moderna de Portfólio (Harry Markowitz, 1952) é que diversificar — combinar ativos que não se movem exatamente da mesma forma — reduz o risco total da carteira sem necessariamente reduzir o retorno esperado. Isso acontece porque, quando um ativo cai, outro pode subir ou ficar estável, suavizando as oscilações do conjunto. Cada ponto na nuvem da aba \"Efficient Frontier\" é uma carteira simulada com pesos diferentes entre os ativos; o objetivo é ficar perto da borda superior esquerda da nuvem — onde, para o risco assumido, o retorno esperado é o maior possível.",
    etf_titulo = "O que é um ETF?",
    etf_texto = "Um ETF (Exchange Traded Fund, ou Fundo de Índice) é negociado na bolsa como uma ação comum, mas seu objetivo é replicar o desempenho de um índice de referência, em vez de representar uma empresa só. Por isso, os ativos marcados como ETF na tabela abaixo estão ligados a um índice específico — o Ibovespa, o S&P 500, o preço do ouro, um índice global de ações etc.",
    pressupostos_titulo = "Pressupostos do modelo (leia antes de tirar conclusões)",
    pressupostos_itens = c(
      "Rebalanceamento diário assumido: a simulação mantém os pesos fixos como se a carteira fosse rebalanceada todo dia útil — na prática isso geraria custos de corretagem e impostos não considerados aqui.",
      "A fronteira é uma aproximação por simulação (Monte Carlo), não um otimizador exato — os pontos de mínima variância / máximo Sharpe / máximo retorno são os melhores encontrados entre as carteiras simuladas, não o ótimo matemático.",
      "O CDI é usado como proxy da taxa livre de risco, obtido direto do Banco Central — não de um fundo específico, para não misturar taxa de administração ou erro de rastreamento na comparação.",
      "O retorno anualizado do Simulador de Aportes é uma aproximação (não uma TIR/XIRR ponderada pelo momento de cada aporte) — serve como ordem de grandeza, não como retorno exato de fundo.",
      "Todos os cálculos usam retornos logarítmicos, anualizados por 252 dias úteis.",
      "Por padrão, os cálculos rodam sobre dados mensais, não diários — isso reduz ruído e atenua distorções na correlação entre ativos de mercados com calendários diferentes (ex.: B3 vs. NYSE).",
      "O período padrão do simulador é de 5 anos — um ano de dados diários vira só ~12 observações mensais, pouco para uma matriz de covariância estável."
    ),
    aportes_titulo_aba = "Simulador de Aportes",
    carteira_aportes_label = "Simular aportes em qual carteira?",
    aporte_inicial_label = "Aporte inicial (R$)",
    aporte_periodico_label = "Valor do aporte periódico (R$)",
    periodicidade_label = "Periodicidade dos aportes",
    periodicidade_semanal = "Semanal", periodicidade_mensal = "Mensal",
    periodicidade_trimestral = "Trimestral", periodicidade_semestral = "Semestral",
    simular_aportes_label = "Simular aportes",
    titulo_aportes = "Evolução do patrimônio com aportes",
    legenda_saldo = "Patrimônio", legenda_investido = "Total investido",
    resumo_investido = "Total investido", resumo_saldo = "Patrimônio final", resumo_ganho = "Ganho",
    aviso_rebalanceamento = "Hipótese do modelo: a simulação assume que a carteira é rebalanceada diariamente para manter os pesos fixos no ponto escolhido (mínima variância / máximo Sharpe / máximo retorno). Na prática isso exigiria comprar e vender ativos todo dia útil, o que gera custos de corretagem e impostos não considerados aqui.",
    legenda_carteira = "Sua carteira", legenda_benchmark = "Benchmark (mesmos aportes)", legenda_cdi = "CDI (mesmos aportes)",
    col_serie = "Onde investiu", col_ganho_nominal = "Ganho (R$)", col_ganho_pct = "Ganho total (%)", col_ganho_anual = "Retorno anualizado (%)",
    aviso_anualizacao = "O retorno anualizado aqui é uma aproximação simples (ganho total elevado a 252/nº de dias úteis do período) — não é uma taxa interna de retorno (XIRR) ponderada pelo momento exato de cada aporte. Serve como ordem de grandeza pra comparar as três opções, não como retorno exato de fundo.",
    observacao_alinhamento_titulo = "Sobre os dados e alinhamento",
    observacao_alinhamento_texto = "Os ativos podem ter diferentes dias de negociação por diversos motivos: feriados locais (B3 vs. NYSE), suspensões de negociação, lançamentos recentes de ETF ou indisponibilidade de dados históricos. Por exemplo, um ETF como o IVVB11 (que replica o S&P 500) pode ter mais dias de negociação que uma ação brasileira, pois o mercado dos EUA tem menos feriados que o Brasil. Por outro lado, um ETF lançado recentemente pode ter menos dados históricos disponíveis. Para garantir que todos os cálculos (correlação, covariância, fronteira eficiente e beta) sejam feitos sobre o mesmo período, o pacote alinha automaticamente as séries, mantendo apenas as datas em que todos os ativos selecionados possuem dados disponíveis. Isso garante consistência e precisão nos resultados apresentados — ainda que possa reduzir o número total de observações disponíveis.",
    rodape_autor = "João Paulo Barbosa",
    rodape_versao = "Versão do pacote",
    rodape_fonte = "Dados: Yahoo Finance via yfR"
  ),
  en = list(
    titulo_dashboard = "brstocks - Markowitz Efficient Frontier Simulation",
    tickers_label = "Assets (comma-separated)",
    periodo_label = "Date range",
    benchmark_label = "Benchmark",
    n_port_label = "Number of simulated portfolios",
    rf_label = "Risk-free rate",
    rf_manual_label = "Manual value (daily freq.)",
    anualizar_label = "Annualize return/risk (x252)",
    simular_label = "Simulate portfolio",
    hover_help = "Hover over any point on the frontier to see that portfolio's composition.",
    carteiras_chave = "Key portfolios",
    comparar_label = "Compare which portfolio?",
    titulo_fronteira = "Efficient Frontier",
    titulo_correlacao = "Correlation Matrix",
    titulo_vs_bench = "Cumulative Return — Portfolio vs. Benchmark",
    eixo_risco = "Risk", eixo_retorno = "Return",
    aprenda_titulo = "Quick guide: what to simulate",
    aprenda_intro = "Not sure where to start? Here are some common assets to try in the simulator:",
    col_ticker = "Ticker", col_oque = "What it is", col_categoria = "Category",
    markowitz_titulo = "What is the Markowitz Frontier?",
    markowitz_texto = "The Markowitz Efficient Frontier shows, for each level of risk, the portfolio with the highest possible expected return combining the chosen assets. The core idea of Modern Portfolio Theory (Harry Markowitz, 1952) is that diversifying — combining assets that don't move in exactly the same way — reduces a portfolio's total risk without necessarily reducing its expected return. This happens because when one asset drops, another may rise or hold steady, smoothing out the swings of the combined portfolio. Every point in the cloud on the \"Efficient Frontier\" tab is a simulated portfolio with different weights across assets; the goal is to sit near the upper-left edge of the cloud, where expected return is the highest possible for the risk taken on.",
    etf_titulo = "What is an ETF?",
    etf_texto = "An ETF (Exchange Traded Fund) trades on the exchange like a regular stock, but its goal is to replicate the performance of a reference index rather than represent a single company. That's why the assets marked as ETFs in the table below are tied to a specific index — the Ibovespa, the S&P 500, the price of gold, a global equity index, and so on.",
    pressupostos_titulo = "Model assumptions (read before drawing conclusions)",
    pressupostos_itens = c(
      "Daily rebalancing assumed: the simulation keeps weights fixed as if the portfolio were rebalanced every trading day -- in practice this would incur brokerage costs and taxes not modeled here.",
      "The frontier is a Monte Carlo approximation, not an exact optimizer -- the minimum-variance / maximum-Sharpe / maximum-return points are the best found among the simulated portfolios, not the true mathematical optimum.",
      "CDI is used as the risk-free proxy, fetched directly from the Central Bank -- not from a specific fund, to avoid mixing in management fees or tracking error.",
      "The annualized return in the Contributions Simulator is an approximation (not a money-weighted IRR/XIRR) -- it's an order-of-magnitude comparison, not an exact fund-style return.",
      "All calculations use log returns, annualized using 252 trading days.",
      "By default, calculations run on monthly data, not daily -- this reduces noise and mitigates correlation distortions between assets on markets with different trading calendars (e.g. B3 vs. NYSE).",
      "The simulator's default date range is 5 years -- a year of daily data only yields ~12 monthly observations, too few for a stable covariance matrix."
    ),
    aportes_titulo_aba = "Contribution Simulator",
    carteira_aportes_label = "Simulate contributions into which portfolio?",
    aporte_inicial_label = "Initial contribution (R$)",
    aporte_periodico_label = "Recurring contribution amount (R$)",
    periodicidade_label = "Contribution frequency",
    periodicidade_semanal = "Weekly", periodicidade_mensal = "Monthly",
    periodicidade_trimestral = "Quarterly", periodicidade_semestral = "Semi-annual",
    simular_aportes_label = "Simulate contributions",
    titulo_aportes = "Portfolio growth with contributions",
    legenda_saldo = "Balance", legenda_investido = "Total contributed",
    resumo_investido = "Total contributed", resumo_saldo = "Final balance", resumo_ganho = "Gain",
    aviso_rebalanceamento = "Model assumption: the simulation assumes the portfolio is rebalanced daily to keep fixed weights at the chosen point (min variance / max Sharpe / max return). In practice this would require buying and selling assets every trading day, incurring brokerage costs and taxes not modeled here.",
    legenda_carteira = "Your portfolio", legenda_benchmark = "Benchmark (same contributions)", legenda_cdi = "CDI (same contributions)",
    col_serie = "Invested in", col_ganho_nominal = "Gain (R$)", col_ganho_pct = "Total gain (%)", col_ganho_anual = "Annualized return (%)",
    aviso_anualizacao = "The annualized return here is a simple approximation (total gain raised to 252/number of trading days in the period) -- it is not a money-weighted internal rate of return (XIRR) accounting for the exact timing of each contribution. Use it as an order-of-magnitude comparison across the three options, not as an exact fund return.",
    observacao_alinhamento_titulo = "About the data and alignment",
    observacao_alinhamento_texto = "Assets may have different trading days for several reasons: local holidays (B3 vs. NYSE), trading suspensions, recently launched ETFs, or unavailable historical data. For example, an ETF like IVVB11 (which tracks the S&P 500) may have more trading days than a Brazilian stock, as the US market has fewer holidays than Brazil. On the other hand, a recently launched ETF may have less historical data available. To ensure that all calculations (correlation, covariance, efficient frontier, and beta) are performed over the same period, the package automatically aligns the series, keeping only the dates where all selected assets have available data. This ensures consistency and accuracy in the results presented — even though it may reduce the total number of available observations.",
    rodape_autor = "João Paulo Barbosa",
    rodape_versao = "Package version",
    rodape_fonte = "Data: Yahoo Finance via yfR"
  )
)

# ---------------------------------------------------------------------
# Example asset table for the Learn tab. Tickers and descriptions were
# verified against public B3/issuer sources as of the time of writing.
# NCDI11 removed because it has no historical data available.
# ---------------------------------------------------------------------
TABELA_APRENDA <- dplyr::tribble(
  ~ticker,   ~pt,                                                 ~en,                                              ~categoria_pt,        ~categoria_en,
  "PETR4",   "Petrobras (ação preferencial)",                     "Petrobras (preferred shares)",                   "Petróleo e gás",      "Oil & gas",
  "VALE3",   "Vale (mineração)",                                  "Vale (mining)",                                  "Commodities",         "Commodities",
  "ITUB4",   "Itaú Unibanco",                                     "Itaú Unibanco",                                  "Bancos",              "Banks",
  "BBDC4",   "Bradesco",                                          "Bradesco",                                       "Bancos",              "Banks",
  "ABEV3",   "Ambev",                                             "Ambev",                                          "Consumo/bebidas",     "Consumer staples",
  "WEGE3",   "WEG (motores e equipamentos industriais)",          "WEG (industrial motors & equipment)",            "Industrial",          "Industrial",
  "BOVA11",  "ETF que replica o Ibovespa",                        "ETF tracking the Ibovespa index",                "Índice BR",           "BR index",
  "IVVB11",  "ETF que replica o S&P 500, listado na B3",          "ETF tracking the S&P 500, listed on B3",         "Índice EUA",          "US index",
  "GOLD11",  "ETF que replica o preço do ouro (LBMA Gold Price)", "ETF tracking the gold price (LBMA Gold Price)",  "Proteção/commodity",  "Safe-haven/commodity",
  "WRLD11",  "ETF que replica um índice global de ações (FTSE Global All Cap, mercados desenvolvidos e emergentes)", "ETF tracking a global equity index (FTSE Global All Cap, developed and emerging markets)", "Índice Mundial", "World index",
  "HASH11",  "ETF que replica uma cesta diversificada de criptoativos (Nasdaq Crypto Index)", "ETF tracking a diversified basket of crypto assets (Nasdaq Crypto Index)", "Criptoativos", "Crypto assets",
  "QBTC11",  "ETF com exposição 100% a Bitcoin",                                       "ETF with 100% Bitcoin exposure",                                            "Criptoativos", "Crypto assets",
  "KNRI11",  "Fundo Imobiliário (FII) — cotas de imóveis comerciais/logísticos, negociado como ação", "Real Estate Fund (FII) — shares of commercial/logistics properties, traded like a stock", "Fundos Imobiliários", "Real estate funds",
  "AAPL34",  "BDR da Apple — recibo negociado na B3 que espelha a ação americana",     "Apple BDR — a B3-traded receipt mirroring the US stock",                   "BDR (ações estrangeiras)", "BDR (foreign stocks)"
)

# ---------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------
ui <- page_sidebar(
  title = div(
    style = "display:flex; justify-content:space-between; align-items:center; width:100%; gap: 12px;",
    span(textOutput("titulo_app", inline = TRUE), style = "font-weight:600;"),
    radioButtons("idioma", NULL, choices = c("Portugues" = "pt", "English" = "en"),
                 selected = "pt", inline = TRUE)
  ),
  theme = bs_theme(version = 5, bg = "#FFFFFF", fg = "#1A1A2E",
                   primary = COR_AZUL, danger = COR_VERM, warning = COR_OURO,
                   base_font = font_google("Inter")),

  sidebar = sidebar(
    width = 320, class = "compact-sidebar",
    textInput("tickers", "Ativos (separados por vírgula)", value = "PETR4, VALE3, ITUB4, BBDC4"),
    uiOutput("aviso_muitos_ativos"),
    dateRangeInput("datas", "Período", start = Sys.Date() - 365 * 5, end = Sys.Date()),
    selectInput("benchmark_mercado", "Benchmark",
                choices = c("Ibovespa" = "BR", "S&P 500" = "US"), selected = "BR"),
    numericInput("n_portfolios", "Nº de carteiras simuladas", value = 3000, min = 500, step = 500),
    selectInput("tipo_rf", "Taxa livre de risco",
                choices = c("CDI" = "cdi", "SELIC" = "selic", "Zero (manual)" = "zero")),
    conditionalPanel(
      "input.tipo_rf == 'zero'",
      numericInput("risk_free_manual", "Valor manual (freq. diária)", value = 0, step = 0.0001)
    ),
    checkboxInput("anualizar", "Anualizar retorno/risco (x252)", value = TRUE),
    actionButton("simular", "Simular carteira", class = "btn-primary"),
    hr(),
    uiOutput("hover_help_txt")
  ),

  tags$style(HTML("
    .tab-content { margin-top: 20px; }
    .compact-sidebar .form-group { margin-bottom: 10px; }
    .compact-sidebar .shiny-html-output:empty { display: none; margin: 0; padding: 0; }
    .text-justify { text-align: justify; }
    .app-footer {
      margin-top: 40px;
      padding: 14px 4px 6px;
      border-top: 1px solid #E5E5E5;
      display: flex;
      flex-wrap: wrap;
      align-items: center;
      gap: 6px 16px;
      font-size: 0.78em;
      color: #6C757D;
    }
    .app-footer .footer-spacer { flex-grow: 1; }
    .footer-icon-link { color: #6C757D; display: inline-flex; align-items: center; }
    .footer-icon-link:hover { color: #0B3D91; }
    .footer-icon-link svg { width: 15px; height: 15px; fill: currentColor; }
  ")),

  navset_tab(
    nav_panel(
      "Efficient Frontier",
      plotlyOutput("plot_fronteira", height = "600px"),
      br(),
      h5(textOutput("titulo_key")),
      DTOutput("tabela_key")
    ),
    nav_panel("Correlation", plotlyOutput("plot_correlacao", height = "420px")),
    nav_panel("Beta", DTOutput("tabela_beta")),
    nav_panel(
      "Portfolio vs Benchmark",
      selectInput("carteira_escolhida", "Comparar qual carteira?",
                  choices = c("Máximo Sharpe" = "Maximo Sharpe",
                              "Mínima Variância" = "Minima Variancia",
                              "Máximo Retorno" = "Maximo Retorno")),
      plotlyOutput("plot_vs_benchmark", height = "550px")
    ),
    nav_panel(
      "Contributions",
      uiOutput("aviso_rebalanceamento"),
      fluidRow(
        column(6, selectInput("carteira_aportes", "Simular aportes em qual carteira?",
                              choices = c("Máximo Sharpe" = "Maximo Sharpe",
                                          "Mínima Variância" = "Minima Variancia",
                                          "Máximo Retorno" = "Maximo Retorno"))),
        column(6, selectInput("periodicidade", "Periodicidade dos aportes",
                              choices = c("Semanal" = "semanal", "Mensal" = "mensal",
                                          "Trimestral" = "trimestral", "Semestral" = "semestral"),
                              selected = "mensal"))
      ),
      fluidRow(
        column(6, numericInput("aporte_inicial", "Aporte inicial (R$)", value = 1000, min = 0, step = 100)),
        column(6, numericInput("aporte_periodico", "Valor do aporte periódico (R$)", value = 200, min = 0, step = 50))
      ),
      div(style = "text-align:center; margin: 15px 0;",
          actionButton("simular_aportes", "Simular aportes", class = "btn-primary")),
      plotlyOutput("plot_aportes", height = "450px"),
      uiOutput("resumo_aportes")
    ),
    nav_panel(
      "Learn",
      uiOutput("texto_markowitz"),
      hr(),
      uiOutput("cabecalho_aprenda"),
      DTOutput("tabela_aprenda"),
      hr(),
      div(
        class = "alert alert-secondary",
        style = "font-size: 0.9em; margin-top: 20px;",
        uiOutput("observacao_alinhamento")
      ),
      div(
        class = "alert alert-secondary",
        style = "font-size: 0.85em; margin-top: 12px;",
        uiOutput("pressupostos_modelo")
      )
    )
  ),

  uiOutput("rodape")
)

# ---------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------
server <- function(input, output, session) {

  dic <- reactive(DIC[[input$idioma]])

  # Update input labels when the language toggle changes.
  # Function and package names remain in English; only UI text is affected.
  observeEvent(input$idioma, {
    d <- dic()
    updateTextInput(session, "tickers", label = d$tickers_label)
    updateDateRangeInput(session, "datas", label = d$periodo_label)
    updateSelectInput(session, "benchmark_mercado", label = d$benchmark_label)
    updateNumericInput(session, "n_portfolios", label = d$n_port_label)
    updateSelectInput(session, "tipo_rf", label = d$rf_label)
    updateNumericInput(session, "risk_free_manual", label = d$rf_manual_label)
    updateCheckboxInput(session, "anualizar", label = d$anualizar_label)
    updateActionButton(session, "simular", label = d$simular_label)
    updateSelectInput(session, "carteira_escolhida", label = d$comparar_label)
    updateSelectInput(session, "carteira_aportes", label = d$carteira_aportes_label)
    updateNumericInput(session, "aporte_inicial", label = d$aporte_inicial_label)
    updateNumericInput(session, "aporte_periodico", label = d$aporte_periodico_label)
    updateSelectInput(session, "periodicidade", label = d$periodicidade_label,
                      choices = c(setNames("semanal", d$periodicidade_semanal),
                                  setNames("mensal", d$periodicidade_mensal),
                                  setNames("trimestral", d$periodicidade_trimestral),
                                  setNames("semestral", d$periodicidade_semestral)),
                      selected = input$periodicidade)
    updateActionButton(session, "simular_aportes", label = d$simular_aportes_label)
  }, ignoreInit = TRUE)

  output$hover_help_txt  <- renderUI(helpText(dic()$hover_help))
  output$titulo_key <- renderText(dic()$carteiras_chave)
  output$titulo_app <- renderText(dic()$titulo_dashboard)

  # ---- Footer: minimal, always visible at the end of the page content ----
  output$rodape <- renderUI({
    d <- dic()
    versao_pkg <- tryCatch(as.character(utils::packageVersion("brstocks")),
                           error = function(e) "dev")
    tags$footer(
      class = "app-footer",
      span(d$rodape_autor),
      tags$a(href = "https://www.linkedin.com/in/jpbarbosa-econ", target = "_blank",
             rel = "noopener noreferrer", title = "LinkedIn", class = "footer-icon-link",
             HTML(SVG_LINKEDIN)),
      tags$a(href = GITHUB_URL, target = "_blank", rel = "noopener noreferrer",
             title = "GitHub", class = "footer-icon-link", HTML(SVG_GITHUB)),
      span(class = "footer-spacer"),
      span(paste0(d$rodape_versao, ": ", versao_pkg)),
      span(d$rodape_fonte)
    )
  })

  output$aviso_muitos_ativos <- renderUI({
    n_tickers <- length(trimws(strsplit(input$tickers, ",")[[1]]))
    if (n_tickers > 10) {
      msg <- if (input$idioma == "pt")
        paste0("Você digitou ", n_tickers, " ativos. Acima de ~10, tabelas e legendas mostram só os principais pesos por carteira -- a fronteira em si continua correta.")
      else
        paste0("You entered ", n_tickers, " assets. Above ~10, tables and legends show only the top weights per portfolio -- the frontier itself remains correct.")
      div(class = "alert alert-secondary", style = "font-size: 0.8em; padding: 6px;", msg)
    }
  })

  # ---- Portfolio data: assets, risk-free rate, efficient frontier, key
  # portfolios. Depends only on tickers/dates/n_portfolios/risk-free/
  # annualize -- deliberately NEVER on the benchmark selector, so switching
  # benchmarks cannot change the frontier or the Sharpe ratio.
  dados_portfolio <- eventReactive(input$simular, {
    tickers_vec <- trimws(strsplit(input$tickers, ",")[[1]])
    validate(need(length(tickers_vec) >= 2, "Informe pelo menos 2 ativos. / Enter at least 2 assets."))

    withProgress(message = "Simulando...", value = 0, {
      incProgress(0.2); acoes <- get_stocks(tickers_vec, from = input$datas[1], to = input$datas[2])
      incProgress(0.2)
      rf <- if (input$tipo_rf == "zero") input$risk_free_manual else
        calc_avg_risk_free(rate = input$tipo_rf, from = input$datas[1], to = input$datas[2])
      incProgress(0.3)
      fronteira <- calc_efficient_frontier(acoes, n_portfolios = input$n_portfolios,
                                           risk_free = rf, annualize = input$anualizar)
      incProgress(0.15); key <- calc_key_portfolios(fronteira)
      incProgress(0.15)
      list(acoes = acoes, fronteira = fronteira, key = key)
    })
  })

  # ---- Benchmark data: downloaded independently of the portfolio/frontier.
  # Reacts automatically to the benchmark selector (no button click needed)
  # once a portfolio has been simulated. Feeds ONLY the beta table and the
  # "Portfolio vs Benchmark" chart -- never the frontier or Sharpe ratio.
  dados_benchmark <- reactive({
    req(dados_portfolio())
    bench <- get_benchmark(market = input$benchmark_mercado,
                           from = input$datas[1], to = input$datas[2])
    beta_tbl <- calc_beta(dados_portfolio()$acoes, bench)
    list(bench = bench, beta = beta_tbl)
  })

  output$plot_fronteira <- renderPlotly({
    req(dados_portfolio())
    d <- dic()
    fr  <- dados_portfolio()$fronteira
    key <- dados_portfolio()$key |>
      mutate(simbolo = c("Minima Variancia" = "triangle-up",
                         "Maximo Sharpe"    = "star",
                         "Maximo Retorno"   = "square")[tipo])

    fr$hover_txt <- purrr::pmap_chr(
      list(fr$retorno, fr$risco, fr$sharpe, fr$pesos),
      function(ret, risk, sharpe, w) {
        top5 <- sort(w, decreasing = TRUE)[seq_len(min(5, length(w)))]
        paste0(d$eixo_retorno, ": ", percent(ret, accuracy = 0.01), "<br>",
               d$eixo_risco, ": ", percent(risk, accuracy = 0.01), "<br>",
               "Sharpe: ", round(sharpe, 2), "<br><br>",
               paste(names(top5), percent(as.numeric(top5), accuracy = 0.1), sep = ": ", collapse = "<br>"))
      }
    )

    plot_ly() |>
      add_trace(data = fr, x = ~risco, y = ~retorno, type = "scattergl", mode = "markers",
                marker = list(color = ~sharpe, colorscale = list(c(0, COR_VERM), c(1, COR_AZUL)),
                              size = 5, opacity = 0.45, showscale = TRUE, colorbar = list(title = "Sharpe")),
                text = ~hover_txt, hoverinfo = "text") |>
      add_trace(data = key, x = ~risco, y = ~retorno, type = "scatter", mode = "markers",
                marker = list(color = COR_OURO, size = 15, symbol = ~simbolo, line = list(color = "#1A1A2E", width = 1)),
                text = ~tipo, hoverinfo = "text") |>
      layout(title = d$titulo_fronteira,
             xaxis = list(title = d$eixo_risco, tickformat = ".1%"),
             yaxis = list(title = d$eixo_retorno, tickformat = ".1%"),
             plot_bgcolor = "#FAFAFA", paper_bgcolor = "#FFFFFF", showlegend = FALSE)
  })

  output$tabela_key <- renderDT({
    req(dados_portfolio())
    tabela <- dados_portfolio()$key |>
      rowwise() |>
      mutate(
        retorno = percent(retorno, accuracy = 0.01), risco = percent(risco, accuracy = 0.01),
        sharpe = round(sharpe, 2),
        pesos_desc = {
          ordenados <- sort(pesos, decreasing = TRUE)
          principais <- ordenados[seq_len(min(5, length(ordenados)))]
          texto <- paste(names(principais), percent(as.numeric(principais), accuracy = 0.1), sep = ": ", collapse = " | ")
          restantes <- length(ordenados) - length(principais)
          if (restantes > 0) texto <- paste0(texto, " | +", restantes, if (input$idioma == "pt") " outros" else " others")
          texto
        }
      ) |>
      ungroup() |> select(tipo, retorno, risco, sharpe, pesos_desc)
    datatable(tabela, options = list(dom = "t", pageLength = 5, ordering = FALSE), rownames = FALSE,
              selection = "none",
              colnames = c("Carteira", "Retorno", "Risco", "Sharpe", "Composição (top 5)"))
  })

  # Correlation heatmap, rendered with plotly for visual consistency with
  # the remaining plots in this dashboard.
  output$plot_correlacao <- renderPlotly({
    req(dados_portfolio())
    d <- dic()
    cor_mat <- suppressWarnings(calc_correlation_matrix(dados_portfolio()$acoes))

    plot_ly(
      x = colnames(cor_mat), y = rownames(cor_mat), z = cor_mat, type = "heatmap",
      colorscale = list(c(0, COR_VERM), c(0.5, "#FFFFFF"), c(1, COR_AZUL)),
      zmin = -1, zmax = 1,
      text = round(cor_mat, 2), texttemplate = "%{text}", hoverongaps = FALSE
    ) |>
      layout(title = d$titulo_correlacao, plot_bgcolor = "#FAFAFA", paper_bgcolor = "#FFFFFF",
             margin = list(t = 60, b = 40)) |>
      config(displayModeBar = FALSE)
  })

  output$tabela_beta <- renderDT({
    req(dados_benchmark())
    datatable(dados_benchmark()$beta, options = list(pageLength = 10, ordering = FALSE), rownames = FALSE,
              selection = "none")
  })

  output$plot_vs_benchmark <- renderPlotly({
    req(dados_portfolio(), dados_benchmark())
    d <- dic()
    pesos_sel <- dados_portfolio()$key |> filter(tipo == input$carteira_escolhida) |> pull(pesos) |> (\(x) x[[1]])()

    retornos_wide <- dados_portfolio()$acoes |>
      filter(ticker %in% names(pesos_sel), !is.na(ret_adjusted_prices)) |>
      select(ticker, ref_date, ret_adjusted_prices) |>
      pivot_wider(names_from = ticker, values_from = ret_adjusted_prices) |>
      arrange(ref_date) |> tidyr::drop_na()

    matriz <- as.matrix(retornos_wide[, names(pesos_sel)])
    ret_carteira <- as.numeric(matriz %*% pesos_sel[colnames(matriz)])

    carteira_df <- dplyr::tibble(ref_date = retornos_wide$ref_date,
                                 valor = exp(cumsum(ret_carteira)), serie = "Carteira")
    bench_df <- dados_benchmark()$bench |> filter(ref_date >= min(carteira_df$ref_date)) |>
      transmute(ref_date, valor = cumret_adjusted_prices, serie = "Benchmark")

    plot_ly(bind_rows(carteira_df, bench_df), x = ~ref_date, y = ~valor, color = ~serie,
            colors = c("Carteira" = COR_AZUL, "Benchmark" = COR_VERM),
            type = "scatter", mode = "lines") |>
      layout(title = d$titulo_vs_bench, xaxis = list(title = ""),
             yaxis = list(title = paste0(d$eixo_retorno, " (base = 1)")),
             plot_bgcolor = "#FAFAFA", paper_bgcolor = "#FFFFFF")
  })

  # ---- Contribution simulator ----
  # fator_diario: the day's growth multiplier. Use exp(log_return) for
  # equities/benchmark series, or (1 + rate) for a simple periodic rate
  # such as the CDI.
  simular_aportes_fn <- function(datas, fator_diario, aporte_inicial, aporte_periodico, periodicidade) {
    id_periodo <- switch(periodicidade,
                         semanal    = format(datas, "%Y-%U"),
                         mensal     = format(datas, "%Y-%m"),
                         trimestral = paste0(format(datas, "%Y"), "-T", ceiling(as.numeric(format(datas, "%m")) / 3)),
                         semestral  = paste0(format(datas, "%Y"), "-S", ceiling(as.numeric(format(datas, "%m")) / 6))
    )

    n <- length(datas)
    saldo <- numeric(n); investido <- numeric(n)
    saldo_atual <- aporte_inicial; total_investido <- aporte_inicial
    periodo_anterior <- id_periodo[1]

    for (i in seq_len(n)) {
      if (i > 1 && id_periodo[i] != periodo_anterior) {
        saldo_atual <- saldo_atual + aporte_periodico
        total_investido <- total_investido + aporte_periodico
        periodo_anterior <- id_periodo[i]
      }
      saldo_atual <- saldo_atual * fator_diario[i]
      saldo[i] <- saldo_atual
      investido[i] <- total_investido
    }

    dplyr::tibble(ref_date = datas, saldo = saldo, investido = investido)
  }

  aportes_dados <- eventReactive(input$simular_aportes, {
    req(dados_portfolio(), dados_benchmark())
    pesos_sel <- dados_portfolio()$key |> filter(tipo == input$carteira_aportes) |> pull(pesos) |> (\(x) x[[1]])()

    retornos_wide <- dados_portfolio()$acoes |>
      filter(ticker %in% names(pesos_sel), !is.na(ret_adjusted_prices)) |>
      select(ticker, ref_date, ret_adjusted_prices) |>
      pivot_wider(names_from = ticker, values_from = ret_adjusted_prices) |>
      arrange(ref_date) |> tidyr::drop_na()

    matriz <- as.matrix(retornos_wide[, names(pesos_sel)])
    ret_carteira <- as.numeric(matriz %*% pesos_sel[colnames(matriz)])

    bench_ret <- dados_benchmark()$bench |>
      filter(!is.na(ret_adjusted_prices)) |>
      select(ref_date, ret_bench = ret_adjusted_prices)

    cdi_serie <- get_risk_free("cdi", from = input$datas[1], to = input$datas[2]) |>
      rename(taxa_cdi = taxa)

    base <- dplyr::tibble(ref_date = retornos_wide$ref_date, ret_carteira = ret_carteira) |>
      dplyr::inner_join(cdi_serie, by = "ref_date") |>
      dplyr::left_join(bench_ret, by = "ref_date") |>
      dplyr::mutate(ret_bench = tidyr::replace_na(ret_bench, 0)) |>
      dplyr::arrange(ref_date)

    validate(need(nrow(base) > 1, "Não há datas em comum suficientes entre carteira e CDI."))

    carteira_sim  <- simular_aportes_fn(base$ref_date, exp(base$ret_carteira),
                                        input$aporte_inicial, input$aporte_periodico, input$periodicidade)
    benchmark_sim <- simular_aportes_fn(base$ref_date, exp(base$ret_bench),
                                        input$aporte_inicial, input$aporte_periodico, input$periodicidade)
    cdi_sim       <- simular_aportes_fn(base$ref_date, 1 + base$taxa_cdi,
                                        input$aporte_inicial, input$aporte_periodico, input$periodicidade)

    list(carteira = carteira_sim, benchmark = benchmark_sim, cdi = cdi_sim)
  })

  moeda_br <- scales::label_currency(prefix = "R$ ", big.mark = ".", decimal.mark = ",", accuracy = 1)

  output$aviso_rebalanceamento <- renderUI({
    div(class = "alert alert-warning text-justify", style = "font-size: 0.9em;", dic()$aviso_rebalanceamento)
  })

  output$plot_aportes <- renderPlotly({
    req(aportes_dados())
    d <- dic(); res <- aportes_dados()

    plot_ly(res$carteira, x = ~ref_date) |>
      add_trace(y = ~res$carteira$saldo, type = "scatter", mode = "lines", name = d$legenda_carteira,
                line = list(color = COR_AZUL, width = 2)) |>
      add_trace(y = ~res$benchmark$saldo, type = "scatter", mode = "lines", name = d$legenda_benchmark,
                line = list(color = COR_VERM, width = 2)) |>
      add_trace(y = ~res$cdi$saldo, type = "scatter", mode = "lines", name = d$legenda_cdi,
                line = list(color = "#6C757D", width = 2)) |>
      add_trace(y = ~res$carteira$investido, type = "scatter", mode = "lines", name = d$legenda_investido,
                line = list(color = COR_OURO, width = 2, dash = "dot")) |>
      layout(title = d$titulo_aportes, xaxis = list(title = ""),
             yaxis = list(title = "R$"),
             plot_bgcolor = "#FAFAFA", paper_bgcolor = "#FFFFFF")
  })

  output$resumo_aportes <- renderUI({
    req(aportes_dados())
    d <- dic()
    tagList(DTOutput("tabela_comparacao_aportes"), div(class = "alert alert-secondary text-justify", style = "font-size: 0.85em; margin-top:10px;", d$aviso_anualizacao))
  })

  output$tabela_comparacao_aportes <- renderDT({
    req(aportes_dados())
    d <- dic(); res <- aportes_dados()
    n_obs <- nrow(res$carteira)

    calc_linha <- function(df, nome) {
      final <- tail(df, 1)
      ganho_nominal <- final$saldo - final$investido
      ganho_pct <- final$saldo / final$investido - 1
      ganho_anual_pct <- (final$saldo / final$investido)^(252 / n_obs) - 1
      dplyr::tibble(Serie = nome, ganho_nominal = ganho_nominal,
                    ganho_pct = ganho_pct, ganho_anual_pct = ganho_anual_pct)
    }

    tabela <- dplyr::bind_rows(
      calc_linha(res$carteira, d$legenda_carteira),
      calc_linha(res$benchmark, d$legenda_benchmark),
      calc_linha(res$cdi, d$legenda_cdi)
    ) |>
      mutate(ganho_nominal = moeda_br(ganho_nominal),
             ganho_pct = percent(ganho_pct, accuracy = 0.1),
             ganho_anual_pct = percent(ganho_anual_pct, accuracy = 0.1))

    datatable(tabela, options = list(dom = "t", ordering = FALSE), rownames = FALSE, selection = "none",
              colnames = c(d$col_serie, d$col_ganho_nominal, d$col_ganho_pct, d$col_ganho_anual))
  })

  # ---- Learn tab: explanatory text (Markowitz frontier and ETFs) ----
  output$texto_markowitz <- renderUI({
    d <- dic()
    tagList(
      h4(d$markowitz_titulo), p(class = "text-justify", d$markowitz_texto),
      h4(d$etf_titulo), p(class = "text-justify", d$etf_texto)
    )
  })

  output$cabecalho_aprenda <- renderUI({
    d <- dic()
    tagList(h4(d$aprenda_titulo), p(d$aprenda_intro))
  })

  output$tabela_aprenda <- renderDT({
    d <- dic()
    campo_desc <- if (input$idioma == "pt") "pt" else "en"
    campo_cat  <- if (input$idioma == "pt") "categoria_pt" else "categoria_en"

    tabela <- TABELA_APRENDA |>
      transmute(Ticker = ticker, Descricao = .data[[campo_desc]], Categoria = .data[[campo_cat]])

    datatable(tabela, options = list(dom = "t", pageLength = 15, ordering = FALSE), rownames = FALSE,
              selection = "none",
              colnames = c(d$col_ticker, d$col_oque, d$col_categoria))
  })

  # ---- Observação sobre alinhamento de dados ----
  output$observacao_alinhamento <- renderUI({
    d <- dic()
    tagList(
      strong(d$observacao_alinhamento_titulo),
      p(class = "text-justify", d$observacao_alinhamento_texto)
    )
  })

  # ---- Model assumptions (condensed version of the README section) ----
  output$pressupostos_modelo <- renderUI({
    d <- dic()
    tagList(
      strong(d$pressupostos_titulo),
      tags$ul(
        class = "text-justify",
        style = "padding-left: 20px; margin-top: 8px; margin-bottom: 0;",
        lapply(d$pressupostos_itens, tags$li)
      )
    )
  })
}

shinyApp(ui, server)
