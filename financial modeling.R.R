library(shiny)
library(shinydashboard)
library(plotly)
library(quantmod)
library(TTR)
library(readr)
library(rvest)
library(DT)
library(zoo)

# ---------------------------
# NIFTY 500
# ---------------------------

nifty_url <- "https://archives.nseindia.com/content/indices/ind_nifty500list.csv"
nifty500 <- read_csv(nifty_url, show_col_types = FALSE)

companies <- setNames(
  paste0(nifty500$Symbol, ".NS"),
  nifty500$Company_Name
)

# ---------------------------
# PRICE DATA
# ---------------------------

get_price <- function(ticker){
  data <- suppressWarnings(getSymbols(ticker, src="yahoo", auto.assign=FALSE))
  df <- data.frame(date = index(data), price = as.numeric(Cl(data)))
  df$price <- na.locf(df$price)
  df$price <- na.locf(df$price, fromLast = TRUE)
  df$MA50 <- SMA(df$price, 50)
  df$MA200 <- SMA(df$price, 200)
  df$return <- c(NA, diff(log(df$price)))
  df
}

# ---------------------------
# FINANCIALS
# ---------------------------

get_financials <- function(ticker){
  symbol <- gsub(".NS", "", ticker)
  tryCatch({
    url <- paste0("https://www.screener.in/company/", symbol, "/")
    page <- read_html(url)
    tables <- html_table(page, fill=TRUE)
    tables[[1]]
  }, error=function(e){
    data.frame(Metric="No Data", Value="NA")
  })
}

# ---------------------------
# DCF
# ---------------------------

dcf_value <- function(price, growth, wacc){
  future <- price * (1 + growth)^5
  future / (1 + wacc)^5
}

# ---------------------------
# MONTE CARLO
# ---------------------------

monte_carlo <- function(price){
  vals <- numeric(400)
  for(i in 1:400){
    g <- rnorm(1, 0.08, 0.03)
    w <- rnorm(1, 0.12, 0.02)
    vals[i] <- dcf_value(price, g, w)
  }
  vals
}

# ---------------------------
# HEATMAP
# ---------------------------

dcf_heatmap <- function(price){
  growth <- seq(0.04, 0.12, 0.01)
  wacc <- seq(0.08, 0.14, 0.01)
  z <- outer(growth, wacc, Vectorize(function(g,w){ dcf_value(price, g, w) }))
  list(growth=growth, wacc=wacc, z=z)
}

# ---------------------------
# GLOWING CSS
# ---------------------------

glowing_css <- "
  @import url('https://fonts.googleapis.com/css2?family=Orbitron:wght@400;700;900&family=Share+Tech+Mono&display=swap');

  :root {
    --gold:       #FFD700;
    --gold-dim:   #b8960c;
    --cyan:       #00f5ff;
    --green:      #00ff88;
    --bg-deep:    #020408;
    --bg-card:    #060d18;
    --bg-panel:   #080f1e;
    --border:     rgba(0,245,255,0.18);
    --glow-gold:  0 0 8px #FFD700, 0 0 20px #FFD70066, 0 0 40px #FFD70033;
    --glow-cyan:  0 0 8px #00f5ff, 0 0 20px #00f5ff66, 0 0 40px #00f5ff22;
    --glow-green: 0 0 8px #00ff88, 0 0 20px #00ff8866;
  }

  /* ---- GLOBAL RESET ---- */
  * { box-sizing: border-box; }

  html, body {
    background: var(--bg-deep) !important;
    font-family: 'Share Tech Mono', monospace !important;
    color: var(--gold) !important;
    min-height: 100vh;
    overflow-x: hidden;
  }

  /* ---- ANIMATED GRID BACKGROUND ---- */
  body::before {
    content: '';
    position: fixed;
    inset: 0;
    z-index: 0;
    background-image:
      linear-gradient(rgba(0,245,255,0.04) 1px, transparent 1px),
      linear-gradient(90deg, rgba(0,245,255,0.04) 1px, transparent 1px);
    background-size: 50px 50px;
    animation: gridPulse 8s ease-in-out infinite;
    pointer-events: none;
  }

  @keyframes gridPulse {
    0%, 100% { opacity: 0.5; }
    50%       { opacity: 1.0; }
  }

  /* ---- RADIAL GLOW ORBS ---- */
  body::after {
    content: '';
    position: fixed;
    inset: 0;
    z-index: 0;
    background:
      radial-gradient(ellipse 600px 400px at 20% 20%, rgba(0,245,255,0.07) 0%, transparent 70%),
      radial-gradient(ellipse 500px 500px at 80% 80%, rgba(255,215,0,0.06) 0%, transparent 70%),
      radial-gradient(ellipse 400px 300px at 50% 50%, rgba(0,255,136,0.03) 0%, transparent 70%);
    pointer-events: none;
    animation: orbFloat 12s ease-in-out infinite alternate;
  }

  @keyframes orbFloat {
    0%   { transform: translate(0,0) scale(1); }
    100% { transform: translate(20px, -20px) scale(1.05); }
  }

  /* ---- WRAPPER / CONTENT ---- */
  .wrapper, .content-wrapper, .main-sidebar, .left-side {
    background: transparent !important;
    position: relative;
    z-index: 1;
  }

  /* ---- HEADER ---- */
  .main-header .navbar,
  .main-header .logo {
    background: linear-gradient(135deg, #050d1a 0%, #0a1628 100%) !important;
    border-bottom: 1px solid var(--border) !important;
    box-shadow: var(--glow-cyan) !important;
  }

  .main-header .logo {
    font-family: 'Orbitron', sans-serif !important;
    font-size: 15px !important;
    font-weight: 900 !important;
    color: var(--gold) !important;
    text-shadow: var(--glow-gold) !important;
    letter-spacing: 2px !important;
    border-right: 1px solid var(--border) !important;
  }

  /* ---- SIDEBAR ---- */
  .main-sidebar {
    background: linear-gradient(180deg, #060d18 0%, #020408 100%) !important;
    border-right: 1px solid var(--border) !important;
    box-shadow: 4px 0 30px rgba(0,245,255,0.08) !important;
  }

  .sidebar-menu > li > a {
    color: rgba(255,215,0,0.7) !important;
    font-family: 'Share Tech Mono', monospace !important;
    letter-spacing: 1px;
    border-left: 2px solid transparent !important;
    transition: all 0.3s ease !important;
  }

  .sidebar-menu > li > a:hover,
  .sidebar-menu > li.active > a {
    color: var(--cyan) !important;
    background: rgba(0,245,255,0.07) !important;
    border-left: 2px solid var(--cyan) !important;
    text-shadow: var(--glow-cyan) !important;
  }

  /* ---- FORM CONTROLS (sidebar inputs) ---- */
  .form-control, select, input[type='number'] {
    background: rgba(0,245,255,0.05) !important;
    border: 1px solid var(--border) !important;
    color: var(--gold) !important;
    font-family: 'Share Tech Mono', monospace !important;
    border-radius: 4px !important;
    transition: border 0.3s, box-shadow 0.3s !important;
  }

  .form-control:focus, select:focus, input[type='number']:focus {
    border-color: var(--cyan) !important;
    box-shadow: var(--glow-cyan) !important;
    outline: none !important;
    background: rgba(0,245,255,0.09) !important;
  }

  .control-label {
    color: var(--gold-dim) !important;
    font-family: 'Orbitron', sans-serif !important;
    font-size: 10px !important;
    letter-spacing: 2px !important;
    text-transform: uppercase !important;
  }

  /* ---- TAB PANEL ---- */
  .nav-tabs {
    border-bottom: 1px solid var(--border) !important;
    background: rgba(6,13,24,0.9) !important;
    backdrop-filter: blur(10px) !important;
    padding: 6px 10px 0 !important;
    position: sticky;
    top: 0;
    z-index: 10;
  }

  .nav-tabs > li > a {
    color: rgba(255,215,0,0.55) !important;
    font-family: 'Orbitron', sans-serif !important;
    font-size: 10px !important;
    letter-spacing: 2px !important;
    border: 1px solid transparent !important;
    border-radius: 4px 4px 0 0 !important;
    background: transparent !important;
    transition: all 0.3s ease !important;
    padding: 8px 16px !important;
    margin-right: 4px !important;
  }

  .nav-tabs > li > a:hover {
    color: var(--cyan) !important;
    border-color: var(--border) !important;
    background: rgba(0,245,255,0.06) !important;
    text-shadow: var(--glow-cyan) !important;
  }

  .nav-tabs > li.active > a,
  .nav-tabs > li.active > a:focus,
  .nav-tabs > li.active > a:hover {
    color: var(--gold) !important;
    background: rgba(255,215,0,0.08) !important;
    border-color: var(--gold-dim) !important;
    border-bottom-color: transparent !important;
    text-shadow: var(--glow-gold) !important;
    box-shadow: 0 -2px 12px rgba(255,215,0,0.25) !important;
  }

  /* ---- TAB CONTENT ---- */
  .tab-content {
    background: rgba(6,13,24,0.75) !important;
    border: 1px solid var(--border) !important;
    border-top: none !important;
    border-radius: 0 0 8px 8px !important;
    padding: 20px !important;
    backdrop-filter: blur(12px) !important;
    box-shadow:
      0 4px 40px rgba(0,245,255,0.06),
      inset 0 1px 0 rgba(0,245,255,0.1) !important;
  }

  /* ---- PLOTLY CHARTS ---- */
  .plotly .main-svg {
    background: transparent !important;
  }

  /* ---- DATATABLE ---- */
  .dataTables_wrapper,
  table.dataTable,
  table.dataTable th,
  table.dataTable td {
    color: var(--gold) !important;
    font-family: 'Share Tech Mono', monospace !important;
    background: transparent !important;
  }

  table.dataTable {
    border-collapse: separate !important;
    border-spacing: 0 4px !important;
  }

  table.dataTable thead th {
    background: rgba(0,245,255,0.08) !important;
    border-bottom: 1px solid var(--cyan) !important;
    color: var(--cyan) !important;
    text-shadow: var(--glow-cyan) !important;
    font-family: 'Orbitron', sans-serif !important;
    font-size: 10px !important;
    letter-spacing: 1px !important;
  }

  table.dataTable tbody tr {
    background: rgba(6,13,24,0.6) !important;
    border: 1px solid transparent !important;
    transition: all 0.2s ease !important;
  }

  table.dataTable tbody tr:hover {
    background: rgba(0,245,255,0.07) !important;
    border-color: var(--border) !important;
    box-shadow: 0 0 12px rgba(0,245,255,0.12) !important;
  }

  table.dataTable tbody td {
    border-top: 1px solid rgba(0,245,255,0.07) !important;
  }

  .dataTables_filter input,
  .dataTables_length select {
    background: rgba(0,245,255,0.05) !important;
    border: 1px solid var(--border) !important;
    color: var(--gold) !important;
    border-radius: 4px !important;
  }

  .dataTables_info, .dataTables_paginate {
    color: var(--gold-dim) !important;
    font-size: 11px !important;
  }

  .paginate_button {
    color: var(--gold-dim) !important;
    border: 1px solid transparent !important;
    border-radius: 4px !important;
  }

  .paginate_button.current, .paginate_button:hover {
    background: rgba(255,215,0,0.1) !important;
    border-color: var(--gold-dim) !important;
    color: var(--gold) !important;
    box-shadow: var(--glow-gold) !important;
  }

  /* ---- VERBATIM / DCF OUTPUT ---- */
  pre, .shiny-text-output {
    background: rgba(0,255,136,0.05) !important;
    border: 1px solid rgba(0,255,136,0.25) !important;
    color: var(--green) !important;
    font-family: 'Share Tech Mono', monospace !important;
    font-size: 22px !important;
    font-weight: bold !important;
    text-shadow: var(--glow-green) !important;
    border-radius: 8px !important;
    padding: 30px !important;
    text-align: center !important;
    box-shadow: 0 0 30px rgba(0,255,136,0.1), inset 0 0 30px rgba(0,255,136,0.03) !important;
    animation: pulseDCF 3s ease-in-out infinite !important;
  }

  @keyframes pulseDCF {
    0%, 100% { box-shadow: 0 0 20px rgba(0,255,136,0.1), inset 0 0 20px rgba(0,255,136,0.03); }
    50%       { box-shadow: 0 0 50px rgba(0,255,136,0.2), inset 0 0 30px rgba(0,255,136,0.07); }
  }

  /* ---- SCROLLBAR ---- */
  ::-webkit-scrollbar { width: 6px; height: 6px; }
  ::-webkit-scrollbar-track { background: var(--bg-deep); }
  ::-webkit-scrollbar-thumb {
    background: linear-gradient(180deg, var(--cyan), var(--gold));
    border-radius: 3px;
    box-shadow: var(--glow-cyan);
  }

  /* ---- LOADING SPINNER ---- */
  .shiny-busy {
    position: fixed !important;
    top: 12px !important;
    right: 16px !important;
    z-index: 9999 !important;
  }

  /* ---- SCANLINE OVERLAY ---- */
  .content-wrapper::before {
    content: '';
    position: fixed;
    inset: 0;
    z-index: 0;
    background: repeating-linear-gradient(
      0deg,
      rgba(0,0,0,0.03) 0px,
      rgba(0,0,0,0.03) 1px,
      transparent 1px,
      transparent 3px
    );
    pointer-events: none;
  }
"

# ---------------------------
# UI
# ---------------------------

ui <- fluidPage(
  
  tags$head(
    tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
    tags$link(rel="stylesheet",
              href="https://fonts.googleapis.com/css2?family=Orbitron:wght@400;700;900&family=Share+Tech+Mono&display=swap"),
    tags$style(HTML(glowing_css)),
    tags$style(HTML("
      html, body { margin:0; padding:0; overflow-x:hidden; }
      .container-fluid { padding:0 !important; margin:0 !important; }
      /* Custom header bar */
      #app-header {
        height: 52px;
        background: linear-gradient(135deg, #050d1a 0%, #0a1628 100%);
        border-bottom: 1px solid rgba(0,245,255,0.18);
        box-shadow: 0 0 8px #00f5ff, 0 0 20px rgba(0,245,255,0.3);
        display: flex;
        align-items: center;
        padding: 0 20px;
        position: fixed;
        top: 0; left: 0; right: 0;
        z-index: 1000;
      }
      #app-header span {
        font-family: 'Orbitron', sans-serif;
        font-size: 15px;
        font-weight: 900;
        color: #FFD700;
        text-shadow: 0 0 8px #FFD700, 0 0 20px rgba(255,215,0,0.5);
        letter-spacing: 3px;
      }
      /* Sidebar */
      #app-sidebar {
        position: fixed;
        top: 52px; left: 0; bottom: 0;
        width: 220px;
        background: linear-gradient(180deg, #060d18 0%, #020408 100%);
        border-right: 1px solid rgba(0,245,255,0.18);
        box-shadow: 4px 0 30px rgba(0,245,255,0.08);
        overflow-y: auto;
        padding: 15px 10px;
        z-index: 999;
      }
      /* Main content */
      #app-main {
        margin-left: 220px;
        margin-top: 52px;
        padding: 10px;
        min-height: calc(100vh - 52px);
      }
    "))
  ),
  
  # HEADER
  tags$div(id = "app-header",
           tags$span("⬡  FINANCIAL MODEL")
  ),
  
  # SIDEBAR
  tags$div(id = "app-sidebar",
           tags$div(
             style="font-family:'Orbitron',sans-serif;font-size:9px;letter-spacing:3px;color:rgba(0,245,255,0.5);border-bottom:1px solid rgba(0,245,255,0.1);padding-bottom:8px;margin-bottom:12px;",
             "▸ NIFTY 500 ANALYSIS"
           ),
           selectInput("company", "COMPANY", choices=companies, width="100%"),
           numericInput("growth", "GROWTH RATE", 0.08, min=0, max=1, step=0.01, width="100%"),
           numericInput("wacc", "WACC", 0.12, min=0, max=1, step=0.01, width="100%"),
           tags$div(
             style="padding:12px;margin-top:10px;border:1px solid rgba(0,245,255,0.1);border-radius:6px;background:rgba(0,245,255,0.03);",
             tags$div(style="font-family:'Orbitron',sans-serif;font-size:8px;letter-spacing:2px;color:rgba(0,245,255,0.5);margin-bottom:6px;","▸ STATUS"),
             tags$div(style="width:8px;height:8px;border-radius:50%;background:#00ff88;display:inline-block;margin-right:8px;box-shadow:0 0 8px #00ff88,0 0 20px #00ff88;animation:blink 2s infinite;"),
             tags$span(style="color:rgba(0,255,136,0.8);font-size:11px;","LIVE DATA")
           ),
           tags$style("@keyframes blink{0%,100%{opacity:1}50%{opacity:0.3}}")
  ),
  
  # MAIN CONTENT
  tags$div(id = "app-main",
           tabsetPanel(id = "main_tabs",
                       
                       tabPanel("📈 PRICE",
                                plotlyOutput("price", height="520px")
                       ),
                       
                       tabPanel("〰 MOVING AVG",
                                plotlyOutput("ma", height="520px")
                       ),
                       
                       tabPanel("📊 RETURNS",
                                plotlyOutput("returns", height="520px")
                       ),
                       
                       tabPanel("💹 FINANCIALS",
                                tags$div(style="margin-top:10px;",
                                         dataTableOutput("fin")
                                )
                       ),
                       
                       tabPanel("💰 DCF VALUE",
                                tags$div(
                                  style="display:flex;flex-direction:column;align-items:center;justify-content:center;padding:40px;",
                                  tags$div(
                                    style="font-family:'Orbitron',sans-serif;font-size:11px;letter-spacing:4px;color:rgba(0,245,255,0.6);margin-bottom:20px;",
                                    "▸ DISCOUNTED CASH FLOW — INTRINSIC VALUE"
                                  ),
                                  verbatimTextOutput("dcf")
                                )
                       ),
                       
                       tabPanel("⚡ IV GAUGE",
                                plotlyOutput("gauge", height="520px")
                       ),
                       
                       tabPanel("🔥 HEATMAP",
                                plotlyOutput("heatmap", height="520px")
                       ),
                       
                       tabPanel("🎲 MONTE CARLO",
                                plotlyOutput("mc", height="520px")
                       )
           )
  )
)

# ---------------------------
# PLOT THEME HELPER
# ---------------------------

dark_layout <- function(p, title=""){
  p %>% layout(
    title = list(text=title, font=list(family="Orbitron", color="#FFD700", size=14), x=0.01),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(6,13,24,0.5)",
    font = list(family="Share Tech Mono", color="#FFD700", size=12),
    xaxis = list(
      gridcolor = "rgba(0,245,255,0.08)",
      linecolor = "rgba(0,245,255,0.2)",
      zerolinecolor = "rgba(0,245,255,0.15)",
      tickfont = list(color="rgba(255,215,0,0.6)")
    ),
    yaxis = list(
      gridcolor = "rgba(0,245,255,0.08)",
      linecolor = "rgba(0,245,255,0.2)",
      zerolinecolor = "rgba(0,245,255,0.15)",
      tickfont = list(color="rgba(255,215,0,0.6)")
    ),
    legend = list(font=list(color="#FFD700"), bgcolor="rgba(6,13,24,0.8)",
                  bordercolor="rgba(0,245,255,0.2)", borderwidth=1),
    margin = list(t=50, b=40, l=50, r=20)
  )
}

# ---------------------------
# SERVER
# ---------------------------

server <- function(input, output){
  
  df_data <- reactive({
    req(input$company)
    get_price(input$company)
  })
  
  price_val <- reactive({
    req(df_data())
    tail(df_data()$price, 1)
  })
  
  # ---- PRICE ----
  output$price <- renderPlotly({
    df <- df_data()
    p <- plot_ly(df, x=~date, y=~price, type="scatter", mode="lines",
                 line=list(color="#00f5ff", width=2),
                 fill="tozeroy",
                 fillcolor="rgba(0,245,255,0.06)",
                 name="Price",
                 hovertemplate="<b>%{x}</b><br>₹%{y:,.2f}<extra></extra>"
    )
    dark_layout(p, "CLOSING PRICE")
  })
  
  # ---- MA ----
  output$ma <- renderPlotly({
    df <- df_data()
    df <- df[complete.cases(df),]
    p <- plot_ly(df, x=~date) %>%
      add_lines(y=~price,  name="Price",  line=list(color="#00f5ff", width=2)) %>%
      add_lines(y=~MA50,   name="MA 50",  line=list(color="#FFD700", width=1.5, dash="dot")) %>%
      add_lines(y=~MA200,  name="MA 200", line=list(color="#ff6b35", width=1.5, dash="dash"))
    dark_layout(p, "MOVING AVERAGES")
  })
  
  # ---- RETURNS ----
  output$returns <- renderPlotly({
    df <- df_data()
    p <- plot_ly(df, x=~date, y=~return, type="scatter", mode="lines",
                 line=list(color="#00ff88", width=1.5),
                 fill="tozeroy", fillcolor="rgba(0,255,136,0.05)",
                 name="Log Return",
                 hovertemplate="<b>%{x}</b><br>Return: %{y:.4f}<extra></extra>"
    )
    dark_layout(p, "LOG RETURNS")
  })
  
  # ---- FINANCIALS ----
  output$fin <- renderDataTable({
    datatable(
      get_financials(input$company),
      options = list(
        pageLength = 15,
        dom = "frtip",
        initComplete = JS("function(settings,json){$(this.api().table().node()).css({'background':'transparent','color':'#FFD700'});}")
      ),
      class = "display compact"
    )
  })
  
  # ---- DCF ----
  output$dcf <- renderPrint({
    g <- input$growth
    w <- input$wacc
    val <- dcf_value(price_val(), g, w)
    cat(sprintf("₹ %s", format(round(val, 2), big.mark=",")))
  })
  
  # ---- GAUGE ----
  output$gauge <- renderPlotly({
    g <- input$growth
    w <- input$wacc
    intrinsic <- dcf_value(price_val(), g, w)
    current   <- price_val()
    ratio     <- intrinsic / current
    gauge_max <- max(intrinsic, current) * 1.5
    
    plot_ly(
      type="indicator", mode="gauge+number+delta",
      value = intrinsic,
      delta = list(reference=current, valueformat=".2f",
                   increasing=list(color="#00ff88"), decreasing=list(color="#ff4444")),
      number = list(prefix="₹", valueformat=",.2f",
                    font=list(color="#FFD700", family="Orbitron", size=36)),
      gauge = list(
        axis  = list(range=list(0, gauge_max), tickcolor="#FFD700",
                     tickfont=list(color="rgba(255,215,0,0.6)", family="Share Tech Mono")),
        bar   = list(color="#00f5ff", thickness=0.25),
        bgcolor = "rgba(6,13,24,0.8)",
        borderwidth = 1, bordercolor = "rgba(0,245,255,0.3)",
        steps = list(
          list(range=list(0, current*0.8),  color="rgba(255,68,68,0.2)"),
          list(range=list(current*0.8, current*1.2), color="rgba(255,215,0,0.15)"),
          list(range=list(current*1.2, gauge_max),   color="rgba(0,255,136,0.15)")
        ),
        threshold = list(
          line=list(color="#FFD700", width=3),
          thickness=0.8, value=current
        )
      )
    ) %>%
      layout(
        paper_bgcolor="rgba(0,0,0,0)",
        font=list(color="#FFD700", family="Share Tech Mono"),
        margin=list(t=60,b=30),
        annotations=list(
          list(x=0.5, y=0.18, text=paste0("CURRENT PRICE: ₹", format(round(current,2), big.mark=",")),
               showarrow=FALSE, font=list(color="rgba(255,215,0,0.6)", size=12, family="Orbitron"))
        )
      )
  })
  
  # ---- HEATMAP ----
  output$heatmap <- renderPlotly({
    h <- dcf_heatmap(price_val())
    plot_ly(
      x=h$wacc, y=h$growth, z=h$z,
      type="heatmap",
      colorscale=list(
        list(0,   "#020408"),
        list(0.25, "#0a2040"),
        list(0.5,  "#00f5ff"),
        list(0.75, "#FFD700"),
        list(1,   "#ffffff")
      ),
      colorbar=list(tickfont=list(color="#FFD700"), title=list(text="DCF Value", font=list(color="#FFD700")))
    ) %>%
      dark_layout("DCF SENSITIVITY — GROWTH vs WACC") %>%
      layout(
        xaxis=list(title=list(text="WACC", font=list(color="#00f5ff")), tickformat=".0%"),
        yaxis=list(title=list(text="Growth", font=list(color="#00f5ff")), tickformat=".0%")
      )
  })
  
  # ---- MONTE CARLO ----
  output$mc <- renderPlotly({
    vals      <- monte_carlo(price_val())
    dens      <- density(vals)
    brks      <- hist(vals, plot=FALSE)
    median_val <- median(vals)
    
    plot_ly() %>%
      add_histogram(
        x=vals, nbinsx=40,
        marker=list(color="rgba(0,245,255,0.45)", line=list(color="#00f5ff", width=0.5)),
        name="Simulations"
      ) %>%
      add_lines(
        x=dens$x,
        y=dens$y * max(brks$counts) / max(dens$y),
        line=list(color="#FFD700", width=2.5),
        name="KDE Curve"
      ) %>%
      dark_layout("MONTE CARLO — DCF DISTRIBUTION (400 SIMULATIONS)") %>%
      layout(
        shapes=list(list(
          type="line", x0=median_val, x1=median_val, y0=0, y1=1, yref="paper",
          line=list(color="#00ff88", width=2, dash="dot")
        )),
        annotations=list(list(
          x=median_val, y=1, yref="paper",
          text=paste0(" MEDIAN: ₹", format(round(median_val,0), big.mark=",")),
          showarrow=FALSE,
          font=list(color="#00ff88", size=11, family="Share Tech Mono"),
          xanchor="left"
        ))
      )
  })
  
}

shinyApp(ui, server)