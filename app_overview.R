# Standalone Overview app for team split workflow
# Run: shiny::runApp("app_overview.R")

library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(readxl)
library(scales)
library(lubridate)
library(glue)

options(bslib.sass.cache = file.path(tempdir(), "bslib-sass-cache"))

source("constants.R")
source("utils.R")
source("modules/overview_ui.R")
source("modules/overview_server.R")

if (HAS_PLOTLY) {
  library(plotly)
}

ui <- tagList(
  tags$head(
    tags$style(
      HTML(
        "
        :root {
          --olive: #6E7E31;
          --border-soft: #E7EAEE;
          --text-primary: #1F2933;
          --text-muted: #6B7280;
          --risk-low-bg: #EAF4EC;
          --risk-medium-bg: #FFF4DF;
          --risk-high-bg: #FCE9E6;
        }
        body { background:#FFFFFF; color:var(--text-primary); }
        .bslib-page-navbar > .navbar { border-bottom:1px solid var(--border-soft); background:#FFFFFF; }
        .nav-link.active { color:var(--olive) !important; border-bottom:2px solid var(--olive); }
        .tab-stack { padding:0.25rem 0.15rem 1.25rem 0.15rem; display:grid; gap:1rem; }
        .rr-card, .kpi-card {
          border:1px solid var(--border-soft);
          border-radius:14px;
          box-shadow:0 2px 8px rgba(17,24,39,0.04);
          background:#FFFFFF;
        }
        .rr-card > .card-header { background:#FFFFFF; border-bottom:1px solid #F1F3F5; font-weight:600; }
        .kpi-card { padding:1rem 1.1rem; }
        .kpi-head { display:flex; align-items:center; justify-content:space-between; margin-bottom:0.45rem; }
        .kpi-title { font-size:0.86rem; color:var(--text-muted); font-weight:600; }
        .kpi-icon i { color:var(--olive); }
        .kpi-value { font-size:1.65rem; font-weight:700; line-height:1.2; }
        .kpi-subtitle { margin-top:0.2rem; font-size:0.8rem; color:var(--text-muted); }
        .risk-badge { display:inline-block; padding:0.2rem 0.55rem; border-radius:999px; font-size:0.73rem; font-weight:700; }
        .risk-low { background:var(--risk-low-bg); color:#1E6A2B; }
        .risk-medium { background:var(--risk-medium-bg); color:#9A6A08; }
        .risk-high { background:var(--risk-high-bg); color:#9E3126; }
      "
      )
    )
  ),
  page_navbar(
    title = "RiskRoute AI",
    theme = bs_theme(
      version = 5,
      bg = "white",
      fg = "#1F2933",
      primary = "#6E7E31",
      base_font = font_google("Source Sans 3"),
      heading_font = font_google("Manrope")
    ),
    nav_panel("Overview", overview_ui())
  )
)

server <- function(input, output, session) {
  data_rv <- reactiveVal(load_shipment_data())
  shipments <- reactive({
    req(data_rv())
    data_rv()
  })

  overview_server(input, output, session, shipments)
}

shinyApp(ui, server)
