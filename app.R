# Main Shiny Application Entry Point

# Load libraries
library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(readxl)
library(scales)
library(lubridate)
library(glue)
library(future)
library(promises)

# multisession: new R process per worker — safe with Shiny/httpuv (no fork issues)
plan(multisession, workers = 1)

options(bslib.sass.cache = file.path(tempdir(), "bslib-sass-cache"))

# Load constants and utilities (HAS_PLOTLY and HAS_LEAFLET are defined in utils.R)
source("constants.R")
source("utils.R")

# Load optional packages if available
if (HAS_PLOTLY) {
  library(plotly)
}

if (HAS_LEAFLET) {
  library(leaflet)
}

# Load UI modules
source("ui_global.R")
source("modules/overview_ui.R")
source("modules/global_map_ui.R")
source("modules/risk_monitor_ui.R")
source("modules/financial_ui_financialRisk.R")
source("modules/warehouse_ui.R")
source("modules/copilot_ui.R")

# ── Global data cache (shared across ALL sessions) ──────────────────────────
# Data loads ONCE in the background. Every session polls this cache instead of
# making its own Supabase request. This means:
#   - First visit: waits for Supabase cold-start (~2 min) once
#   - All later visits: instant data from cache
.app_cache <- new.env(parent = emptyenv())
.app_cache$shipments  <- NULL
.app_cache$historical <- NULL
.app_cache$warehouse  <- NULL
.app_cache$ready      <- FALSE

# Kick off the background fetch immediately at app startup
future({
  suppressPackageStartupMessages({
    library(dplyr); library(tibble); library(tidyr); library(purrr)
    library(lubridate); library(scales); library(glue)
    library(httr2); library(jsonlite); library(readxl); library(dotenv)
  })
  setwd("/app")
  source("constants.R")
  source("utils.R")
  ships <- load_shipment_data()
  list(
    shipments  = ships,
    historical = load_historical_data(base_shipments = ships),
    warehouse  = load_warehouse_status_data(base_shipments = ships)
  )
}) %...>% (function(result) {
  .app_cache$shipments  <- result$shipments
  .app_cache$historical <- result$historical
  .app_cache$warehouse  <- result$warehouse
  .app_cache$ready      <- TRUE
  message("[cache] Data loaded: ", nrow(result$shipments), " shipments")
}) %...!% (function(e) {
  warning("[cache] Load failed, using mock data: ", conditionMessage(e), call. = FALSE)
  ships <- generate_mock_shipments()
  .app_cache$shipments  <- ships
  .app_cache$historical <- derive_historical_from_shipments(ships)
  .app_cache$warehouse  <- derive_warehouse_from_shipments(ships)
  .app_cache$ready      <- TRUE
})

# Main UI
ui <- ui_global()

# Main Server
server <- function(input, output, session) {

  # ── Poll global cache every 800ms until data is ready ──────────
  cache_ready <- reactivePoll(
    intervalMillis = 800,
    session        = session,
    checkFunc      = function() .app_cache$ready,
    valueFunc      = function() .app_cache$ready
  )

  shipments <- reactive({
    req(cache_ready())
    req(.app_cache$shipments)
    .app_cache$shipments
  })

  historical_shipments <- reactive({
    req(cache_ready())
    req(.app_cache$historical)
    .app_cache$historical
  })

  warehouse_status_by_cargo <- reactive({
    req(cache_ready())
    req(.app_cache$warehouse)
    .app_cache$warehouse
  })

  # ── Loading overlay — shown while cache is not yet ready ────────
  output$loading_overlay <- renderUI({
    if (!isTRUE(cache_ready())) {
      div(
        id = "app-loading-overlay",
        style = "position:fixed;inset:0;z-index:9999;display:flex;align-items:center;justify-content:center;background:rgba(10,10,20,0.92);",
        div(style = "text-align:center;color:#E0E0E0;",
          div(style = "width:48px;height:48px;border:3px solid rgba(255,255,255,0.1);border-top-color:#00BCD4;border-radius:50%;animation:spin 0.8s linear infinite;margin:0 auto 16px;"),
          tags$style("@keyframes spin{to{transform:rotate(360deg)}}"),
          div(style = "font-size:16px;font-weight:600;", "Loading RiskRoute AI..."),
          div(style = "font-size:13px;opacity:0.6;margin-top:6px;", "Fetching shipment data from database")
        )
      )
    }
  })

  # ── Load and initialize server modules ──────────────────────────
  source("modules/overview_server.R", local = TRUE)
  overview_server(input, output, session, shipments)

  source("modules/global_map_server.R", local = TRUE)
  global_map_server(input, output, session, shipments)

  source("modules/risk_monitor_server.R", local = TRUE)
  risk_monitor_server(input, output, session, shipments)

  source("modules/financial_server_financialRisk.R", local = TRUE)
  financial_server(input, output, session, shipments)

  source("modules/warehouse_server.R", local = TRUE)
  warehouse_base <- warehouse_server(
    input, output, session,
    shipments                 = shipments,
    historical_shipments      = historical_shipments,
    warehouse_status_by_cargo = warehouse_status_by_cargo
  )

  source("modules/copilot_server.R", local = TRUE)
  copilot_server(input, output, session, shipments, warehouse_base)
}

shinyApp(ui, server)
