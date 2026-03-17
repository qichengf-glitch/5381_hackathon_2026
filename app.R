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

# Main UI
ui <- ui_global()

# Main Server
server <- function(input, output, session) {
  data_rv               <- reactiveVal(NULL)
  historical_rv         <- reactiveVal(NULL)
  warehouse_status_rv   <- reactiveVal(NULL)
  data_loading          <- reactiveVal(TRUE)

  # ── Async primary data load ─────────────────────────────────────
  # multisession workers are fresh R sessions — must reload packages & sources.
  future_promise({
    suppressPackageStartupMessages({
      library(dplyr); library(tibble); library(tidyr); library(purrr)
      library(lubridate); library(scales); library(glue)
      library(httr2); library(jsonlite); library(readxl); library(dotenv)
    })
    setwd("/app")
    source("constants.R")
    source("utils.R")
    load_shipment_data()
  }) %...>% (function(data) {
    data_rv(data)
    data_loading(FALSE)
  }) %...!% (function(e) {
    warning("Async primary data load failed: ", conditionMessage(e), call. = FALSE)
    data_rv(generate_mock_shipments())
    data_loading(FALSE)
  })

  shipments <- reactive({
    req(data_rv())
    data_rv()
  })

  # ── Async secondary data load (after primary is ready) ──────────
  observe({
    base <- shipments()
    future_promise({
      suppressPackageStartupMessages({
        library(dplyr); library(tibble); library(tidyr); library(purrr)
        library(lubridate); library(scales); library(glue)
        library(httr2); library(jsonlite); library(readxl); library(dotenv)
      })
      setwd("/app")
      source("constants.R")
      source("utils.R")
      list(
        historical = load_historical_data(base_shipments = base),
        warehouse  = load_warehouse_status_data(base_shipments = base)
      )
    }) %...>% (function(result) {
      historical_rv(result$historical)
      warehouse_status_rv(result$warehouse)
    }) %...!% (function(e) {
      warning("Async secondary data load failed: ", conditionMessage(e), call. = FALSE)
      historical_rv(derive_historical_from_shipments(base))
      warehouse_status_rv(derive_warehouse_from_shipments(base))
    })
  })

  # ── Loading overlay — shows until primary data arrives ──────────
  output$loading_overlay <- renderUI({
    if (isTRUE(data_loading())) {
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

  historical_shipments <- reactive({
    req(historical_rv())
    historical_rv()
  })

  warehouse_status_by_cargo <- reactive({
    req(warehouse_status_rv())
    warehouse_status_rv()
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
    shipments               = shipments,
    historical_shipments    = historical_shipments,
    warehouse_status_by_cargo = warehouse_status_by_cargo
  )

  source("modules/copilot_server.R", local = TRUE)
  copilot_server(input, output, session, shipments, warehouse_base)
}

shinyApp(ui, server)
