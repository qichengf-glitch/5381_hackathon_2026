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
  # Load data from Supabase only
  data_rv <- reactiveVal(load_shipment_data())
  historical_rv <- reactiveVal(NULL)
  warehouse_status_rv <- reactiveVal(NULL)

  shipments <- reactive({
    req(data_rv())
    data_rv()
  })

  observe({
    base <- shipments()
    historical_rv(load_historical_data(base_shipments = base))
    warehouse_status_rv(load_warehouse_status_data(base_shipments = base))
  })

  historical_shipments <- reactive({
    req(historical_rv())
    historical_rv()
  })

  warehouse_status_by_cargo <- reactive({
    req(warehouse_status_rv())
    warehouse_status_rv()
  })

  # Load and initialize server modules
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
    shipments = shipments,
    historical_shipments = historical_shipments,
    warehouse_status_by_cargo = warehouse_status_by_cargo
  )
  
  source("modules/copilot_server.R", local = TRUE)
  copilot_server(input, output, session, shipments, warehouse_base)
}

shinyApp(ui, server)
