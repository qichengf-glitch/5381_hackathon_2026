# Risk Monitor Page UI

risk_monitor_ui <- function() {
  div(
    class = "tab-stack",
    card(
      class = "rr-card",
      card_header("Operational Filters"),
      layout_columns(
        col_widths = c(2, 2, 2, 2, 2, 2),
        selectizeInput("filter_origin", "Origin", choices = NULL, multiple = TRUE),
        selectizeInput("filter_destination", "Destination", choices = NULL, multiple = TRUE),
        selectizeInput("filter_method", "Shipping Method", choices = NULL, multiple = TRUE),
        selectizeInput("filter_weather", "Weather Condition", choices = NULL, multiple = TRUE),
        selectizeInput("filter_risk", "Risk Level", choices = c("Low", "Medium", "High"), multiple = TRUE),
        selectInput("filter_delay", "Delay Flag", choices = c("All", "Delayed", "On Time"), selected = "All")
      )
    ),
    card(
      class = "rr-card",
      card_header("Shipment Risk Queue"),
      DTOutput("risk_table")
    ),
    layout_columns(
      col_widths = c(4, 8),
      card(
        class = "rr-card",
        card_header("Shipment Detail"),
        uiOutput("shipment_detail_card")
      ),
      card(
        class = "rr-card",
        card_header("Risk Driver Breakdown"),
        chartOutput("risk_driver_plot", height = 300)
      )
    )
  )
}
