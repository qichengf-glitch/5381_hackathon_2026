# Warehouse & Historical Analytics Page UI

warehouse_ui <- function() {
  div(
    class = "tab-stack",
    card(
      class = "rr-card",
      card_header("Filters"),
      layout_columns(
        col_widths = c(4, 4, 4),
        radioButtons(
          "wh_time_range",
          "Time Range",
          choices = c("Last 30 days" = 30, "Last 90 days" = 90, "Last 180 days" = 180, "All history" = 0),
          selected = 90,
          inline = TRUE
        ),
        sliderInput(
          "wh_top_n",
          "Top Warehouses",
          min = 3, max = 12, value = 6, step = 1
        ),
        numericInput(
          "wh_min_route_shipments",
          "Min Route Shipments",
          value = 5, min = 1, max = 100, step = 1
        )
      ),
      uiOutput("wh_filter_state"),
      div(class = "text-muted", style = "margin-top: 0.35rem;", "Tip: all charts and table below update together when filters change.")
    ),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      uiOutput("wh_kpi_shipments"),
      uiOutput("wh_kpi_delay_rate"),
      uiOutput("wh_kpi_reliability"),
      uiOutput("wh_kpi_high_stress")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        class = "rr-card",
        card_header("A. Warehouse Utilization"),
        chartOutput("warehouse_utilization_plot", height = 310)
      ),
      card(
        class = "rr-card",
        card_header("B. Predicted Capacity Stress"),
        uiOutput("capacity_stress_cards")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "rr-card",
        card_header("C. Historical Route Reliability"),
        chartOutput("route_reliability_trend", height = 300)
      ),
      card(
        class = "rr-card",
        card_header("D. Delay Trend Over Time"),
        chartOutput("delay_trend_plot", height = 300)
      )
    ),
    card(
      class = "rr-card",
      card_header("E. Route Reliability Leaderboard"),
      DTOutput("route_leaderboard")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        class = "rr-card",
        card_header("F. Selected Route Trend (Last 12 Weeks)"),
        chartOutput("route_drilldown_trend", height = 290)
      ),
      card(
        class = "rr-card",
        card_header("G. Actionable Insights"),
        uiOutput("route_drilldown_insights"),
        div(
          style = "display:flex; gap:0.5rem; margin-top:0.5rem; flex-wrap:wrap;",
          downloadButton("download_route_trend_csv", "Download Trend CSV", class = "btn btn-primary btn-sm"),
          downloadButton("download_route_insight_txt", "Download Insight TXT", class = "btn btn-outline-secondary btn-sm")
        )
      )
    )
  )
}
