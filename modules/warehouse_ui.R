# Warehouse & Historical Analytics Page UI

warehouse_ui <- function() {
  div(
    class = "tab-stack",
    tags$style(HTML("
      .hub-stress-card {
        margin-bottom: 0.38rem;
        padding: 0.4rem 0.5rem !important;
        border-radius: 9px;
      }
      .capacity-scroll {
        max-height: 270px;
        overflow-y: auto;
        padding-right: 0.2rem;
      }
      .hub-metric-row {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 0.3rem;
        margin-top: 0.28rem;
      }
      .hub-metric {
        border: 1px solid #E7EAEE;
        border-radius: 8px;
        padding: 0.2rem 0.3rem;
        background: #FFFFFF;
      }
      .hub-metric-label {
        font-size: 0.62rem;
        color: #6B7280;
        text-transform: uppercase;
        letter-spacing: 0.2px;
      }
      .hub-metric-value {
        font-size: 0.8rem;
        font-weight: 700;
        margin-top: 0.08rem;
        color: #1F2933;
      }
      .hub-stress-tag {
        display: inline-block;
        margin-top: 0.28rem;
        padding: 0.1rem 0.4rem;
        border-radius: 999px;
        font-size: 0.63rem;
        font-weight: 700;
      }
      .hub-stress-card .stress-title { font-size: 0.72rem; }
      .hub-stress-card .stress-value { font-size: 0.98rem; margin-top: 0.08rem; }
    ")),
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
          "Top Hubs",
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
        card_header("A. Regional Hub Utilization"),
        chartOutputSafe("warehouse_utilization_plot", height = "245px")
      ),
      card(
        class = "rr-card",
        card_header("B. Predicted Capacity Stress"),
        div(class = "capacity-scroll", uiOutput("capacity_stress_cards"))
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "rr-card",
        card_header("C. Top Routes Reliability Scatter"),
        chartOutputSafe("route_reliability_trend", height = "300px")
      ),
      card(
        class = "rr-card",
        card_header("D. Network Delay Trend (Last 12 Weeks)"),
        chartOutputSafe("delay_trend_plot", height = "300px")
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
        radioButtons(
          "wh_route_metric",
          NULL,
          choices = c("Delay Rate" = "delay_rate", "Risk Score" = "avg_risk"),
          selected = "delay_rate",
          inline = TRUE
        ),
        chartOutputSafe("route_drilldown_trend", height = "290px")
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
