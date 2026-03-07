# Warehouse & Historical Analytics Page UI

warehouse_ui <- function() {
  div(
    class = "tab-stack",
    card(
      class = "rr-card placeholder-note",
      strong("Warehouse & Historical Analytics scaffold"),
      p(
        "This section is intentionally structured for future warehouse feeds and time-series integrations.",
        "Current visuals are data-driven where possible and include placeholders for expansion."
      )
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
    )
  )
}
