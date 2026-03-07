# Overview Page UI

overview_ui <- function() {
  div(
    class = "tab-stack",
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      uiOutput("kpi_total_shipments"),
      uiOutput("kpi_high_risk"),
      uiOutput("kpi_total_exposure"),
      uiOutput("kpi_avg_risk")
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        class = "rr-card",
        card_header("Risk Level Distribution"),
        chartOutput("overview_risk_dist", height = 290)
      ),
      card(
        class = "rr-card",
        card_header("Shipping Method Distribution"),
        chartOutput("overview_method_dist", height = 290)
      ),
      card(
        class = "rr-card",
        card_header("Weather Impact on Average Risk"),
        chartOutput("overview_weather_risk", height = 290)
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "rr-card",
        card_header("Top High-Risk Shipments"),
        DTOutput("tbl_high_risk")
      ),
      card(
        class = "rr-card",
        card_header("Top Financial Exposure"),
        DTOutput("tbl_top_exposure")
      )
    )
  )
}
