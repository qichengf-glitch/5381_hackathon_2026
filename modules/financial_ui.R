# Financial Risk Page UI

financial_ui <- function() {
  div(
    class = "tab-stack",
    layout_columns(
      col_widths = c(6, 6),
      uiOutput("kpi_financial_total"),
      uiOutput("kpi_financial_avg")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        class = "rr-card",
        card_header("Risk Score vs Shipment Value"),
        chartOutput("financial_scatter", height = 340)
      ),
      card(
        class = "rr-card",
        card_header("Financial Exposure Distribution"),
        chartOutput("financial_distribution", height = 340)
      )
    ),
    card(
      class = "rr-card",
      card_header("Top 10 Exposed Shipments"),
      DTOutput("financial_top_table")
    )
  )
}
