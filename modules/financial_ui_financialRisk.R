# Financial Risk Page UI (Financial-Risk variant)

financial_ui <- function() {
  div(
    class = "tab-stack",
    layout_columns(
      col_widths = c(6, 6),
      uiOutput("kpi_financial_avg"),
      uiOutput("kpi_financial_focus")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "rr-card",
        card_header("Risk Score vs Shipment Value"),
        chartOutput("financial_scatter", height = 300)
      ),
      card(
        class = "rr-card",
        card_header("Top 10 Exposed Shipments"),
        DTOutput("financial_top_table")
      )
    ),
    card(
      class = "rr-card",
      card_header("Summary"),
      uiOutput("financial_ai_summary")
    )
  )
}
