# Financial Risk Page UI (Financial-Risk variant)

financial_ui <- function() {
  div(
    class = "tab-stack financial-tab",
    tags$style(HTML("
      .financial-olive-header > .card-header {
        background: #6E7E31 !important;
        color: #FFFFFF !important;
        border-bottom: 1px solid #5D6A2B !important;
      }
      .financial-olive-header > .card-header .card-title,
      .financial-olive-header > .card-header .bslib-card-title {
        color: #FFFFFF !important;
      }
      .financial-tab .rr-card,
      .financial-tab .kpi-card {
        transition: transform 0.16s ease, box-shadow 0.18s ease, border-color 0.18s ease;
      }
      .financial-tab .rr-card:hover,
      .financial-tab .kpi-card:hover {
        transform: translateY(-2px);
        border-color: #7F9337 !important;
        box-shadow: 0 8px 18px rgba(110, 126, 49, 0.20), 0 2px 6px rgba(17, 24, 39, 0.08);
      }
      .financial-tab .rr-card:active,
      .financial-tab .kpi-card:active {
        transform: translateY(-1px);
      }
    ")),
    layout_columns(
      col_widths = c(6, 6),
      uiOutput("kpi_financial_avg"),
      uiOutput("kpi_financial_focus")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "rr-card financial-olive-header",
        card_header("Risk Score vs Shipment Value"),
        chartOutput("financial_scatter", height = 300)
      ),
      card(
        class = "rr-card financial-olive-header",
        card_header("Top Financial Exposure"),
        DTOutput("financial_top_table")
      )
    ),
    card(
      class = "rr-card financial-olive-header",
      card_header("Summary"),
      uiOutput("financial_ai_summary")
    )
  )
}
