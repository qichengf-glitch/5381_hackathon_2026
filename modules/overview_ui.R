# Overview Page UI

overview_ui <- function() {
  div(
    class = "tab-stack overview-tab",
    tags$style(HTML("
      .overview-olive-header > .card-header {
        background: #6E7E31 !important;
        color: #FFFFFF !important;
        border-bottom: 1px solid #5D6A2B !important;
      }
      .overview-olive-header > .card-header .card-title,
      .overview-olive-header > .card-header .bslib-card-title {
        color: #FFFFFF !important;
      }
      .overview-tab .rr-card,
      .overview-tab .kpi-card {
        transition: transform 0.16s ease, box-shadow 0.18s ease, border-color 0.18s ease;
      }
      .overview-tab .rr-card:hover,
      .overview-tab .kpi-card:hover {
        transform: translateY(-2px);
        border-color: #7F9337 !important;
        box-shadow: 0 8px 18px rgba(110, 126, 49, 0.20), 0 2px 6px rgba(17, 24, 39, 0.08);
      }
      .overview-tab .rr-card:active,
      .overview-tab .kpi-card:active {
        transform: translateY(-1px);
      }
    ")),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      uiOutput("kpi_total_shipments"),
      uiOutput("kpi_high_risk"),
      uiOutput("kpi_total_exposure"),
      uiOutput("kpi_avg_risk")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "rr-card overview-olive-header",
        card_header("Risk Level Distribution"),
        chartOutput("overview_risk_dist", height = 320)
      ),
      card(
        class = "rr-card overview-olive-header",
        card_header("Average Risk Score by Shipping Method"),
        chartOutput("overview_method_dist", height = 320)
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        class = "rr-card overview-olive-header",
        card_header("Average Risk Score by Weather Condition"),
        chartOutput("overview_weather_risk", height = 320)
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "rr-card overview-olive-header",
        card_header("Top High-Risk Shipments"),
        DTOutput("tbl_high_risk")
      ),
      card(
        class = "rr-card overview-olive-header",
        card_header("Top Financial Exposure"),
        DTOutput("tbl_top_exposure")
      )
    )
  )
}
