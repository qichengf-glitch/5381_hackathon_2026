# Risk Monitor Page UI

risk_monitor_ui <- function() {
  div(
    class = "tab-stack",
    tags$style(HTML("
      .rm-preset-group .btn { transition: none !important; }
    ")),
    tags$script(HTML("
      $(document).on('click', '.rm-preset-group .rm-preset-btn', function() {
        var $group = $(this).closest('.rm-preset-group');
        $group.find('.rm-preset-btn').removeClass('btn-primary').addClass('btn-outline-secondary');
        $(this).removeClass('btn-outline-secondary').addClass('btn-primary');
      });
    ")),
    card(
      class = "rr-card",
      card_header("Operational Filters"),
      div(
        class = "rm-preset-group",
        style = "display:flex; gap:0.5rem; justify-content:flex-end; margin-bottom:0.4rem;",
        actionButton("rm_preset_critical", "Critical Now", class = "rm-preset-btn btn btn-sm btn-primary"),
        actionButton("rm_preset_delayed", "Delayed Only", class = "rm-preset-btn btn btn-sm btn-outline-secondary"),
        actionButton("rm_preset_exposure", "High Exposure", class = "rm-preset-btn btn btn-sm btn-outline-secondary"),
        actionButton("rm_select_all", "Select All", class = "rm-preset-btn btn btn-sm btn-outline-secondary"),
        actionButton("rm_clear_all", "Clear", class = "rm-preset-btn btn btn-sm btn-outline-secondary")
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        selectizeInput(
          "filter_origin", "Origin", choices = NULL, multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "All origins")
        ),
        selectizeInput(
          "filter_destination", "Destination", choices = NULL, multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "All destinations")
        ),
        selectizeInput(
          "filter_method", "Shipping Method", choices = NULL, multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "All methods")
        )
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        selectizeInput(
          "filter_weather", "Weather Condition", choices = NULL, multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "All weather")
        ),
        selectizeInput(
          "filter_risk", "Risk Level", choices = c("Low", "Medium", "High"), multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "All levels")
        ),
        selectInput("filter_delay", "Delay Flag", choices = c("All", "Delayed", "On Time"), selected = "All")
      ),
      uiOutput("rm_filter_summary"),
      div(style = "color:#6b7280; font-size:0.85rem;", "Tip: empty multi-select means include all.")
    ),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      uiOutput("rm_kpi_shipments"),
      uiOutput("rm_kpi_avg_risk"),
      uiOutput("rm_kpi_exposure"),
      uiOutput("rm_kpi_delay_rate")
    ),
    card(
      class = "rr-card",
      card_header("Shipment Risk Queue"),
      layout_columns(
        col_widths = c(8, 4),
        textInput("rm_search", "Search", placeholder = "Shipment ID / Origin / Destination / Route"),
        div(
          style = "display:flex; align-items:flex-end; justify-content:flex-end; height:100%;",
          downloadButton("rm_export_queue", "Export Current Queue", class = "btn btn-sm btn-outline-secondary")
        )
      ),
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
