# Global Map Page UI

global_map_ui <- function() {
  div(
    class = "tab-stack",
    layout_columns(
      col_widths = c(8, 4),
      card(
        class = "rr-card map-card",
        card_header("Global Route Intelligence"),
        mapWidgetOutput("route_map", height = 630)
      ),
      div(
        class = "stacked-panel",
        card(
          class = "rr-card",
          card_header("Transport Mode Summary"),
          chartOutput("map_transport_summary", height = 190)
        ),
        card(
          class = "rr-card",
          card_header("Route Risk Summary"),
          DTOutput("map_risk_summary")
        ),
        card(
          class = "rr-card",
          card_header("Risk Legend"),
          uiOutput("risk_legend_widget")
        )
      )
    ),
    card(
      class = "rr-card",
      card_header("Selected Route Detail"),
      uiOutput("map_route_detail")
    )
  )
}
