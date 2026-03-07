# AI Copilot Page UI

copilot_ui <- function() {
  div(
    class = "tab-stack",
    layout_columns(
      col_widths = c(4, 8),
      card(
        class = "rr-card",
        card_header("Suggested Business Questions"),
        p(class = "copilot-caption", "Click a prompt or write your own question."),
        actionLink("q_delay", "Which shipments are at highest risk of delay?", class = "copilot-link"),
        br(),
        actionLink("q_routes", "Which routes are most vulnerable?", class = "copilot-link"),
        br(),
        actionLink("q_drivers", "What factors are driving shipment risk?", class = "copilot-link"),
        br(),
        actionLink("q_exposure", "Which shipments create the highest financial exposure?", class = "copilot-link"),
        br(),
        actionLink("q_warehouse", "Which warehouse is under the most stress?", class = "copilot-link")
      ),
      card(
        class = "rr-card",
        card_header("AI Copilot"),
        div(class = "copilot-chat", uiOutput("copilot_chat")),
        textInput("copilot_query", NULL, placeholder = "Ask a supply chain risk question..."),
        div(class = "copilot-actions", actionButton("ask_copilot", "Analyze", class = "btn-olive"))
      )
    )
  )
}
