# AI Copilot Page UI

copilot_ui <- function() {
  prompt_btn <- function(id, icon_name, label_text) {
    actionButton(
      id,
      label = tagList(icon(icon_name), span(label_text)),
      class = "copilot-chip"
    )
  }

  div(
    class = "tab-stack",
    tags$style(
      HTML(
        "
        .copilot-layout-v2 {
          display: grid;
          grid-template-columns: minmax(260px, 30%) minmax(0, 70%);
          gap: 1rem;
          align-items: stretch;
        }

        .copilot-quick {
          align-self: start;
        }

        .copilot-quick-head {
          margin-bottom: 0.55rem;
          padding-bottom: 0.55rem;
          border-bottom: 1px solid rgba(110, 126, 49, 0.35);
        }

        .copilot-quick-title {
          color: var(--olive);
          font-size: 1.08rem;
          font-weight: 760;
          margin-bottom: 0.18rem;
        }

        .copilot-quick-subtitle {
          color: #5F6D2A;
          font-size: 0.85rem;
          margin-bottom: 0;
        }

        .copilot-prompt-list {
          display: grid;
          gap: 0.42rem;
          margin-top: 0;
        }

        .copilot-chip.btn {
          width: 100%;
          text-align: left;
          border-radius: 11px;
          border: 1px solid var(--olive);
          background: var(--olive);
          color: #FFFFFF;
          font-weight: 500;
          padding: 0.48rem 0.68rem;
          line-height: 1.28;
          white-space: normal;
          min-height: 40px;
        }

        .copilot-chip.btn i {
          color: rgba(255, 255, 255, 0.95);
          margin-right: 0.42rem;
          width: 0.95rem;
          text-align: center;
        }

        .copilot-prompt-list .copilot-chip:nth-child(1) i { color: #FFD166 !important; }
        .copilot-prompt-list .copilot-chip:nth-child(2) i { color: #8AD6FF !important; }
        .copilot-prompt-list .copilot-chip:nth-child(3) i { color: #FF9F9F !important; }
        .copilot-prompt-list .copilot-chip:nth-child(4) i { color: #B9F18C !important; }
        .copilot-prompt-list .copilot-chip:nth-child(5) i { color: #FFC8A2 !important; }
        .copilot-prompt-list .copilot-chip:nth-child(6) i { color: #D0B3FF !important; }
        .copilot-prompt-list .copilot-chip:nth-child(7) i { color: #9FE4D3 !important; }

        .copilot-chip.btn:hover,
        .copilot-chip.btn:focus,
        .copilot-chip.btn:active {
          background: #5F6D2A !important;
          border-color: #5F6D2A !important;
          color: #FFFFFF !important;
          box-shadow: none;
        }

        .copilot-quick-note {
          margin-top: 0.62rem;
          margin-bottom: 0;
          color: var(--text-muted);
          font-size: 0.84rem;
        }

        .copilot-main-card {
          min-height: 0;
          height: 100%;
          display: flex;
          flex-direction: column;
        }

        .copilot-head-label {
          color: var(--olive);
          font-size: 0.74rem;
          letter-spacing: 0.5px;
          text-transform: uppercase;
          margin-bottom: 0.16rem;
          font-weight: 700;
        }

        .copilot-main-title {
          margin: 0;
          font-size: 1.34rem;
          font-weight: 800;
        }

        .copilot-main-subtitle {
          margin-top: 0.28rem;
          margin-bottom: 0.78rem;
          color: var(--text-muted);
          font-size: 0.98rem;
        }

        .copilot-compose {
          margin-top: 0;
          margin-bottom: 0.7rem;
          display: flex;
          align-items: center;
          gap: 0.42rem;
          border: 1px solid var(--border-soft);
          border-radius: 13px;
          background: #FFFFFF;
          padding: 0.34rem;
        }

        .copilot-compose:focus-within {
          border-color: #C8D6A4;
          box-shadow: 0 0 0 3px rgba(110, 126, 49, 0.12);
        }

        .copilot-compose .shiny-input-container {
          margin-bottom: 0;
          width: auto;
          max-width: none;
          flex: 1;
        }

        .copilot-compose .form-control {
          border: none;
          box-shadow: none;
          min-height: 44px;
          font-size: 1rem;
          padding: 0.55rem 0.75rem;
        }

        .copilot-compose .form-control:focus {
          box-shadow: none;
        }

        .copilot-send.btn {
          border-radius: 10px;
          min-width: 108px;
          min-height: 44px;
          padding: 0.5rem 1rem;
          font-weight: 650;
        }

        .copilot-chat-v2 {
          border: 1px solid var(--border-soft);
          border-radius: 12px;
          background: #FBFCFD;
          padding: 0.74rem;
          min-height: 240px;
          max-height: 380px;
          overflow-y: auto;
          flex: 1;
        }

        .copilot-msg {
          margin-bottom: 0.72rem;
        }

        .copilot-msg-meta {
          font-size: 0.74rem;
          color: var(--text-muted);
          margin-bottom: 0.18rem;
          font-weight: 600;
        }

        .copilot-msg-bubble {
          border-radius: 11px;
          padding: 0.56rem 0.72rem;
          line-height: 1.42;
          white-space: pre-wrap;
          border: 1px solid var(--border-soft);
          background: #FFFFFF;
        }

        .copilot-msg.user .copilot-msg-bubble {
          background: #ECF1DF;
          border-color: #DCE6C1;
        }

        @media (max-width: 992px) {
          .copilot-layout-v2 {
            grid-template-columns: 1fr;
            align-items: start;
          }

          .copilot-main-card {
            min-height: 460px;
            height: auto;
          }
        }
        "
      )
    ),
    div(
      class = "copilot-layout-v2",
      div(
        class = "copilot-quick",
        card(
          class = "rr-card",
          div(
            class = "copilot-quick-head",
            div(class = "copilot-quick-title", "Suggested Questions"),
            div(class = "copilot-quick-subtitle", "Quick prompts across all dashboard sections.")
          ),
          div(
            class = "copilot-prompt-list",
            prompt_btn("q_dashboard", "compass", "Copilot: Explain this dashboard end-to-end."),
            prompt_btn("q_overview", "chart-pie", "Overview: What's our current risk snapshot?"),
            prompt_btn("q_routes", "route", "Global Map: Which routes are most vulnerable now?"),
            prompt_btn("q_delay", "triangle-exclamation", "Risk Monitor: Which shipments face highest delay risk?"),
            prompt_btn("q_drivers", "sliders-h", "Risk Monitor: What factors drive shipment risk most?"),
            prompt_btn("q_exposure", "dollar-sign", "Financial Risk: Which shipments drive top exposure?"),
            prompt_btn("q_warehouse", "warehouse", "Warehouse: Which hub is under the most stress?")
          ),
          p(class = "copilot-quick-note", "Click a prompt to generate an executive summary.")
        )
      ),
      card(
        class = "rr-card copilot-main-card",
        div(class = "copilot-head-label", "RiskRoute AI"),
        h3(class = "copilot-main-title", "AI Copilot"),
        p(
          class = "copilot-main-subtitle",
          "Ask any dashboard or free-form logistics risk question and get a concise operational briefing."
        ),
        div(
          class = "copilot-compose",
          textInput("copilot_query", NULL, placeholder = "Ask a supply chain risk question...", width = "100%"),
          actionButton("ask_copilot", "Analyze", class = "btn-olive copilot-send")
        ),
        div(class = "copilot-chat-v2", uiOutput("copilot_chat"))
      )
    )
  )
}
