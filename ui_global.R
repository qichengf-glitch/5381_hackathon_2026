# Global UI and Styles

ui_global <- function() {
  tagList(
    tags$head(
      tags$style(
        HTML(
          "
          :root {
            --olive: #6E7E31;
            --olive-soft: #EEF3E2;
            --border-soft: #E7EAEE;
            --text-primary: #1F2933;
            --text-muted: #6B7280;
            --risk-low-bg: #EAF4EC;
            --risk-medium-bg: #FFF4DF;
            --risk-high-bg: #FCE9E6;
          }

          body {
            background: #FFFFFF;
            color: var(--text-primary);
          }

          .bslib-page-navbar > .navbar {
            border-bottom: 1px solid var(--border-soft);
            background: #FFFFFF;
          }

          .navbar-brand {
            font-weight: 700;
            letter-spacing: 0.2px;
          }

          .nav-link.active {
            color: var(--olive) !important;
            border-bottom: 2px solid var(--olive);
          }

          .tab-stack {
            padding: 0.25rem 0.15rem 1.25rem 0.15rem;
            display: grid;
            gap: 1rem;
          }

          .rr-card, .kpi-card {
            border: 1px solid var(--olive);
            border-radius: 14px;
            box-shadow: 0 2px 8px rgba(17, 24, 39, 0.04);
            background: var(--olive-soft);
          }

          .rr-card > .card-header {
            background: var(--olive-soft);
            border-bottom: 1px solid var(--olive);
            font-weight: 600;
            color: var(--olive);
          }

          .kpi-card {
            padding: 1rem 1.1rem;
          }

          .kpi-head {
            display: flex;
            align-items: center;
            justify-content: space-between;
            margin-bottom: 0.45rem;
          }

          .kpi-title {
            font-size: 0.86rem;
            color: var(--text-muted);
            font-weight: 600;
          }

          .kpi-icon i {
            color: var(--olive);
          }

          .kpi-value {
            font-size: 1.65rem;
            font-weight: 700;
            line-height: 1.2;
          }

          .kpi-subtitle {
            margin-top: 0.2rem;
            font-size: 0.8rem;
            color: var(--text-muted);
          }

          .risk-badge {
            display: inline-block;
            padding: 0.2rem 0.55rem;
            border-radius: 999px;
            font-size: 0.73rem;
            font-weight: 700;
          }

          .risk-low {
            background: var(--risk-low-bg);
            color: #1E6A2B;
          }

          .risk-medium {
            background: var(--risk-medium-bg);
            color: #9A6A08;
          }

          .risk-high {
            background: var(--risk-high-bg);
            color: #9E3126;
          }

          .btn-olive, .btn-primary {
            background-color: var(--olive) !important;
            border-color: var(--olive) !important;
            color: #FFFFFF !important;
          }

          .btn-olive:hover, .btn-primary:hover {
            background-color: #5F6D2A !important;
            border-color: #5F6D2A !important;
          }

          .stacked-panel {
            display: grid;
            gap: 1rem;
          }

          .legend-row {
            display: flex;
            align-items: center;
            gap: 0.45rem;
            margin-bottom: 0.5rem;
            font-size: 0.9rem;
            color: var(--text-primary);
          }

          .legend-dot {
            width: 11px;
            height: 11px;
            border-radius: 50%;
            display: inline-block;
          }

          .detail-grid {
            display: grid;
            grid-template-columns: repeat(2, minmax(0, 1fr));
            gap: 0.65rem;
          }

          .detail-item {
            border: 1px solid var(--border-soft);
            border-radius: 10px;
            padding: 0.6rem 0.75rem;
            background: #FAFBFC;
          }

          .detail-label {
            font-size: 0.76rem;
            color: var(--text-muted);
            text-transform: uppercase;
            letter-spacing: 0.3px;
          }

          .detail-value {
            font-size: 0.96rem;
            font-weight: 600;
            margin-top: 0.15rem;
          }

          .placeholder-note p {
            margin-bottom: 0;
            color: var(--text-muted);
          }

          .stress-card {
            border: 1px solid var(--border-soft);
            border-left: 4px solid var(--olive);
            border-radius: 10px;
            padding: 0.65rem 0.75rem;
            margin-bottom: 0.6rem;
          }

          .stress-title {
            font-size: 0.84rem;
            color: var(--text-muted);
          }

          .stress-value {
            font-size: 1.1rem;
            font-weight: 700;
            margin-top: 0.2rem;
          }

          .copilot-caption {
            color: var(--text-muted);
            margin-bottom: 0.8rem;
          }

          .copilot-link {
            display: inline-block;
            margin-bottom: 0.6rem;
            color: #314254;
            font-weight: 500;
            text-decoration: none;
          }

          .copilot-link:hover {
            color: var(--olive);
          }

          .copilot-chat {
            border: 1px solid var(--border-soft);
            border-radius: 12px;
            background: #FBFCFD;
            padding: 0.75rem;
            min-height: 290px;
            max-height: 420px;
            overflow-y: auto;
            margin-bottom: 0.8rem;
          }

          .chat-msg {
            margin-bottom: 0.75rem;
          }

          .chat-role {
            font-size: 0.73rem;
            text-transform: uppercase;
            letter-spacing: 0.3px;
            color: var(--text-muted);
            margin-bottom: 0.2rem;
          }

          .chat-bubble {
            border-radius: 10px;
            padding: 0.55rem 0.7rem;
            line-height: 1.38;
            white-space: pre-wrap;
          }

          .chat-user .chat-bubble {
            background: #ECF1DF;
            border: 1px solid #DCE6C1;
          }

          .chat-assistant .chat-bubble {
            background: #FFFFFF;
            border: 1px solid var(--border-soft);
          }

          .copilot-actions {
            display: flex;
            justify-content: flex-end;
          }
        "
        )
      )
    ),
    page_navbar(
      title = "RiskRoute AI",
      theme = bs_theme(
        version = 5,
        bg = "white",
        fg = "#1F2933",
        primary = "#6E7E31",
        base_font = font_google("Source Sans 3"),
        heading_font = font_google("Manrope")
      ),
      nav_panel("Overview", overview_ui()),
      nav_panel("Global Map", global_map_ui()),
      nav_panel("Risk Monitor", risk_monitor_ui()),
      nav_panel("Financial Risk", financial_ui()),
      nav_panel("Warehouse & Historical Analytics", warehouse_ui()),
      nav_panel("AI Copilot", copilot_ui())
    )
  )
}
