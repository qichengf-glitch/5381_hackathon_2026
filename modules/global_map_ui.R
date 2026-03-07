# Global Map Page UI — Floating Overlay Design v2

global_map_ui <- function() {
  tagList(
    tags$style(HTML("
      /* ══════════════════════════════════════════════════
         FULL-BLEED MAP  +  GLASSMORPHIC OVERLAYS  v2
         ══════════════════════════════════════════════════ */

      .map-fullbleed.tab-stack {
        padding: 0 !important;
        display: block !important;
        gap: 0 !important;
        position: relative;
        height: calc(100vh - 58px);
        overflow: hidden;
      }
      .map-bg {
        position: absolute;
        inset: 0;
        z-index: 0;
      }
      .map-bg .html-widget,
      .map-bg .leaflet-container,
      .map-bg .shiny-plot-output {
        width: 100% !important;
        height: 100% !important;
      }
      .map-bg .leaflet-control-zoom {
        margin-top: 70px !important;
        margin-left: 10px !important;
      }
      .map-bg .leaflet-control-zoom a {
        background: rgba(15,15,25,0.6) !important;
        color: #aaa !important;
        border-color: rgba(255,255,255,0.06) !important;
      }
      .map-bg .leaflet-control-attribution {
        font-size: 9px;
        opacity: 0.35;
        background: transparent !important;
      }

      /* ── Glass base ──────────────────────────────────── */
      .glass {
        border-radius: 16px;
        transition: transform 0.25s cubic-bezier(.4,0,.2,1),
                    box-shadow 0.25s cubic-bezier(.4,0,.2,1);
      }
      .glass:hover {
        transform: scale(1.025);
      }

      /* — Dark / Satellite ——————————————————————— */
      .theme-dark .glass,
      .theme-satellite .glass {
        background: rgba(12, 14, 24, 0.52);
        backdrop-filter: blur(24px) saturate(1.4);
        -webkit-backdrop-filter: blur(24px) saturate(1.4);
        border: 1px solid rgba(255,255,255,0.07);
        box-shadow: 0 4px 24px rgba(0,0,0,0.30),
                    inset 0 0.5px 0 rgba(255,255,255,0.06);
        color: #E2E5EA;
      }
      .theme-dark .glass:hover,
      .theme-satellite .glass:hover {
        box-shadow: 0 8px 40px rgba(0,0,0,0.40),
                    inset 0 0.5px 0 rgba(255,255,255,0.08);
      }
      /* — Light ——————————————————————————————— */
      .theme-light .glass {
        background: rgba(255, 255, 255, 0.62);
        backdrop-filter: blur(24px) saturate(1.3);
        -webkit-backdrop-filter: blur(24px) saturate(1.3);
        border: 1px solid rgba(0,0,0,0.06);
        box-shadow: 0 4px 24px rgba(0,0,0,0.08),
                    inset 0 0.5px 0 rgba(255,255,255,0.7);
        color: #1F2933;
      }
      .theme-light .glass:hover {
        box-shadow: 0 8px 40px rgba(0,0,0,0.12),
                    inset 0 0.5px 0 rgba(255,255,255,0.8);
      }

      /* ── Filter strip (top) ──────────────────────────── */
      .fp-filters {
        position: absolute;
        z-index: 1000;
        top: 12px;
        left: 12px;
        max-height: 40px;
        overflow: hidden;
        padding: 0;
        transition: max-height 0.35s cubic-bezier(.4,0,.2,1),
                    padding 0.35s cubic-bezier(.4,0,.2,1);
      }
      .fp-filters .filter-toggle {
        display: flex;
        align-items: center;
        gap: 6px;
        padding: 10px 16px;
        font-size: 11.5px;
        font-weight: 700;
        letter-spacing: 0.3px;
        cursor: pointer;
        user-select: none;
      }
      .fp-filters .filter-toggle .ft-icon {
        opacity: 0.7;
        font-size: 11px;
      }
      .fp-filters .filter-toggle .ft-arrow {
        opacity: 0.5;
        font-size: 9px;
        margin-left: auto;
        transition: transform 0.3s ease;
      }
      .fp-filters.expanded .filter-toggle .ft-arrow {
        transform: rotate(180deg);
      }
      .fp-filters.expanded {
        max-height: 300px;
        padding-bottom: 8px;
        max-width: calc(100% - 260px);
      }
      .fp-filters .filter-body {
        padding: 0 14px;
      }
      .fp-filters .filter-grid {
        display: grid;
        grid-template-columns: 1fr 1fr 1fr 140px;
        align-items: end;
        gap: 6px 10px;
      }
      /* ── Timeline slider ─────────────────────────────── */
      .filter-timeline {
        margin-top: 6px;
        padding: 4px 0 2px;
        border-top: 1px solid rgba(128,128,128,0.10);
      }
      .filter-timeline .tl-header {
        display: flex;
        align-items: center;
        gap: 6px;
        margin-bottom: 2px;
      }
      .filter-timeline .tl-label {
        font-size: 9px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.8px;
        opacity: 0.7;
      }
      .filter-timeline .tl-badge {
        font-size: 9px;
        font-weight: 700;
        padding: 1px 6px;
        border-radius: 4px;
        background: rgba(77,208,200,0.15);
        color: #4DD0C8;
      }
      .filter-timeline .form-group { margin-bottom: 0; }
      .filter-timeline .irs { height: 32px; }
      .filter-timeline .irs-line { height: 4px; top: 14px; border-radius: 2px; }
      .theme-dark .filter-timeline .irs-line,
      .theme-satellite .filter-timeline .irs-line { background: rgba(255,255,255,0.08); }
      .theme-light .filter-timeline .irs-line { background: rgba(0,0,0,0.08); }
      .filter-timeline .irs-bar { height: 4px; top: 14px; border-radius: 2px; background: linear-gradient(90deg, #6E7E31, #4DD0C8); border: none; }
      .filter-timeline .irs-handle { width: 14px; height: 14px; top: 8px; border-radius: 50%; background: #fff; border: 2px solid #4DD0C8; box-shadow: 0 1px 6px rgba(0,0,0,0.3); cursor: pointer; }
      .filter-timeline .irs-min, .filter-timeline .irs-max { display: none; }
      .filter-timeline .irs-single, .filter-timeline .irs-from, .filter-timeline .irs-to {
        font-size: 9px; padding: 1px 5px; border-radius: 4px;
        background: rgba(77,208,200,0.2); color: #4DD0C8;
      }
      .filter-timeline .irs-slider { cursor: pointer; }
      /* Play button style */
      .filter-timeline .slider-animate-button {
        font-size: 10px !important;
        padding: 2px 6px !important;
        border-radius: 6px !important;
        top: -2px !important;
        right: 0 !important;
      }
      .theme-dark .filter-timeline .slider-animate-button,
      .theme-satellite .filter-timeline .slider-animate-button {
        background: rgba(255,255,255,0.08) !important;
        color: #4DD0C8 !important;
        border: 1px solid rgba(255,255,255,0.10) !important;
      }
      .fp-filters .f-group {
        min-width: 0;
      }
      .fp-filters .f-label {
        font-size: 9px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.8px;
        opacity: 0.7;
        margin-bottom: 4px;
        padding-left: 2px;
      }
      .fp-filters .form-group { margin-bottom: 0; }
      .fp-filters .selectize-input {
        min-height: 30px;
        font-size: 12px;
        padding: 3px 8px;
        border-radius: 10px;
      }
      .theme-dark .fp-filters .selectize-input,
      .theme-satellite .fp-filters .selectize-input {
        background: rgba(255,255,255,0.06);
        border: 1px solid rgba(255,255,255,0.10);
        color: #ddd;
      }
      .theme-dark .fp-filters .selectize-input .item,
      .theme-satellite .fp-filters .selectize-input .item {
        background: rgba(110,126,49,0.35);
        color: #cdd6a8;
        border-radius: 6px;
        font-size: 11px;
        padding: 1px 6px;
      }
      .theme-dark .fp-filters .selectize-dropdown,
      .theme-satellite .fp-filters .selectize-dropdown {
        background: rgba(20,22,35,0.96);
        border: 1px solid rgba(255,255,255,0.10);
        color: #ddd;
        border-radius: 10px;
        margin-top: 2px;
      }
      .theme-dark .fp-filters .selectize-dropdown .active,
      .theme-satellite .fp-filters .selectize-dropdown .active {
        background: rgba(110,126,49,0.45);
        color: #fff;
      }
      .theme-light .fp-filters .selectize-input {
        background: rgba(255,255,255,0.7);
        border: 1px solid rgba(0,0,0,0.08);
      }
      .fp-filters .f-checks {
        display: flex;
        flex-wrap: wrap;
        gap: 8px 14px;
        flex-shrink: 0;
        padding-bottom: 4px;
        align-items: center;
      }
      .fp-filters .f-checks .form-check,
      .fp-filters .f-checks .checkbox { margin: 0; }
      .fp-filters .f-checks label {
        font-size: 11px;
        font-weight: 500;
        white-space: nowrap;
        opacity: 0.8;
      }
      .theme-dark .fp-filters .f-checks .form-check-input,
      .theme-satellite .fp-filters .f-checks .form-check-input {
        background-color: rgba(255,255,255,0.10);
        border-color: rgba(255,255,255,0.15);
      }

      /* ── Style toggle (top-right) ───────────────────── */
      .fp-style {
        position: absolute;
        z-index: 1001;
        top: 12px;
        right: 12px;
        padding: 5px 8px;
      }
      .map-style-toggle {
        display: flex;
        align-items: center;
        gap: 1px;
      }
      .map-style-btn {
        background: none;
        border: none;
        font-size: 11.5px;
        font-weight: 600;
        padding: 5px 14px;
        border-radius: 8px;
        cursor: pointer;
        transition: all 0.18s ease;
      }
      .theme-dark .map-style-btn,
      .theme-satellite .map-style-btn { color: rgba(255,255,255,0.7); }
      .theme-light .map-style-btn { color: rgba(0,0,0,0.55); }
      .map-style-btn:hover {
        color: #B8C95A;
        background: rgba(110,126,49,0.12);
      }
      .map-style-btn.active-style {
        color: #fff !important;
        background: linear-gradient(135deg, #6E7E31, #8A9E3E);
        box-shadow: 0 2px 8px rgba(110,126,49,0.35);
      }
      .map-style-sep {
        color: rgba(150,150,150,0.2);
        font-size: 10px;
        user-select: none;
        padding: 0 2px;
      }

      /* ── KPI strip ──────────────────────────────────── */
      .fp-kpi-strip {
        position: absolute;
        z-index: 1000;
        bottom: 70px;
        left: 12px;
        display: flex;
        gap: 8px;
      }
      .kpi-chip {
        border-radius: 12px;
        padding: 8px 14px;
        min-width: 85px;
        transition: transform 0.25s cubic-bezier(.4,0,.2,1),
                    box-shadow 0.25s cubic-bezier(.4,0,.2,1);
      }
      .kpi-chip:hover {
        transform: scale(1.06);
      }
      .theme-dark .kpi-chip,
      .theme-satellite .kpi-chip {
        background: rgba(12,14,24,0.55);
        backdrop-filter: blur(24px) saturate(1.4);
        -webkit-backdrop-filter: blur(24px) saturate(1.4);
        border: 1px solid rgba(255,255,255,0.07);
        box-shadow: 0 4px 24px rgba(0,0,0,0.3);
        color: #E2E5EA;
      }
      .theme-dark .kpi-chip:hover,
      .theme-satellite .kpi-chip:hover {
        box-shadow: 0 8px 36px rgba(0,0,0,0.4);
      }
      .theme-light .kpi-chip {
        background: rgba(255,255,255,0.65);
        backdrop-filter: blur(24px);
        -webkit-backdrop-filter: blur(24px);
        border: 1px solid rgba(0,0,0,0.06);
        box-shadow: 0 4px 24px rgba(0,0,0,0.08);
        color: #1F2933;
      }
      .kpi-chip .kpi-c-label {
        font-size: 8.5px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.6px;
        opacity: 0.65;
        margin-bottom: 2px;
      }
      .kpi-chip .kpi-c-value {
        font-size: 1.05rem;
        font-weight: 800;
        line-height: 1;
        letter-spacing: -0.3px;
      }
      .kpi-chip .kpi-c-value.accent-green { color: #8FBF3A; }
      .kpi-chip .kpi-c-value.accent-amber { color: #E5A836; }
      .kpi-chip .kpi-c-value.accent-red   { color: #E05D50; }
      .kpi-chip .kpi-c-value.accent-cyan  { color: #4DD0C8; }

      /* ── Sidebar ─────────────────────────────────────── */
      .fp-sidebar {
        position: absolute;
        z-index: 1000;
        top: 56px;
        right: 12px;
        display: flex;
        flex-direction: column;
        gap: 5px;
        width: 220px;
      }
      .sb-panel {
        border-radius: 14px;
        overflow: hidden;
        max-height: 44px;
        transition: transform 0.25s cubic-bezier(.4,0,.2,1),
                    box-shadow 0.25s cubic-bezier(.4,0,.2,1),
                    max-height 0.4s cubic-bezier(.4,0,.2,1);
        cursor: pointer;
      }
      .theme-dark .sb-panel,
      .theme-satellite .sb-panel {
        background: rgba(12,14,24,0.52);
        backdrop-filter: blur(24px) saturate(1.4);
        -webkit-backdrop-filter: blur(24px) saturate(1.4);
        border: 1px solid rgba(255,255,255,0.07);
        box-shadow: 0 4px 24px rgba(0,0,0,0.3);
        color: #E2E5EA;
      }
      .theme-light .sb-panel {
        background: rgba(255,255,255,0.62);
        backdrop-filter: blur(24px);
        -webkit-backdrop-filter: blur(24px);
        border: 1px solid rgba(0,0,0,0.06);
        box-shadow: 0 4px 24px rgba(0,0,0,0.08);
        color: #1F2933;
      }
      .sb-panel:hover {
        transform: scale(1.02);
      }
      .sb-panel.expanded {
        max-height: 450px;
      }
      .sb-panel .sb-head {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 12px 16px;
        font-size: 12.5px;
        font-weight: 700;
        letter-spacing: 0.2px;
        user-select: none;
      }
      .sb-panel .sb-head .sb-icon {
        width: 22px;
        height: 22px;
        border-radius: 6px;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 10px;
        margin-right: 8px;
        flex-shrink: 0;
      }
      .theme-dark .sb-panel .sb-head .sb-icon,
      .theme-satellite .sb-panel .sb-head .sb-icon {
        background: rgba(110,126,49,0.25);
        color: #B8C95A;
      }
      .theme-light .sb-panel .sb-head .sb-icon {
        background: rgba(110,126,49,0.12);
        color: #6E7E31;
      }
      .sb-panel .sb-head .sb-title-row {
        display: flex;
        align-items: center;
        flex: 1;
      }
      .sb-panel .sb-head .sb-arrow {
        font-size: 10px;
        opacity: 0.55;
        transition: transform 0.3s ease, opacity 0.3s ease;
      }
      .sb-panel.expanded .sb-head .sb-arrow {
        transform: rotate(180deg);
        opacity: 0.6;
      }
      .sb-panel.expanded .sb-head {
        border-bottom: 1px solid rgba(128,128,128,0.12);
      }
      .sb-panel .sb-content {
        padding: 8px 12px 10px;
      }
      .sb-panel .sb-content .plotly {
        width: 100% !important;
      }
      .sb-panel .legend-row {
        font-size: 11.5px;
        padding: 2px 0;
        opacity: 0.85;
      }
      .sb-panel .legend-dot { width: 10px; height: 10px; }

      /* Force sidebar content text to inherit theme color */
      .theme-dark .sb-panel .sb-content,
      .theme-dark .sb-panel .sb-content *,
      .theme-satellite .sb-panel .sb-content,
      .theme-satellite .sb-panel .sb-content * {
        color: #D8DCE4 !important;
      }
      .theme-dark .sb-panel .sb-content table th,
      .theme-satellite .sb-panel .sb-content table th {
        color: #A0A8B8 !important;
        border-color: rgba(255,255,255,0.08) !important;
      }
      .theme-dark .sb-panel .sb-content table td,
      .theme-satellite .sb-panel .sb-content table td {
        border-color: rgba(255,255,255,0.06) !important;
      }
      .theme-light .sb-panel .sb-content,
      .theme-light .sb-panel .sb-content * {
        color: #2D3748 !important;
      }
      .theme-light .sb-panel .sb-content table th {
        color: #4A5568 !important;
      }

      /* ── Bottom detail bar ───────────────────────────── */
      .fp-detail {
        position: absolute;
        z-index: 1000;
        bottom: 12px;
        left: 12px;
        right: 12px;
        padding: 12px 18px;
        max-height: 48px;
        overflow: hidden;
        cursor: pointer;
        transition: transform 0.25s cubic-bezier(.4,0,.2,1),
                    box-shadow 0.25s cubic-bezier(.4,0,.2,1),
                    max-height 0.4s cubic-bezier(.4,0,.2,1);
      }
      .fp-detail:hover { transform: scale(1.008); }
      .fp-detail.expanded { max-height: 280px; }
      .fp-detail .detail-compact {
        display: flex;
        align-items: center;
        gap: 14px;
        font-size: 13px;
        white-space: nowrap;
        overflow: hidden;
      }
      .fp-detail .detail-compact .dc-id {
        font-weight: 700;
      }
      .fp-detail .detail-compact .dc-sep {
        opacity: 0.35;
        font-size: 11px;
      }
      .fp-detail .detail-compact .dc-risk {
        font-weight: 700;
      }
      .fp-detail .detail-compact .dc-hint {
        margin-left: auto;
        opacity: 0.5;
        font-size: 10px;
        font-weight: 500;
      }
      .fp-detail .detail-expanded {
        margin-top: 12px;
        padding-top: 10px;
        border-top: 1px solid rgba(128,128,128,0.12);
      }
      .fp-detail .detail-grid {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 10px 16px;
      }
      .fp-detail .detail-item {
        padding: 8px 10px;
        border-radius: 8px;
      }
      .theme-dark .fp-detail .detail-item,
      .theme-satellite .fp-detail .detail-item {
        background: rgba(255,255,255,0.06);
        border: 1px solid rgba(255,255,255,0.06);
      }
      .theme-light .fp-detail .detail-item {
        background: rgba(0,0,0,0.04);
        border: 1px solid rgba(0,0,0,0.06);
      }
      .fp-detail .detail-label { font-size: 11px; font-weight: 600; text-transform: uppercase; letter-spacing: 0.4px; margin-bottom: 2px; }
      .theme-dark .fp-detail .detail-label,
      .theme-satellite .fp-detail .detail-label { color: #A0A8B8; }
      .theme-light .fp-detail .detail-label { color: #6B7280; }
      .fp-detail .detail-value { font-weight: 700; font-size: 13px; }
      .theme-dark .fp-detail .detail-value,
      .theme-satellite .fp-detail .detail-value { color: #F0F2F5; }
      .theme-light .fp-detail .detail-value { color: #1F2933; }

      /* ── Leaflet legend override ─────────────────────── */
      .map-bg .leaflet-control-layers,
      .map-bg .info.legend {
        background: rgba(12,14,24,0.6) !important;
        backdrop-filter: blur(12px) !important;
        border: 1px solid rgba(255,255,255,0.08) !important;
        border-radius: 10px !important;
        color: #ccc !important;
        box-shadow: 0 2px 12px rgba(0,0,0,0.3) !important;
      }
    ")),
    div(
      class = "tab-stack map-fullbleed theme-dark",

      # ── Full-bleed map ────────────────────────────────
      div(class = "map-bg", mapWidgetOutput("route_map", height = "100%")),

      # ── Top: filter strip (collapsible) ────────────────
      div(
        class = "glass fp-filters",
        onclick = "this.classList.toggle('expanded')",
        div(class = "filter-toggle",
            span(class = "ft-icon", icon("filter")),
            span("Filters"),
            span(class = "ft-arrow", icon("chevron-down"))),
        div(class = "filter-body",
            onclick = "event.stopPropagation()",
            div(class = "filter-grid",
                div(class = "f-group",
                    div(class = "f-label", "Transport"),
                    selectizeInput("map_transport_filter", NULL,
                      choices = NULL, multiple = TRUE, width = "100%",
                      options = list(placeholder = "All", plugins = list("remove_button")))),
                div(class = "f-group",
                    div(class = "f-label", "Risk Level"),
                    selectizeInput("map_risk_filter", NULL,
                      choices = NULL, multiple = TRUE, width = "100%",
                      options = list(placeholder = "All", plugins = list("remove_button")))),
                div(class = "f-group",
                    div(class = "f-label", "Weather"),
                    selectizeInput("map_weather_filter", NULL,
                      choices = NULL, multiple = TRUE, width = "100%",
                      options = list(placeholder = "All", plugins = list("remove_button")))),
                div(class = "f-checks",
                    checkboxInput("map_show_routes", "Routes", TRUE),
                    checkboxInput("map_show_markers", "Markers", TRUE))),
            div(class = "filter-timeline",
                div(class = "tl-header",
                    span(class = "tl-label", "Timeline"),
                    uiOutput("map_transit_badge", inline = TRUE)),
                uiOutput("map_timeline_slider")))
      ),

      # ── Top-right: style toggle ───────────────────────
      div(class = "glass fp-style", uiOutput("map_style_buttons")),

      # ── Bottom-left: KPI chips ────────────────────────
      div(class = "fp-kpi-strip",
          uiOutput("map_kpi_routes"),
          uiOutput("map_kpi_avg_risk"),
          uiOutput("map_kpi_high_pct"),
          uiOutput("map_kpi_exposure"),
          uiOutput("map_kpi_in_transit")),

      # ── Right: sidebar ────────────────────────────────
      div(
        class = "fp-sidebar",
        div(class = "sb-panel",
            onclick = "this.classList.toggle('expanded')",
            div(class = "sb-head",
                div(class = "sb-title-row",
                    div(class = "sb-icon", icon("truck")),
                    span("Transport")),
                span(class = "sb-arrow", icon("chevron-down"))),
            div(class = "sb-content", chartOutput("map_transport_summary", height = 160))),
        div(class = "sb-panel",
            onclick = "this.classList.toggle('expanded')",
            div(class = "sb-head",
                div(class = "sb-title-row",
                    div(class = "sb-icon", icon("chart-bar")),
                    span("Risk Breakdown")),
                span(class = "sb-arrow", icon("chevron-down"))),
            div(class = "sb-content", DTOutput("map_risk_summary"))),
        div(class = "sb-panel",
            onclick = "this.classList.toggle('expanded')",
            div(class = "sb-head",
                div(class = "sb-title-row",
                    div(class = "sb-icon", icon("circle-info")),
                    span("Legend")),
                span(class = "sb-arrow", icon("chevron-down"))),
            div(class = "sb-content", uiOutput("risk_legend_widget")))
      ),

      # ── Bottom: route detail ──────────────────────────
      div(class = "glass fp-detail",
          onclick = "this.classList.toggle('expanded')",
          uiOutput("map_route_detail"))
    )
  )
}
