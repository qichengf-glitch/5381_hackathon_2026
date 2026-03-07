library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(readxl)
library(scales)
library(lubridate)
library(glue)

options(bslib.sass.cache = file.path(tempdir(), "bslib-sass-cache"))

HAS_PLOTLY <- requireNamespace("plotly", quietly = TRUE)
if (HAS_PLOTLY) {
  library(plotly)
}

HAS_LEAFLET <- requireNamespace("leaflet", quietly = TRUE)
if (HAS_LEAFLET) {
  library(leaflet)
}

DEFAULT_DATA_PATH <- "/Users/qichengfu/Desktop/5381_hackathon/Final_shipment.xlsx"

RISK_COLORS <- c(
  Low = "#2E7D32",
  Medium = "#D39B33",
  High = "#C6473B"
)

METHOD_COLORS <- c(
  Truck = "#5B7CFA",
  Sea = "#0A6E8A",
  Air = "#4AA6A0",
  Rail = "#7B8D40"
)

CITY_COORDS <- tribble(
  ~city, ~lat, ~lng,
  "Shanghai", 31.2304, 121.4737,
  "Istanbul", 41.0082, 28.9784,
  "Berlin", 52.5200, 13.4050,
  "Tokyo", 35.6762, 139.6503,
  "Seoul", 37.5665, 126.9780,
  "Bangkok", 13.7563, 100.5018,
  "Kuala Lumpur", 3.1390, 101.6869,
  "Singapore", 1.3521, 103.8198,
  "Paris", 48.8566, 2.3522,
  "Hangzhou", 30.2741, 120.1551,
  "Moscow", 55.7558, 37.6173,
  "Dubai", 25.2048, 55.2708,
  "Shenzhen", 22.5431, 114.0579,
  "Guangzhou", 23.1291, 113.2644,
  "Nanjing", 32.0603, 118.7969,
  "Beijing", 39.9042, 116.4074
)

required_cols <- c(
  "origin", "destination", "shipping_method", "weather_condition",
  "supplier_reliability_score", "shipment_value", "shipping_distance",
  "delay_flag", "delay_hours", "risk_score", "financial_exposure",
  "weather_risk", "shipping_risk", "distance_risk", "supplier_risk"
)

safe_rescale <- function(x, to = c(0, 1)) {
  x <- as.numeric(x)
  if (length(x) == 0 || all(is.na(x))) {
    return(rep(mean(to), length(x)))
  }
  rng <- range(x, na.rm = TRUE)
  if (isTRUE(all.equal(rng[1], rng[2]))) {
    return(rep(mean(to), length(x)))
  }
  scales::rescale(x, to = to)
}

pick_data_source <- function(path = DEFAULT_DATA_PATH) {
  if (file.exists(path)) return(path)

  csv_fallback <- sub("\\.xlsx$", ".csv", path, ignore.case = TRUE)
  if (file.exists(csv_fallback)) return(csv_fallback)

  local_candidates <- c("Final_shipment.xlsx", "Final_shipment.csv")
  local_existing <- local_candidates[file.exists(local_candidates)]
  if (length(local_existing) > 0) {
    return(local_existing[1])
  }

  stop(
    "No shipment data file found. Expected one of:\n",
    "- ", path, "\n",
    "- ", csv_fallback, "\n",
    "- Final_shipment.xlsx / Final_shipment.csv in the app directory."
  )
}

load_shipment_data <- function(path = DEFAULT_DATA_PATH) {
  source_file <- pick_data_source(path)
  ext <- tolower(tools::file_ext(source_file))

  raw <- if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(source_file)
  } else if (ext == "csv") {
    readr::read_csv(source_file, show_col_types = FALSE)
  } else {
    stop("Unsupported file type: ", ext, ". Use .xlsx/.xls or .csv")
  }

  missing_cols <- setdiff(required_cols, names(raw))
  if (length(missing_cols) > 0) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  cleaned <- raw %>%
    mutate(
      shipment_id = row_number(),
      across(
        c(
          supplier_reliability_score, shipment_value, shipping_distance,
          delay_hours, risk_score, financial_exposure, weather_risk,
          shipping_risk, distance_risk, supplier_risk
        ),
        as.numeric
      ),
      across(c(origin, destination, shipping_method, weather_condition), as.character),
      delay_flag = case_when(
        as.character(delay_flag) %in% c("1", "TRUE", "T", "Delayed", "Yes", "Y") ~ "Delayed",
        TRUE ~ "On Time"
      ),
      risk_level = case_when(
        risk_score < 0.4 ~ "Low",
        risk_score >= 0.4 & risk_score < 0.7 ~ "Medium",
        TRUE ~ "High"
      ),
      risk_level = factor(risk_level, levels = c("Low", "Medium", "High")),
      route = paste(origin, destination, sep = " \u2192 "),
      departure_dt = suppressWarnings(lubridate::mdy_hm(departure_time)),
      expected_arrival_dt = suppressWarnings(lubridate::mdy_hm(expected_arrival_time)),
      actual_arrival_dt = suppressWarnings(lubridate::mdy_hm(actual_arrival_time))
    )

  if (all(is.na(cleaned$departure_dt))) {
    cleaned <- cleaned %>%
      mutate(
        departure_dt = suppressWarnings(lubridate::ymd_hms(departure_time, quiet = TRUE)),
        expected_arrival_dt = suppressWarnings(lubridate::ymd_hms(expected_arrival_time, quiet = TRUE)),
        actual_arrival_dt = suppressWarnings(lubridate::ymd_hms(actual_arrival_time, quiet = TRUE))
      )
  }

  origin_coords <- CITY_COORDS %>%
    rename(origin = city, origin_lat = lat, origin_lng = lng)
  destination_coords <- CITY_COORDS %>%
    rename(destination = city, dest_lat = lat, dest_lng = lng)

  cleaned %>%
    left_join(origin_coords, by = "origin") %>%
    left_join(destination_coords, by = "destination")
}

format_currency <- label_dollar(accuracy = 1)
format_number <- label_number(accuracy = 0, big.mark = ",")
format_risk <- label_number(accuracy = 0.01)

risk_badge <- function(level) {
  glue("<span class='risk-badge risk-{tolower(level)}'>{level}</span>")
}

kpi_card <- function(title, value, subtitle, icon_name = "chart-line") {
  div(
    class = "kpi-card",
    div(
      class = "kpi-head",
      span(class = "kpi-title", title),
      span(class = "kpi-icon", icon(icon_name))
    ),
    div(class = "kpi-value", value),
    div(class = "kpi-subtitle", subtitle)
  )
}

chartOutput <- function(output_id, height = NULL) {
  if (HAS_PLOTLY) {
    plotly::plotlyOutput(output_id, height = height)
  } else {
    plotOutput(output_id, height = height)
  }
}

renderChart <- function(expr) {
  if (HAS_PLOTLY) {
    plotly::renderPlotly(expr)
  } else {
    renderPlot(expr, res = 110)
  }
}

finalize_chart <- function(p, tooltip = "text") {
  if (HAS_PLOTLY) {
    plotly::ggplotly(p, tooltip = tooltip) %>%
      plotly::config(displayModeBar = FALSE)
  } else {
    p
  }
}

mapWidgetOutput <- function(output_id, height = NULL) {
  if (HAS_LEAFLET) {
    leaflet::leafletOutput(output_id, height = height)
  } else {
    plotOutput(output_id, height = height)
  }
}

renderMapWidget <- function(expr) {
  if (HAS_LEAFLET) {
    leaflet::renderLeaflet(expr)
  } else {
    renderPlot(expr, res = 110)
  }
}

overview_ui <- function() {
  div(
    class = "tab-stack",
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      uiOutput("kpi_total_shipments"),
      uiOutput("kpi_high_risk"),
      uiOutput("kpi_total_exposure"),
      uiOutput("kpi_avg_risk")
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        class = "rr-card",
        card_header("Risk Level Distribution"),
        chartOutput("overview_risk_dist", height = 290)
      ),
      card(
        class = "rr-card",
        card_header("Shipping Method Distribution"),
        chartOutput("overview_method_dist", height = 290)
      ),
      card(
        class = "rr-card",
        card_header("Weather Impact on Average Risk"),
        chartOutput("overview_weather_risk", height = 290)
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "rr-card",
        card_header("Top High-Risk Shipments"),
        DTOutput("tbl_high_risk")
      ),
      card(
        class = "rr-card",
        card_header("Top Financial Exposure"),
        DTOutput("tbl_top_exposure")
      )
    )
  )
}

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

risk_monitor_ui <- function() {
  div(
    class = "tab-stack",
    card(
      class = "rr-card",
      card_header("Operational Filters"),
      layout_columns(
        col_widths = c(2, 2, 2, 2, 2, 2),
        selectizeInput("filter_origin", "Origin", choices = NULL, multiple = TRUE),
        selectizeInput("filter_destination", "Destination", choices = NULL, multiple = TRUE),
        selectizeInput("filter_method", "Shipping Method", choices = NULL, multiple = TRUE),
        selectizeInput("filter_weather", "Weather Condition", choices = NULL, multiple = TRUE),
        selectizeInput("filter_risk", "Risk Level", choices = c("Low", "Medium", "High"), multiple = TRUE),
        selectInput("filter_delay", "Delay Flag", choices = c("All", "Delayed", "On Time"), selected = "All")
      )
    ),
    card(
      class = "rr-card",
      card_header("Shipment Risk Queue"),
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

warehouse_ui <- function() {
  div(
    class = "tab-stack",
    card(
      class = "rr-card placeholder-note",
      strong("Warehouse & Historical Analytics scaffold"),
      p(
        "This section is intentionally structured for future warehouse feeds and time-series integrations.",
        "Current visuals are data-driven where possible and include placeholders for expansion."
      )
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        class = "rr-card",
        card_header("A. Warehouse Utilization"),
        chartOutput("warehouse_utilization_plot", height = 310)
      ),
      card(
        class = "rr-card",
        card_header("B. Predicted Capacity Stress"),
        uiOutput("capacity_stress_cards")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "rr-card",
        card_header("C. Historical Route Reliability"),
        chartOutput("route_reliability_trend", height = 300)
      ),
      card(
        class = "rr-card",
        card_header("D. Delay Trend Over Time"),
        chartOutput("delay_trend_plot", height = 300)
      )
    ),
    card(
      class = "rr-card",
      card_header("E. Route Reliability Leaderboard"),
      DTOutput("route_leaderboard")
    )
  )
}

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

ui <- tagList(
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
          border: 1px solid var(--border-soft);
          border-radius: 14px;
          box-shadow: 0 2px 8px rgba(17, 24, 39, 0.04);
          background: #FFFFFF;
        }

        .rr-card > .card-header {
          background: #FFFFFF;
          border-bottom: 1px solid #F1F3F5;
          font-weight: 600;
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

server <- function(input, output, session) {
  data_rv <- reactiveVal(load_shipment_data(DEFAULT_DATA_PATH))

  shipments <- reactive({
    req(data_rv())
    data_rv()
  })

  output$kpi_total_shipments <- renderUI({
    d <- shipments()
    kpi_card(
      "Total Shipments",
      format_number(nrow(d)),
      "Monitored across active routes",
      "boxes-stacked"
    )
  })

  output$kpi_high_risk <- renderUI({
    d <- shipments()
    high_n <- sum(d$risk_level == "High", na.rm = TRUE)
    high_pct <- percent(high_n / max(1, nrow(d)), accuracy = 0.1)
    kpi_card(
      "High-Risk Shipments",
      format_number(high_n),
      glue("{high_pct} of total shipment volume"),
      "triangle-exclamation"
    )
  })

  output$kpi_total_exposure <- renderUI({
    d <- shipments()
    kpi_card(
      "Total Financial Exposure",
      format_currency(sum(d$financial_exposure, na.rm = TRUE)),
      "Aggregate value at risk",
      "sack-dollar"
    )
  })

  output$kpi_avg_risk <- renderUI({
    d <- shipments()
    kpi_card(
      "Average Risk Score",
      format_risk(mean(d$risk_score, na.rm = TRUE)),
      "Portfolio-wide risk intensity",
      "chart-line"
    )
  })

  output$overview_risk_dist <- renderChart({
    d <- shipments() %>%
      count(risk_level, .drop = FALSE)

    p <- ggplot(d, aes(x = risk_level, y = n, fill = risk_level, text = paste("Shipments:", comma(n)))) +
      geom_col(width = 0.65) +
      scale_fill_manual(values = RISK_COLORS) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      )

    finalize_chart(p, tooltip = "text")
  })

  output$overview_method_dist <- renderChart({
    d <- shipments() %>%
      count(shipping_method) %>%
      mutate(shipping_method = fct_reorder(shipping_method, n))

    p <- ggplot(
      d,
      aes(
        x = shipping_method,
        y = n,
        fill = shipping_method,
        text = paste(shipping_method, "-", comma(n), "shipments")
      )
    ) +
      geom_col(width = 0.65, show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = METHOD_COLORS) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()
      )

    finalize_chart(p, tooltip = "text")
  })

  output$overview_weather_risk <- renderChart({
    d <- shipments() %>%
      group_by(weather_condition) %>%
      summarise(avg_risk = mean(risk_score, na.rm = TRUE), .groups = "drop") %>%
      mutate(weather_condition = fct_reorder(weather_condition, avg_risk))

    p <- ggplot(
      d,
      aes(
        x = weather_condition,
        y = avg_risk,
        fill = avg_risk,
        text = paste(weather_condition, "<br>Average risk:", round(avg_risk, 2))
      )
    ) +
      geom_col(width = 0.65) +
      scale_fill_gradient(low = "#E7EFD3", high = "#6E7E31") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      )

    finalize_chart(p, tooltip = "text")
  })

  output$tbl_high_risk <- renderDT({
    d <- shipments() %>%
      mutate(risk_level_badge = map_chr(as.character(risk_level), risk_badge)) %>%
      arrange(desc(risk_score), desc(financial_exposure)) %>%
      transmute(
        `Shipment ID` = shipment_id,
        Origin = origin,
        Destination = destination,
        Method = shipping_method,
        Weather = weather_condition,
        `Risk Score` = round(risk_score, 2),
        `Risk Level` = risk_level_badge,
        `Financial Exposure` = financial_exposure,
        Delay = delay_flag
      ) %>%
      slice_head(n = 12)

    datatable(
      d,
      rownames = FALSE,
      escape = FALSE,
      options = list(dom = "tip", pageLength = 8, autoWidth = TRUE)
    ) %>%
      formatCurrency("Financial Exposure", currency = "$", interval = 3, mark = ",", digits = 0)
  })

  output$tbl_top_exposure <- renderDT({
    d <- shipments() %>%
      mutate(risk_level_badge = map_chr(as.character(risk_level), risk_badge)) %>%
      arrange(desc(financial_exposure), desc(risk_score)) %>%
      transmute(
        `Shipment ID` = shipment_id,
        Origin = origin,
        Destination = destination,
        Method = shipping_method,
        `Shipment Value` = shipment_value,
        `Financial Exposure` = financial_exposure,
        `Risk Score` = round(risk_score, 2),
        `Risk Level` = risk_level_badge
      ) %>%
      slice_head(n = 12)

    datatable(
      d,
      rownames = FALSE,
      escape = FALSE,
      options = list(dom = "tip", pageLength = 8, autoWidth = TRUE)
    ) %>%
      formatCurrency(c("Shipment Value", "Financial Exposure"), currency = "$", interval = 3, mark = ",", digits = 0)
  })

  map_data <- reactive({
    shipments() %>%
      filter(!is.na(origin_lat), !is.na(origin_lng), !is.na(dest_lat), !is.na(dest_lng))
  })

  selected_map_shipment <- reactiveVal(NULL)

  output$route_map <- renderMapWidget({
    d <- map_data()
    req(nrow(d) > 0)

    if (HAS_LEAFLET) {
      pal <- colorFactor(RISK_COLORS, domain = c("Low", "Medium", "High"), ordered = TRUE)

      icons <- list(
        Truck = makeAwesomeIcon(icon = "truck", markerColor = "blue", iconColor = "white", library = "fa"),
        Sea = makeAwesomeIcon(icon = "ship", markerColor = "cadetblue", iconColor = "white", library = "fa"),
        Air = makeAwesomeIcon(icon = "plane", markerColor = "green", iconColor = "white", library = "fa"),
        Rail = makeAwesomeIcon(icon = "train", markerColor = "orange", iconColor = "white", library = "fa")
      )

      m <- leaflet(d) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 80, lat = 28, zoom = 2) %>%
        addCircleMarkers(
          data = distinct(d, origin, origin_lat, origin_lng),
          lng = ~origin_lng,
          lat = ~origin_lat,
          radius = 5,
          fillOpacity = 0.9,
          stroke = FALSE,
          fillColor = "#2C3E50",
          label = ~paste("Origin:", origin)
        ) %>%
        addPolylines(
          lng = ~c(origin_lng, dest_lng),
          lat = ~c(origin_lat, dest_lat),
          color = ~pal(risk_level),
          weight = 2.1,
          opacity = 0.48,
          layerId = ~paste0("route_", shipment_id),
          label = ~paste(
            origin, "\u2192", destination,
            "|", shipping_method,
            "| Risk:", round(risk_score, 2),
            "| Exposure:", dollar(financial_exposure)
          )
        )

      for (method in names(icons)) {
        subset_method <- d %>% filter(shipping_method == method)
        if (nrow(subset_method) > 0) {
          m <- m %>%
            addAwesomeMarkers(
              data = subset_method,
              lng = ~dest_lng,
              lat = ~dest_lat,
              icon = icons[[method]],
              layerId = ~paste0("dest_", shipment_id),
              label = ~paste(
                destination,
                "|", shipping_method,
                "| Risk:", round(risk_score, 2),
                "| Exposure:", dollar(financial_exposure)
              ),
              popup = ~paste0(
                "<b>Shipment #", shipment_id, "</b><br>",
                "<b>Route:</b> ", origin, " \u2192 ", destination, "<br>",
                "<b>Method:</b> ", shipping_method, "<br>",
                "<b>Weather:</b> ", weather_condition, "<br>",
                "<b>Risk Score:</b> ", round(risk_score, 2), "<br>",
                "<b>Exposure:</b> ", dollar(financial_exposure), "<br>",
                "<b>Delay:</b> ", delay_flag, " (", round(delay_hours, 1), "h)"
              ),
              clusterOptions = markerClusterOptions()
            )
        }
      }

      m %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~risk_level,
          title = "Route Risk",
          opacity = 0.9
        )
    } else {
      ggplot(
        d,
        aes(
          x = origin_lng,
          y = origin_lat,
          xend = dest_lng,
          yend = dest_lat,
          color = risk_level
        )
      ) +
        geom_segment(alpha = 0.3, linewidth = 0.6) +
        geom_point(
          aes(x = dest_lng, y = dest_lat, shape = shipping_method),
          size = 2.1,
          alpha = 0.85
        ) +
        scale_color_manual(values = RISK_COLORS) +
        scale_shape_manual(values = c(Truck = 15, Sea = 16, Air = 17, Rail = 18)) +
        coord_quickmap(xlim = c(-10, 150), ylim = c(-5, 62)) +
        labs(x = NULL, y = NULL, color = "Risk Level", shape = "Method") +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#ECEFF3"),
          legend.position = "bottom"
        )
    }
  })

  if (HAS_LEAFLET) {
    observeEvent(input$route_map_shape_click, {
      clicked <- input$route_map_shape_click
      if (is.null(clicked$id)) return()
      id_val <- str_remove(clicked$id, "^(route_|dest_)")
      selected_map_shipment(as.integer(id_val))
    })
  }

  output$map_transport_summary <- renderChart({
    d <- map_data() %>%
      count(shipping_method) %>%
      mutate(shipping_method = fct_reorder(shipping_method, n))

    p <- ggplot(
      d,
      aes(
        x = shipping_method,
        y = n,
        fill = shipping_method,
        text = paste(shipping_method, "-", comma(n), "routes")
      )
    ) +
      geom_col(width = 0.6, show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = METHOD_COLORS) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )

    finalize_chart(p, tooltip = "text")
  })

  output$map_risk_summary <- renderDT({
    d <- map_data() %>%
      group_by(risk_level) %>%
      summarise(
        Routes = n(),
        `Avg Delay (hrs)` = mean(delay_hours, na.rm = TRUE),
        `Avg Exposure` = mean(financial_exposure, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(match(risk_level, c("Low", "Medium", "High")))

    datatable(
      d,
      rownames = FALSE,
      options = list(dom = "t", ordering = FALSE, autoWidth = TRUE)
    ) %>%
      formatRound("Avg Delay (hrs)", 1) %>%
      formatCurrency("Avg Exposure", currency = "$", digits = 0)
  })

  output$risk_legend_widget <- renderUI({
    tagList(
      div(class = "legend-row", span(class = "legend-dot", style = paste0("background:", RISK_COLORS["Low"])), "Low Risk"),
      div(class = "legend-row", span(class = "legend-dot", style = paste0("background:", RISK_COLORS["Medium"])), "Medium Risk"),
      div(class = "legend-row", span(class = "legend-dot", style = paste0("background:", RISK_COLORS["High"])), "High Risk"),
      tags$hr(),
      div(class = "legend-row", icon("truck"), "Truck"),
      div(class = "legend-row", icon("ship"), "Sea"),
      div(class = "legend-row", icon("plane"), "Air"),
      div(class = "legend-row", icon("train"), "Rail")
    )
  })

  output$map_route_detail <- renderUI({
    d <- map_data()
    if (nrow(d) == 0) {
      return(div("No routes available for mapping."))
    }

    selected_id <- selected_map_shipment()
    rec <- if (is.null(selected_id)) {
      d %>% arrange(desc(risk_score), desc(financial_exposure)) %>% slice(1)
    } else {
      d %>% filter(shipment_id == selected_id) %>% slice(1)
    }

    if (nrow(rec) == 0) {
      return(div("Click a route or destination marker to inspect shipment details."))
    }

    tagList(
      div(
        class = "detail-grid",
        div(class = "detail-item", div(class = "detail-label", "Shipment ID"), div(class = "detail-value", rec$shipment_id)),
        div(class = "detail-item", div(class = "detail-label", "Route"), div(class = "detail-value", rec$route)),
        div(class = "detail-item", div(class = "detail-label", "Shipping Method"), div(class = "detail-value", rec$shipping_method)),
        div(class = "detail-item", div(class = "detail-label", "Weather"), div(class = "detail-value", rec$weather_condition)),
        div(class = "detail-item", div(class = "detail-label", "Risk Score"), div(class = "detail-value", round(rec$risk_score, 2))),
        div(class = "detail-item", div(class = "detail-label", "Financial Exposure"), div(class = "detail-value", dollar(rec$financial_exposure))),
        div(class = "detail-item", div(class = "detail-label", "Delay Flag"), div(class = "detail-value", rec$delay_flag)),
        div(class = "detail-item", div(class = "detail-label", "Delay Hours"), div(class = "detail-value", round(rec$delay_hours, 1)))
      )
    )
  })

  observeEvent(shipments(), {
    d <- shipments()
    updateSelectizeInput(session, "filter_origin", choices = sort(unique(d$origin)), selected = sort(unique(d$origin)), server = TRUE)
    updateSelectizeInput(session, "filter_destination", choices = sort(unique(d$destination)), selected = sort(unique(d$destination)), server = TRUE)
    updateSelectizeInput(session, "filter_method", choices = sort(unique(d$shipping_method)), selected = sort(unique(d$shipping_method)), server = TRUE)
    updateSelectizeInput(session, "filter_weather", choices = sort(unique(d$weather_condition)), selected = sort(unique(d$weather_condition)), server = TRUE)
    updateSelectizeInput(session, "filter_risk", choices = c("Low", "Medium", "High"), selected = c("Low", "Medium", "High"), server = TRUE)
  }, ignoreInit = FALSE)

  filtered_shipments <- reactive({
    d <- shipments()
    req(length(input$filter_origin) > 0, length(input$filter_destination) > 0, length(input$filter_method) > 0, length(input$filter_weather) > 0, length(input$filter_risk) > 0)

    d <- d %>%
      filter(
        origin %in% input$filter_origin,
        destination %in% input$filter_destination,
        shipping_method %in% input$filter_method,
        weather_condition %in% input$filter_weather,
        as.character(risk_level) %in% input$filter_risk
      )

    if (input$filter_delay != "All") {
      d <- d %>% filter(delay_flag == input$filter_delay)
    }

    d
  })

  risk_table_data <- reactive({
    filtered_shipments() %>%
      mutate(risk_level_badge = map_chr(as.character(risk_level), risk_badge)) %>%
      transmute(
        `Shipment ID` = shipment_id,
        Origin = origin,
        Destination = destination,
        Method = shipping_method,
        Weather = weather_condition,
        `Risk Score` = round(risk_score, 2),
        `Risk Level` = risk_level_badge,
        `Financial Exposure` = financial_exposure,
        `Delay Flag` = delay_flag
      )
  })

  output$risk_table <- renderDT({
    datatable(
      risk_table_data(),
      rownames = FALSE,
      escape = FALSE,
      selection = "single",
      options = list(pageLength = 12, autoWidth = TRUE)
    ) %>%
      formatCurrency("Financial Exposure", currency = "$", digits = 0)
  })

  selected_monitor_row <- reactive({
    idx <- input$risk_table_rows_selected
    table_view <- filtered_shipments()
    if (length(idx) == 0 || nrow(table_view) == 0) return(NULL)
    table_view[idx[1], ]
  })

  output$shipment_detail_card <- renderUI({
    rec <- selected_monitor_row()
    if (is.null(rec)) {
      return(div("Select a shipment row to inspect its risk and financial profile."))
    }

    tagList(
      div(
        class = "detail-grid",
        div(class = "detail-item", div(class = "detail-label", "Shipment ID"), div(class = "detail-value", rec$shipment_id)),
        div(class = "detail-item", div(class = "detail-label", "Risk Level"), div(class = "detail-value", HTML(risk_badge(as.character(rec$risk_level))))),
        div(class = "detail-item", div(class = "detail-label", "Risk Score"), div(class = "detail-value", round(rec$risk_score, 2))),
        div(class = "detail-item", div(class = "detail-label", "Financial Exposure"), div(class = "detail-value", dollar(rec$financial_exposure))),
        div(class = "detail-item", div(class = "detail-label", "Delay Hours"), div(class = "detail-value", round(rec$delay_hours, 1))),
        div(class = "detail-item", div(class = "detail-label", "Supplier Reliability"), div(class = "detail-value", percent(rec$supplier_reliability_score, accuracy = 0.1)))
      )
    )
  })

  output$risk_driver_plot <- renderChart({
    rec <- selected_monitor_row()
    req(!is.null(rec))

    drivers <- tibble(
      driver = c("Weather Risk", "Shipping Risk", "Distance Risk", "Supplier Risk", "Delay Signal"),
      value = c(
        rec$weather_risk, rec$shipping_risk, rec$distance_risk,
        rec$supplier_risk, if_else(rec$delay_flag == "Delayed", 1, 0)
      )
    ) %>%
      mutate(driver = fct_reorder(driver, value))

    p <- ggplot(
      drivers,
      aes(
        x = driver,
        y = value,
        fill = value,
        text = paste(driver, ":", round(value, 2))
      )
    ) +
      geom_col(width = 0.6) +
      coord_flip() +
      scale_fill_gradient(low = "#E9F0D8", high = "#6E7E31") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )

    finalize_chart(p, tooltip = "text")
  })

  output$kpi_financial_total <- renderUI({
    d <- shipments()
    kpi_card(
      "Total Financial Exposure",
      dollar(sum(d$financial_exposure, na.rm = TRUE)),
      "Potential impact under current risk profile",
      "building-shield"
    )
  })

  output$kpi_financial_avg <- renderUI({
    d <- shipments()
    kpi_card(
      "Average Exposure per Shipment",
      dollar(mean(d$financial_exposure, na.rm = TRUE)),
      "Mean value at risk per route movement",
      "chart-column"
    )
  })

  output$financial_scatter <- renderChart({
    d <- shipments()

    p <- ggplot(
      d,
      aes(
        x = shipment_value,
        y = risk_score,
        color = risk_level,
        size = financial_exposure,
        text = paste0(
          "Shipment #", shipment_id,
          "<br>Route: ", route,
          "<br>Shipment Value: ", dollar(shipment_value),
          "<br>Risk Score: ", round(risk_score, 2),
          "<br>Exposure: ", dollar(financial_exposure)
        )
      )
    ) +
      geom_point(alpha = 0.7) +
      scale_color_manual(values = RISK_COLORS) +
      scale_size_continuous(range = c(2.5, 11)) +
      labs(x = "Shipment Value", y = "Risk Score", color = "Risk Level", size = "Exposure") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())

    finalize_chart(p, tooltip = "text")
  })

  output$financial_distribution <- renderChart({
    d <- shipments()

    p <- ggplot(
      d,
      aes(
        x = financial_exposure,
        fill = risk_level,
        text = paste("Exposure:", dollar(financial_exposure))
      )
    ) +
      geom_histogram(bins = 30, alpha = 0.9, position = "identity") +
      scale_fill_manual(values = RISK_COLORS) +
      labs(x = "Financial Exposure", y = "Shipments", fill = "Risk Level") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())

    finalize_chart(p, tooltip = "text")
  })

  output$financial_top_table <- renderDT({
    d <- shipments() %>%
      mutate(risk_level_badge = map_chr(as.character(risk_level), risk_badge)) %>%
      arrange(desc(financial_exposure), desc(risk_score)) %>%
      transmute(
        `Shipment ID` = shipment_id,
        Route = route,
        `Shipment Value` = shipment_value,
        `Risk Score` = round(risk_score, 2),
        `Risk Level` = risk_level_badge,
        `Financial Exposure` = financial_exposure
      ) %>%
      slice_head(n = 10)

    datatable(
      d,
      rownames = FALSE,
      escape = FALSE,
      options = list(dom = "tip", pageLength = 10, ordering = FALSE)
    ) %>%
      formatCurrency(c("Shipment Value", "Financial Exposure"), currency = "$", digits = 0)
  })

  warehouse_base <- reactive({
    d <- shipments() %>%
      group_by(destination) %>%
      summarise(
        throughput = sum(shipment_value, na.rm = TRUE),
        avg_risk = mean(risk_score, na.rm = TRUE),
        delay_rate = mean(delay_flag == "Delayed", na.rm = TRUE),
        shipments = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(throughput)) %>%
      slice_head(n = 6) %>%
      mutate(
        warehouse = paste(destination, "Hub"),
        utilization = pmin(98, 45 + safe_rescale(throughput, c(10, 40)) + delay_rate * 25),
        stress_level = case_when(
          utilization >= 85 ~ "High",
          utilization >= 70 ~ "Medium",
          TRUE ~ "Low"
        )
      )

    d
  })

  output$warehouse_utilization_plot <- renderChart({
    d <- warehouse_base() %>%
      mutate(warehouse = fct_reorder(warehouse, utilization))

    p <- ggplot(
      d,
      aes(
        x = warehouse,
        y = utilization,
        fill = stress_level,
        text = paste(
          warehouse,
          "<br>Utilization:", round(utilization, 1), "%",
          "<br>Shipments:", comma(shipments)
        )
      )
    ) +
      geom_col(width = 0.65) +
      coord_flip() +
      scale_fill_manual(values = RISK_COLORS) +
      labs(x = NULL, y = "Utilization (%)", fill = "Stress") +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()
      )

    finalize_chart(p, tooltip = "text")
  })

  output$capacity_stress_cards <- renderUI({
    d <- warehouse_base() %>%
      arrange(desc(utilization)) %>%
      slice_head(n = 3)

    tagList(
      lapply(seq_len(nrow(d)), function(i) {
        row <- d[i, ]
        div(
          class = "stress-card",
          div(class = "stress-title", paste0(row$warehouse, " Capacity Stress")),
          div(class = "stress-value", paste0(round(row$utilization, 1), "%")),
          div(
            class = "stress-title",
            paste0(
              "Predicted stress: ", row$stress_level,
              " | Delay Rate: ", percent(row$delay_rate, accuracy = 0.1)
            )
          )
        )
      })
    )
  })

  historical_base <- reactive({
    d <- shipments()
    if (all(is.na(d$departure_dt))) {
      d <- d %>% mutate(departure_dt = as.POSIXct("2024-01-01", tz = "UTC") + row_number() * 3600 * 10)
    }
    d
  })

  output$route_reliability_trend <- renderChart({
    d <- historical_base() %>%
      mutate(month = floor_date(departure_dt, unit = "month")) %>%
      group_by(month) %>%
      summarise(
        reliability = 1 - mean(delay_flag == "Delayed", na.rm = TRUE),
        avg_risk = mean(risk_score, na.rm = TRUE),
        .groups = "drop"
      )

    p <- ggplot(
      d,
      aes(
        x = month,
        y = reliability,
        text = paste(
          format(month, "%b %Y"),
          "<br>Reliability:", percent(reliability, accuracy = 0.1),
          "<br>Avg risk:", round(avg_risk, 2)
        )
      )
    ) +
      geom_line(color = "#6E7E31", linewidth = 1.1) +
      geom_point(color = "#6E7E31", size = 2.2) +
      scale_y_continuous(labels = percent_format()) +
      labs(x = NULL, y = "Reliability") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())

    finalize_chart(p, tooltip = "text")
  })

  output$delay_trend_plot <- renderChart({
    d <- historical_base() %>%
      mutate(week = floor_date(departure_dt, unit = "week")) %>%
      group_by(week) %>%
      summarise(
        delay_rate = mean(delay_flag == "Delayed", na.rm = TRUE),
        shipments = n(),
        .groups = "drop"
      )

    p <- ggplot(
      d,
      aes(
        x = week,
        y = delay_rate,
        text = paste(
          format(week, "%d %b %Y"),
          "<br>Delay Rate:", percent(delay_rate, accuracy = 0.1),
          "<br>Shipments:", comma(shipments)
        )
      )
    ) +
      geom_line(color = "#C6473B", linewidth = 1.1) +
      geom_point(color = "#C6473B", size = 1.9) +
      scale_y_continuous(labels = percent_format()) +
      labs(x = NULL, y = "Delay Rate") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())

    finalize_chart(p, tooltip = "text")
  })

  output$route_leaderboard <- renderDT({
    d <- historical_base() %>%
      group_by(route) %>%
      summarise(
        Shipments = n(),
        `Delay Rate` = mean(delay_flag == "Delayed", na.rm = TRUE),
        `Avg Risk` = mean(risk_score, na.rm = TRUE),
        `Avg Exposure` = mean(financial_exposure, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(`Delay Rate`), desc(`Avg Risk`)) %>%
      slice_head(n = 10)

    datatable(
      d,
      rownames = FALSE,
      options = list(dom = "tip", ordering = FALSE)
    ) %>%
      formatPercentage("Delay Rate", digits = 1) %>%
      formatRound("Avg Risk", digits = 2) %>%
      formatCurrency("Avg Exposure", currency = "$", digits = 0)
  })

  compose_copilot_reply <- function(question, d, wh) {
    q <- str_to_lower(question)

    if (str_detect(q, "highest risk of delay|delay.*highest risk|highest risk.*delay")) {
      top_delay <- d %>%
        mutate(delay_signal = if_else(delay_flag == "Delayed", 1, 0)) %>%
        arrange(desc(delay_signal), desc(risk_score), desc(financial_exposure)) %>%
        slice_head(n = 5)

      lines <- paste0(
        "- #", top_delay$shipment_id, " ", top_delay$route,
        " | risk ", round(top_delay$risk_score, 2),
        " | exposure ", dollar(top_delay$financial_exposure)
      )
      return(paste("Top shipments with immediate delay risk:", paste(lines, collapse = "\n")))
    }

    if (str_detect(q, "routes.*vulnerable|most vulnerable|vulnerable route|which routes")) {
      top_routes <- d %>%
        group_by(route) %>%
        summarise(
          delay_rate = mean(delay_flag == "Delayed", na.rm = TRUE),
          avg_risk = mean(risk_score, na.rm = TRUE),
          exposure = mean(financial_exposure, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(vulnerability_index = 0.45 * delay_rate + 0.35 * safe_rescale(avg_risk) + 0.20 * safe_rescale(exposure)) %>%
        arrange(desc(vulnerability_index)) %>%
        slice_head(n = 3)

      lines <- paste0(
        "- ", top_routes$route,
        " | delay ", percent(top_routes$delay_rate, accuracy = 0.1),
        " | avg risk ", round(top_routes$avg_risk, 2)
      )
      return(paste("Most vulnerable routes right now:", paste(lines, collapse = "\n")))
    }

    if (str_detect(q, "driving shipment risk|factors|drivers")) {
      drivers <- d %>%
        summarise(
          weather = mean(weather_risk, na.rm = TRUE),
          shipping = mean(shipping_risk, na.rm = TRUE),
          distance = mean(distance_risk, na.rm = TRUE),
          supplier = mean(supplier_risk, na.rm = TRUE),
          delay = mean(delay_flag == "Delayed", na.rm = TRUE)
        ) %>%
        pivot_longer(everything(), names_to = "driver", values_to = "score") %>%
        arrange(desc(score))

      lines <- paste0("- ", str_to_title(drivers$driver), ": ", round(drivers$score, 2))
      return(paste("Top drivers of risk in the current shipment portfolio:", paste(lines, collapse = "\n")))
    }

    if (str_detect(q, "highest financial exposure|highest exposure|financial risk")) {
      top_fin <- d %>%
        arrange(desc(financial_exposure), desc(risk_score)) %>%
        slice_head(n = 5)
      lines <- paste0(
        "- #", top_fin$shipment_id, " ", top_fin$route,
        " | exposure ", dollar(top_fin$financial_exposure),
        " | risk ", round(top_fin$risk_score, 2)
      )
      return(paste("Highest financial exposure shipments:", paste(lines, collapse = "\n")))
    }

    if (str_detect(q, "warehouse|stress|capacity")) {
      top_wh <- wh %>% arrange(desc(utilization)) %>% slice(1)
      return(
        paste0(
          top_wh$warehouse, " is currently under the highest predicted stress at ",
          round(top_wh$utilization, 1), "% utilization. ",
          "Delay rate is ", percent(top_wh$delay_rate, accuracy = 0.1),
          " and average route risk is ", round(top_wh$avg_risk, 2), "."
        )
      )
    }

    "I can help with shipment risk, route vulnerability, delay signals, exposure prioritization, and warehouse stress. Please ask a logistics risk question tied to this dashboard context."
  }

  chat_log <- reactiveVal(
    tibble(
      role = "assistant",
      text = "Ask about delays, vulnerable routes, risk drivers, or financial exposure to get a concise operational briefing."
    )
  )

  append_chat <- function(role, text) {
    chat_log(bind_rows(chat_log(), tibble(role = role, text = text)))
  }

  submit_copilot_query <- function(query_text) {
    q <- str_squish(query_text)
    if (!nzchar(q)) return()
    append_chat("user", q)
    answer <- compose_copilot_reply(q, shipments(), warehouse_base())
    append_chat("assistant", answer)
  }

  observeEvent(input$ask_copilot, {
    submit_copilot_query(input$copilot_query)
    updateTextInput(session, "copilot_query", value = "")
  })

  observeEvent(input$q_delay, {
    prompt <- "Which shipments are at highest risk of delay?"
    updateTextInput(session, "copilot_query", value = prompt)
    submit_copilot_query(prompt)
  })

  observeEvent(input$q_routes, {
    prompt <- "Which routes are most vulnerable?"
    updateTextInput(session, "copilot_query", value = prompt)
    submit_copilot_query(prompt)
  })

  observeEvent(input$q_drivers, {
    prompt <- "What factors are driving shipment risk?"
    updateTextInput(session, "copilot_query", value = prompt)
    submit_copilot_query(prompt)
  })

  observeEvent(input$q_exposure, {
    prompt <- "Which shipments create the highest financial exposure?"
    updateTextInput(session, "copilot_query", value = prompt)
    submit_copilot_query(prompt)
  })

  observeEvent(input$q_warehouse, {
    prompt <- "Which warehouse is under the most stress?"
    updateTextInput(session, "copilot_query", value = prompt)
    submit_copilot_query(prompt)
  })

  output$copilot_chat <- renderUI({
    log_tbl <- chat_log()
    tagList(
      lapply(seq_len(nrow(log_tbl)), function(i) {
        row <- log_tbl[i, ]
        div(
          class = paste("chat-msg", if_else(row$role == "user", "chat-user", "chat-assistant")),
          div(class = "chat-role", if_else(row$role == "user", "You", "RiskRoute AI Copilot")),
          div(class = "chat-bubble", row$text)
        )
      })
    )
  })
}

shinyApp(ui, server)
