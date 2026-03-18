# Warehouse & Historical Analytics Page Server Logic

warehouse_server <- function(input, output, session, shipments, historical_shipments, warehouse_status_by_cargo) {
  stress_palette <- c(
    "Normal" = "#8FA85E",
    "Medium Stress" = "#C8A24A",
    "High Stress" = "#C25B4A"
  )

  hub_stress_label <- function(x) {
    case_when(
      is.na(x) ~ "Normal",
      x > 85 ~ "High Stress",
      x >= 70 ~ "Medium Stress",
      TRUE ~ "Normal"
    )
  }

  stress_tag_style <- function(label) {
    switch(
      label,
      "High Stress" = "background:#FCE9E6;color:#9E3126;border:1px solid #F5CDC7;",
      "Medium Stress" = "background:#FFF4DF;color:#9A6A08;border:1px solid #F1D9A6;",
      "Normal" = "background:#EAF4EC;color:#1E6A2B;border:1px solid #CFE7D3;",
      "background:#EEF3E2;color:#3B4A2A;border:1px solid #D9E2C3;"
    )
  }

  window_days <- reactive({
    raw_input <- input$wh_time_range
    raw <- as.character(if (is.null(raw_input)) "90" else raw_input)
    if (tolower(raw) %in% c("all", "all history", "0")) return(0)

    days <- suppressWarnings(as.numeric(raw))
    if (is.na(days)) {
      # Fallback for cases where UI returns labels like "Last 30 days"
      num <- suppressWarnings(as.numeric(gsub("[^0-9]", "", raw)))
      days <- ifelse(is.na(num), 90, num)
    }
    days
  })

  top_n <- reactive({
    n <- suppressWarnings(as.integer(input$wh_top_n))
    if (is.na(n)) 6 else max(3, min(12, n))
  })

  min_route_shipments <- reactive({
    n <- suppressWarnings(as.integer(input$wh_min_route_shipments))
    if (is.na(n)) 5 else max(1, n)
  })

  window_label <- reactive({
    days <- window_days()
    if (days <= 0) "All history" else paste0("Last ", days, " days")
  })

  shipments_filtered <- reactive({
    d <- shipments()
    days <- window_days()
    d <- d %>% filter(!is.na(destination), nzchar(destination))
    if (!"departure_dt" %in% names(d) || days <= 0 || all(is.na(d$departure_dt))) return(d)

    cutoff <- max(d$departure_dt, na.rm = TRUE) - lubridate::days(days)
    d %>% filter(is.na(departure_dt) | departure_dt >= cutoff)
  })

  warehouse_base <- reactive({
    shipments_filtered() %>%
      group_by(destination) %>%
      summarise(
        throughput = sum(shipment_value, na.rm = TRUE),
        avg_risk = mean(risk_score, na.rm = TRUE),
        delay_rate = mean(delay_flag == "Delayed", na.rm = TRUE),
        shipments = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(throughput)) %>%
      slice_head(n = top_n()) %>%
      mutate(
        hub = paste(destination, "Hub"),
        warehouse = hub,
        utilization = pmin(98, 45 + safe_rescale(throughput, c(10, 40)) + delay_rate * 25),
        predicted_utilization = pmin(99, utilization + safe_rescale(avg_risk, c(2, 10))),
        utilization_band = hub_stress_label(utilization),
        stress_level = hub_stress_label(predicted_utilization)
      )
  })

  historical_base <- reactive({
    d <- historical_shipments()
    if (all(is.na(d$departure_dt))) {
      d <- d %>% mutate(departure_dt = as.POSIXct("2024-01-01", tz = "UTC") + row_number() * 3600 * 10)
    }

    if (!"route" %in% names(d)) {
      d <- d %>% mutate(route = paste(origin, destination, sep = " -> "))
    }

    d %>%
      mutate(route = as.character(route)) %>%
      filter(!is.na(route), nzchar(route))
  })

  historical_filtered <- reactive({
    d <- historical_base()
    days <- window_days()

    if (days <= 0 || all(is.na(d$departure_dt))) return(d)

    cutoff <- max(d$departure_dt, na.rm = TRUE) - lubridate::days(days)
    d %>% filter(is.na(departure_dt) | departure_dt >= cutoff)
  })

  historical_for_charts <- reactive({
    d <- historical_filtered()
    if (nrow(d) == 0) return(d)

    route_counts <- d %>% count(route, name = "route_n", sort = TRUE)
    eligible_routes <- route_counts %>%
      filter(route_n >= min_route_shipments()) %>%
      slice_head(n = top_n()) %>%
      pull(route)

    d %>% filter(route %in% eligible_routes)
  })

  apply_route_filters <- function(df) {
    if (nrow(df) == 0) return(df)
    route_counts <- df %>% count(route, name = "route_n", sort = TRUE)
    eligible_routes <- route_counts %>%
      filter(route_n >= min_route_shipments()) %>%
      slice_head(n = top_n()) %>%
      pull(route)
    df %>% filter(route %in% eligible_routes)
  }

  summarize_routes <- function(hist_df) {
    if (nrow(hist_df) == 0) return(tibble())

    monthly <- hist_df %>%
      mutate(month = floor_date(departure_dt, unit = "month")) %>%
      group_by(route, month) %>%
      summarise(delay_rate = mean(delay_flag == "Delayed", na.rm = TRUE), .groups = "drop")

    trend_tbl <- monthly %>%
      arrange(route, month) %>%
      group_by(route) %>%
      summarise(
        delay_rate_trend = {
          vals <- delay_rate[is.finite(delay_rate)]
          if (length(vals) >= 2) vals[length(vals)] - vals[length(vals) - 1] else NA_real_
        },
        .groups = "drop"
      )

    hist_df %>%
      group_by(route) %>%
      summarise(
        Shipments = n(),
        `Delay Rate` = mean(delay_flag == "Delayed", na.rm = TRUE),
        `Avg Risk` = mean(risk_score, na.rm = TRUE),
        `Avg Exposure` = mean(financial_exposure, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(Shipments >= min_route_shipments()) %>%
      left_join(trend_tbl, by = "route") %>%
      mutate(
        `Trend (MoM)` = case_when(
          is.na(delay_rate_trend) ~ "Flat",
          delay_rate_trend > 0.03 ~ paste0("Worsening (+", percent(delay_rate_trend, accuracy = 0.1), ")"),
          delay_rate_trend < -0.03 ~ paste0("Improving (", percent(delay_rate_trend, accuracy = 0.1), ")"),
          TRUE ~ "Flat"
        ),
        Severity = case_when(
          `Delay Rate` >= 0.35 | `Avg Risk` >= 0.70 ~ "High",
          `Delay Rate` >= 0.20 | `Avg Risk` >= 0.50 ~ "Medium",
          TRUE ~ "Low"
        ),
        `Recommended Action` = case_when(
          `Delay Rate` > 0.30 ~ "Re-route + build safety stock",
          `Delay Rate` > 0.25 ~ "Escalate carrier + monitor hub capacity",
          TRUE ~ "Monitor"
        )
      ) %>%
      arrange(desc(`Delay Rate`), desc(`Avg Risk`), desc(Shipments))
  }

  kpi_metrics <- reactive({
    hist <- historical_filtered()
    wh <- warehouse_base()

    delay_rate <- mean(hist$delay_flag == "Delayed", na.rm = TRUE)
    if (!is.finite(delay_rate)) delay_rate <- 0

    reliability <- 1 - delay_rate
    if (!is.finite(reliability)) reliability <- 0

    high_stress <- sum(wh$stress_level == "High Stress", na.rm = TRUE)
    if (!is.finite(high_stress)) high_stress <- 0

    list(
      shipments = nrow(hist),
      delay_rate = delay_rate,
      reliability = reliability,
      high_stress = high_stress
    )
  })

  output$wh_kpi_shipments <- renderUI({
    m <- kpi_metrics()
    kpi_card("Shipments In Scope", comma(m$shipments), window_label(), "boxes-stacked")
  })

  output$wh_kpi_delay_rate <- renderUI({
    m <- kpi_metrics()
    kpi_card("Delay Rate", percent(m$delay_rate, accuracy = 0.1), "Unified definition: delayed / total", "triangle-exclamation")
  })

  output$wh_kpi_reliability <- renderUI({
    m <- kpi_metrics()
    kpi_card("Route Reliability", percent(m$reliability, accuracy = 0.1), "Unified definition: 1 - delay rate", "shield")
  })

  output$wh_kpi_high_stress <- renderUI({
    m <- kpi_metrics()
    kpi_card("High Stress Hubs", comma(m$high_stress), "Predicted utilization > 85%", "warehouse")
  })

  output$wh_filter_state <- renderUI({
    d <- historical_for_charts()
    min_dt <- if (nrow(d) > 0) suppressWarnings(min(d$departure_dt, na.rm = TRUE)) else NA
    max_dt <- if (nrow(d) > 0) suppressWarnings(max(d$departure_dt, na.rm = TRUE)) else NA

    div(
      style = "margin-top: 0.3rem; font-size: 0.9rem; color: #5b6570;",
      HTML(
        paste0(
          "<strong>Applied:</strong> ",
          window_label(),
          " | Top Hubs=", top_n(),
          " | Min Route Shipments=", min_route_shipments(),
          " | C/D rows=", nrow(d),
          " | C/D range=",
          if (is.finite(min_dt)) format(min_dt, "%Y-%m-%d") else "NA",
          " to ",
          if (is.finite(max_dt)) format(max_dt, "%Y-%m-%d") else "NA"
        )
      )
    )
  })

  output$warehouse_utilization_plot <- renderChart({
    d <- warehouse_base() %>%
      mutate(
        hub = as.character(hub),
        utilization = as.numeric(utilization),
        predicted_utilization = as.numeric(predicted_utilization),
        shipments = as.numeric(shipments),
        utilization_band = as.character(utilization_band)
      ) %>%
      filter(!is.na(hub), nzchar(hub), !is.na(utilization)) %>%
      arrange(desc(utilization)) %>%
      distinct(hub, .keep_all = TRUE)

    validate(need(nrow(d) > 0, "No hub data for current filters."))

    d <- d %>%
      mutate(
        hub = forcats::fct_reorder(factor(hub), utilization, .na_rm = TRUE)
      )

    p <- ggplot(
      d,
      aes(
        x = hub,
        y = utilization,
        fill = utilization_band
      )
    ) +
      geom_col(width = 0.48) +
      geom_hline(yintercept = 85, color = "#9E3126", linetype = "dashed", linewidth = 0.8) +
      coord_flip() +
      scale_fill_manual(values = stress_palette, breaks = c("High Stress", "Medium Stress", "Normal")) +
      labs(x = NULL, y = "Utilization (%)", fill = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 10)
      )

    finalize_chart(p)
  })

  output$capacity_stress_cards <- renderUI({
    d <- warehouse_base() %>%
      arrange(desc(predicted_utilization))

    if (nrow(d) == 0) {
      return(div(class = "placeholder-note", p("No stress-card data for current filters.")))
    }

    tagList(
      lapply(seq_len(nrow(d)), function(i) {
        row <- d[i, ]
        gap_vs_threshold <- ifelse(is.finite(row$predicted_utilization), sprintf("%+.1f%%", row$predicted_utilization - 85), "NA")
        div(
          class = "stress-card hub-stress-card",
          div(class = "stress-title", row$hub),
          div(class = "stress-value", paste0(round(row$predicted_utilization, 1), "%")),
          div(
            class = "hub-metric-row",
            div(
              class = "hub-metric",
              div(class = "hub-metric-label", "Current Utilization"),
              div(class = "hub-metric-value", percent(row$utilization / 100, accuracy = 0.1))
            ),
            div(
              class = "hub-metric",
              div(class = "hub-metric-label", "Delay Rate"),
              div(class = "hub-metric-value", percent(row$delay_rate, accuracy = 0.1))
            ),
            div(
              class = "hub-metric",
              div(class = "hub-metric-label", "Gap vs Threshold"),
              div(class = "hub-metric-value", gap_vs_threshold)
            )
          ),
          div(
            class = "hub-stress-tag",
            style = stress_tag_style(row$stress_level),
            row$stress_level
          )
        )
      })
    )
  })

  output$route_reliability_trend <- renderChart({
    d <- historical_filtered() %>%
      mutate(route = if_else(is.na(route) | !nzchar(route), paste(origin, destination, sep = " \u2192 "), route)) %>%
      group_by(route) %>%
      summarise(
        shipments = n(),
        delay_rate = mean(delay_flag == "Delayed", na.rm = TRUE),
        reliability = 1 - delay_rate,
        avg_risk = mean(risk_score, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(shipments >= min_route_shipments()) %>%
      arrange(desc(shipments)) %>%
      slice_head(n = max(8, top_n()))

    validate(need(nrow(d) > 0, "No route reliability data for current filters."))

    p <- ggplot(
      d,
      aes(
        x = shipments,
        y = reliability,
        color = delay_rate
      )
    ) +
      geom_point(size = 3.4, alpha = 0.8) +
      scale_color_gradient(low = "#8FA85E", high = "#C25B4A", guide = "none") +
      scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
      labs(x = "Shipment Volume", y = "Route Reliability (%)") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())

    finalize_chart(p)
  })

  output$delay_trend_plot <- renderChart({
    d <- historical_filtered() %>%
      mutate(week = floor_date(departure_dt, unit = "week")) %>%
      group_by(week) %>%
      summarise(
        delay_rate = mean(delay_flag == "Delayed", na.rm = TRUE),
        shipments = n(),
        .groups = "drop"
      ) %>%
      arrange(week)

    if (nrow(d) > 12) {
      d <- tail(d, 12)
    }

    validate(need(nrow(d) > 0, "No delay trend data for current filters."))

    p <- ggplot(
      d,
      aes(
        x = week,
        y = delay_rate,
        group = 1
      )
    ) +
      geom_line(color = "#C6473B", linewidth = 1.05) +
      geom_point(color = "#C6473B", size = 2) +
      scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
      labs(x = "Week", y = "Delay Rate (%)") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())

    if (nrow(d) >= 4) {
      p <- p + geom_smooth(
        data = d,
        mapping = aes(x = week, y = delay_rate, group = 1),
        inherit.aes = FALSE,
        method = "loess",
        se = FALSE,
        color = "#9E3126",
        linetype = "dashed",
        linewidth = 0.9
      )
    }

    finalize_chart(p)
  })

  route_leaderboard_data <- reactive({
    current_hist <- historical_for_charts()
    current_tbl <- summarize_routes(current_hist) %>%
      mutate(curr_rank = row_number())

    prev_tbl <- tibble(route = character(), prev_rank = integer())
    wd <- window_days()
    base_hist <- historical_base()

    if (wd > 0 && nrow(base_hist) > 0 && !all(is.na(base_hist$departure_dt))) {
      max_dt <- max(base_hist$departure_dt, na.rm = TRUE)
      curr_start <- max_dt - lubridate::days(wd)
      prev_start <- curr_start - lubridate::days(wd)
      prev_hist <- base_hist %>% filter(departure_dt > prev_start, departure_dt <= curr_start)
      prev_hist <- apply_route_filters(prev_hist)
      prev_tbl <- summarize_routes(prev_hist) %>%
        mutate(prev_rank = row_number()) %>%
        select(route, prev_rank)
    }

    current_tbl %>%
      left_join(prev_tbl, by = "route") %>%
      mutate(
        `Rank Delta` = case_when(
          is.na(prev_rank) ~ "<span style='color:#6B7280;'>&bull; New</span>",
          prev_rank > curr_rank ~ paste0("<span style='color:#1E6A2B;'>&uarr; ", prev_rank - curr_rank, "</span>"),
          prev_rank < curr_rank ~ paste0("<span style='color:#9E3126;'>&darr; ", curr_rank - prev_rank, "</span>"),
          TRUE ~ "<span style='color:#6B7280;'>&rarr; 0</span>"
        )
      ) %>%
      select(route, `Rank Delta`, Shipments, `Delay Rate`, `Avg Risk`, `Avg Exposure`, `Trend (MoM)`, Severity, `Recommended Action`) %>%
      slice_head(n = 10)
  })

  output$route_leaderboard <- renderDT({
    d <- route_leaderboard_data()

    validate(need(nrow(d) > 0, "No route leaderboard rows for current filters."))
    top_route <- d$route[[1]]

    datatable(
      d,
      rownames = FALSE,
      escape = FALSE,
      selection = list(mode = "single", target = "row", selected = 1),
      options = list(
        dom = "tip",
        ordering = TRUE,
        order = list(list(3, "desc")),
        pageLength = 10
      )
    ) %>%
      formatPercentage("Delay Rate", digits = 1) %>%
      formatRound("Avg Risk", digits = 2) %>%
      formatCurrency("Avg Exposure", currency = "$", digits = 0) %>%
      formatStyle(
        "Severity",
        backgroundColor = styleEqual(
          c("High", "Medium", "Low"),
          c("#FCE9E6", "#FFF4DF", "#EAF4EC")
        ),
        color = styleEqual(
          c("High", "Medium", "Low"),
          c("#9E3126", "#9A6A08", "#1E6A2B")
        ),
        fontWeight = "700"
      ) %>%
      formatStyle(
        "route",
        target = "row",
        backgroundColor = styleEqual(top_route, "#FDECEA"),
        fontWeight = styleEqual(top_route, "700")
      )
  }, server = FALSE)

  selected_route <- reactive({
    d <- route_leaderboard_data()
    validate(need(nrow(d) > 0, "No route selected."))

    idx <- input$route_leaderboard_rows_selected
    if (length(idx) == 1 && idx >= 1 && idx <= nrow(d)) {
      return(as.character(d$route[idx]))
    }
    as.character(d$route[1])
  })

  route_drilldown_weekly <- reactive({
    r <- selected_route()
    d <- historical_filtered() %>%
      filter(route == r) %>%
      mutate(week = floor_date(departure_dt, unit = "week")) %>%
      group_by(week) %>%
      summarise(
        delay_rate = mean(delay_flag == "Delayed", na.rm = TRUE),
        shipments = n(),
        avg_risk = mean(risk_score, na.rm = TRUE),
        avg_exposure = mean(financial_exposure, na.rm = TRUE),
        avg_delay_hours = mean(delay_hours, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(week)

    if (nrow(d) > 12) d <- tail(d, 12)
    d
  })

  route_drilldown_summary <- reactive({
    d <- route_drilldown_weekly()
    r <- selected_route()
    route_destination <- trimws(sub("^.*(\u2192|->)", "", r))
    hub_row <- warehouse_base() %>%
      filter(destination == route_destination) %>%
      slice_head(n = 1)
    hub_utilization <- if (nrow(hub_row) > 0) as.numeric(hub_row$utilization[[1]]) else NA_real_

    if (nrow(d) == 0) {
      return(list(
        route = r,
        trend_summary = "No trend data available for current filters.",
        risk_summary = "No route-level risk summary available.",
        action_text = "No action available.",
        avg_exposure = NA_real_
      ))
    }

    current_delay <- mean(tail(d$delay_rate, 4), na.rm = TRUE)
    prior_delay <- if (nrow(d) > 4) mean(head(tail(d$delay_rate, 8), 4), na.rm = TRUE) else NA_real_
    delta <- current_delay - prior_delay
    avg_risk <- mean(d$avg_risk, na.rm = TRUE)
    avg_exposure <- mean(d$avg_exposure, na.rm = TRUE)

    trend_summary <- if (is.finite(delta)) {
      if (delta > 0.03) paste0("Worsening by ", percent(delta, accuracy = 0.1))
      else if (delta < -0.03) paste0("Improving by ", percent(abs(delta), accuracy = 0.1))
      else "Stable"
    } else {
      "Insufficient prior weeks"
    }

    risk_summary <- paste0(
      "Delay Rate (4w avg): ", percent(current_delay, accuracy = 0.1),
      " | Avg Risk Score: ", number(avg_risk, accuracy = 0.01),
      " | Avg Exposure: ", dollar(avg_exposure, accuracy = 1),
      " | Hub Utilization: ", ifelse(is.finite(hub_utilization), percent(hub_utilization / 100, accuracy = 0.1), "NA")
    )

    action_text <- if (is.finite(current_delay) && current_delay > 0.30 && is.finite(hub_utilization) && hub_utilization > 85) {
      "Re-route shipments and increase safety stock."
    } else if (is.finite(current_delay) && current_delay > 0.25) {
      "Escalate carrier and monitor capacity."
    } else {
      "Maintain current routing and continue monitoring."
    }

    list(
      route = r,
      trend_summary = trend_summary,
      risk_summary = risk_summary,
      avg_exposure = avg_exposure,
      action_text = action_text
    )
  })

  output$route_drilldown_trend <- renderChart({
    d <- route_drilldown_weekly()
    r <- selected_route()
    metric <- ifelse(isTruthy(input$wh_route_metric), input$wh_route_metric, "delay_rate")
    y_col <- if (metric == "avg_risk") "avg_risk" else "delay_rate"
    y_label <- if (metric == "avg_risk") "Risk Score" else "Delay Rate (%)"
    y_color <- if (metric == "avg_risk") "#6E7E31" else "#C6473B"
    validate(need(nrow(d) > 0, "Select a route from leaderboard to view trend."))

    p <- ggplot(
      d,
      aes(
        x = week,
        y = .data[[y_col]],
        group = 1
      )
    ) +
      geom_line(color = y_color, linewidth = 1.1) +
      geom_point(color = y_color, size = 2) +
      labs(x = "Week", y = y_label, subtitle = r) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())

    if (metric == "delay_rate") {
      p <- p + scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1))
    } else {
      p <- p + scale_y_continuous(labels = number_format(accuracy = 0.01))
    }

    finalize_chart(p)
  })

  output$route_drilldown_insights <- renderUI({
    d <- route_drilldown_weekly()
    s <- route_drilldown_summary()
    if (nrow(d) == 0) {
      return(div(class = "placeholder-note", p("Select a route from leaderboard to see insights.")))
    }

    tagList(
      div(class = "stress-card", div(class = "stress-title", "Selected Route"), div(class = "stress-value", s$route)),
      div(class = "stress-card", div(class = "stress-title", "Trend Summary"), div(class = "stress-value", s$trend_summary)),
      div(class = "stress-card", div(class = "stress-title", "Risk Summary"), div(class = "stress-value", s$risk_summary)),
      div(class = "stress-card", div(class = "stress-title", "Recommended Action"), div(class = "stress-value", s$action_text))
    )
  })

  output$download_route_trend_csv <- downloadHandler(
    filename = function() {
      paste0("route_trend_", gsub("[^A-Za-z0-9]+", "_", selected_route()), "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      readr::write_csv(route_drilldown_weekly(), file)
    }
  )

  output$download_route_insight_txt <- downloadHandler(
    filename = function() {
      paste0("route_insights_", gsub("[^A-Za-z0-9]+", "_", selected_route()), "_", format(Sys.Date(), "%Y%m%d"), ".txt")
    },
    content = function(file) {
      s <- route_drilldown_summary()
      lines <- c(
        paste("Selected Route:", s$route),
        paste("Trend Summary:", s$trend_summary),
        paste("Risk Summary:", s$risk_summary),
        paste("Average Financial Exposure:", dollar(s$avg_exposure, accuracy = 1)),
        paste("Recommended Action:", s$action_text)
      )
      writeLines(lines, con = file)
    }
  )

  # Return warehouse_base for use in copilot
  warehouse_base
}
