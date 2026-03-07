# Risk Monitor Page Server Logic

risk_monitor_server <- function(input, output, session, shipments) {
  preset_mode <- reactiveVal("none")

  all_choices <- reactive({
    d <- shipments()
    list(
      origin = sort(unique(d$origin)),
      destination = sort(unique(d$destination)),
      method = sort(unique(d$shipping_method)),
      weather = sort(unique(d$weather_condition)),
      risk = c("Low", "Medium", "High")
    )
  })

  observeEvent(shipments(), {
    ch <- all_choices()
    updateSelectizeInput(session, "filter_origin", choices = ch$origin, selected = character(0))
    updateSelectizeInput(session, "filter_destination", choices = ch$destination, selected = character(0))
    updateSelectizeInput(session, "filter_method", choices = ch$method, selected = character(0))
    updateSelectizeInput(session, "filter_weather", choices = ch$weather, selected = character(0))
    updateSelectizeInput(session, "filter_risk", choices = ch$risk, selected = character(0))
  }, ignoreInit = FALSE)

  observeEvent(input$rm_select_all, {
    # Empty multi-select is interpreted as "include all", which keeps the UI compact.
    updateSelectizeInput(session, "filter_origin", selected = character(0))
    updateSelectizeInput(session, "filter_destination", selected = character(0))
    updateSelectizeInput(session, "filter_method", selected = character(0))
    updateSelectizeInput(session, "filter_weather", selected = character(0))
    updateSelectizeInput(session, "filter_risk", selected = character(0))
    updateSelectInput(session, "filter_delay", selected = "All")
    preset_mode("all")
  })

  observeEvent(input$rm_clear_all, {
    updateSelectizeInput(session, "filter_origin", selected = character(0))
    updateSelectizeInput(session, "filter_destination", selected = character(0))
    updateSelectizeInput(session, "filter_method", selected = character(0))
    updateSelectizeInput(session, "filter_weather", selected = character(0))
    updateSelectizeInput(session, "filter_risk", selected = character(0))
    updateSelectInput(session, "filter_delay", selected = "All")
    preset_mode("none")
  })

  observeEvent(input$rm_preset_critical, {
    updateSelectizeInput(session, "filter_origin", selected = character(0))
    updateSelectizeInput(session, "filter_destination", selected = character(0))
    updateSelectizeInput(session, "filter_method", selected = character(0))
    updateSelectizeInput(session, "filter_weather", selected = character(0))
    updateSelectizeInput(session, "filter_risk", selected = "High")
    updateSelectInput(session, "filter_delay", selected = "Delayed")
    updateTextInput(session, "rm_search", value = "")
    preset_mode("critical")
  })

  observeEvent(input$rm_preset_delayed, {
    updateSelectInput(session, "filter_delay", selected = "Delayed")
    updateTextInput(session, "rm_search", value = "")
    preset_mode("delayed")
  })

  observeEvent(input$rm_preset_exposure, {
    updateSelectInput(session, "filter_delay", selected = "All")
    updateSelectizeInput(session, "filter_origin", selected = character(0))
    updateSelectizeInput(session, "filter_destination", selected = character(0))
    updateSelectizeInput(session, "filter_method", selected = character(0))
    updateSelectizeInput(session, "filter_weather", selected = character(0))
    updateSelectizeInput(session, "filter_risk", selected = character(0))
    updateTextInput(session, "rm_search", value = "")
    preset_mode("high_exposure")
  })

  filter_inputs <- reactive({
    list(
      origin = input$filter_origin,
      destination = input$filter_destination,
      method = input$filter_method,
      weather = input$filter_weather,
      risk = input$filter_risk,
      delay = input$filter_delay,
      search = input$rm_search
    )
  })
  filter_inputs_debounced <- debounce(filter_inputs, 120)

  filtered_shipments <- reactive({
    fi <- filter_inputs_debounced()
    d <- shipments()
    if (length(fi$origin) > 0) {
      d <- d %>% filter(origin %in% fi$origin)
    }
    if (length(fi$destination) > 0) {
      d <- d %>% filter(destination %in% fi$destination)
    }
    if (length(fi$method) > 0) {
      d <- d %>% filter(shipping_method %in% fi$method)
    }
    if (length(fi$weather) > 0) {
      d <- d %>% filter(weather_condition %in% fi$weather)
    }
    if (length(fi$risk) > 0) {
      d <- d %>% filter(as.character(risk_level) %in% fi$risk)
    }

    if (!is.null(fi$delay) && fi$delay != "All") {
      d <- d %>% filter(delay_flag == fi$delay)
    }

    search_raw <- fi$search
    search_q <- trimws(if (is.null(search_raw)) "" else as.character(search_raw))
    if (nzchar(search_q)) {
      q <- tolower(search_q)
      d <- d %>%
        mutate(
          rm_search_blob = paste(
            shipment_id, origin, destination, route, shipping_method,
            sep = " "
          )
        ) %>%
        filter(grepl(q, tolower(rm_search_blob), fixed = TRUE)) %>%
        select(-rm_search_blob)
    }

    if (identical(preset_mode(), "high_exposure") && nrow(d) > 0) {
      cutoff <- quantile(d$financial_exposure, probs = 0.90, na.rm = TRUE)
      d <- d %>% filter(financial_exposure >= cutoff)
    }

    d
  })

  output$rm_filter_summary <- renderUI({
    d <- filtered_shipments()
    delayed_n <- sum(d$delay_flag == "Delayed", na.rm = TRUE)
    div(
      style = "margin-top:0.45rem; margin-bottom:0.25rem; font-size:0.92rem; color:#4b5563;",
      HTML(
        paste0(
          "<strong>Matched:</strong> ", comma(nrow(d)),
          " shipments",
          " | <strong>Delayed:</strong> ", comma(delayed_n),
          " | <strong>Avg Risk:</strong> ", number(mean(d$risk_score, na.rm = TRUE), accuracy = 0.01),
          " | <strong>Preset:</strong> ", gsub("_", " ", preset_mode())
        )
      )
    )
  })

  monitor_kpis <- reactive({
    d <- filtered_shipments()
    delay_rate <- mean(d$delay_flag == "Delayed", na.rm = TRUE)
    if (!is.finite(delay_rate)) delay_rate <- 0

    avg_risk <- mean(d$risk_score, na.rm = TRUE)
    if (!is.finite(avg_risk)) avg_risk <- 0

    total_exposure <- sum(d$financial_exposure, na.rm = TRUE)
    if (!is.finite(total_exposure)) total_exposure <- 0

    list(
      shipments = nrow(d),
      avg_risk = avg_risk,
      total_exposure = total_exposure,
      delay_rate = delay_rate
    )
  })

  output$rm_kpi_shipments <- renderUI({
    m <- monitor_kpis()
    kpi_card("Shipments In Scope", comma(m$shipments), "After active filters", "boxes-stacked")
  })

  output$rm_kpi_avg_risk <- renderUI({
    m <- monitor_kpis()
    kpi_card("Average Risk", number(m$avg_risk, accuracy = 0.01), "Portfolio risk intensity", "triangle-exclamation")
  })

  output$rm_kpi_exposure <- renderUI({
    m <- monitor_kpis()
    kpi_card("Total Exposure", dollar(m$total_exposure, accuracy = 1), "Sum of filtered shipments", "sack-dollar")
  })

  output$rm_kpi_delay_rate <- renderUI({
    m <- monitor_kpis()
    kpi_card("Delayed Rate", percent(m$delay_rate, accuracy = 0.1), "Delayed / total", "clock")
  })

  risk_table_base <- reactive({
    filtered_shipments() %>%
      mutate(
        delay_bucket = case_when(
          delay_flag == "On Time" | delay_hours <= 0 ~ "On Time",
          delay_hours < 6 ~ "<6h",
          delay_hours < 24 ~ "6-24h",
          TRUE ~ "24h+"
        )
      ) %>%
      arrange(desc(risk_score), desc(financial_exposure)) %>%
      mutate(risk_rank = row_number())
  })

  risk_table_data <- reactive({
    risk_table_base() %>%
      mutate(risk_level_badge = map_chr(as.character(risk_level), risk_badge)) %>%
      transmute(
        `Risk Rank` = risk_rank,
        `Shipment ID` = shipment_id,
        Origin = origin,
        Destination = destination,
        Method = shipping_method,
        Weather = weather_condition,
        `Risk Score` = round(risk_score, 2),
        `Risk Level` = risk_level_badge,
        `Financial Exposure` = financial_exposure,
        `Delay Flag` = delay_flag,
        `Delay Bucket` = delay_bucket
      )
  })

  output$risk_table <- renderDT({
    datatable(
      risk_table_data(),
      rownames = FALSE,
      escape = FALSE,
      class = "stripe hover compact",
      selection = "single",
      options = list(pageLength = 12, autoWidth = TRUE, ordering = FALSE)
    ) %>%
      formatCurrency("Financial Exposure", currency = "$", digits = 0) %>%
      formatStyle(
        "Risk Score",
        backgroundColor = styleInterval(c(0.5, 0.75), c("#EAF4EC", "#FFF4DF", "#FCE9E6"))
      ) %>%
      formatStyle(
        "Delay Bucket",
        backgroundColor = styleEqual(c("24h+"), c("#FCE9E6"))
      )
  })

  selected_monitor_row <- reactive({
    idx <- input$risk_table_rows_selected
    table_view <- risk_table_base()
    if (length(idx) == 0 || nrow(table_view) == 0) return(NULL)
    table_view[idx[1], ]
  })

  output$shipment_detail_card <- renderUI({
    rec <- selected_monitor_row()
    if (is.null(rec)) {
      return(div("Select a shipment row to inspect its risk and financial profile."))
    }

    route_view <- filtered_shipments() %>%
      filter(route == rec$route) %>%
      mutate(week = floor_date(departure_dt, unit = "week")) %>%
      group_by(week) %>%
      summarise(risk = mean(risk_score, na.rm = TRUE), .groups = "drop") %>%
      arrange(week)

    risk_delta_text <- "Insufficient weekly history"
    if (nrow(route_view) >= 2) {
      last_risk <- tail(route_view$risk, 1)
      prev_risk <- tail(route_view$risk, 2)[1]
      delta <- last_risk - prev_risk
      risk_delta_text <- if (delta > 0.02) {
        paste0("Worsening by +", number(delta, accuracy = 0.01), " vs last week")
      } else if (delta < -0.02) {
        paste0("Improving by ", number(delta, accuracy = 0.01), " vs last week")
      } else {
        "Stable vs last week"
      }
    }

    tagList(
      div(
        class = "detail-grid",
        div(class = "detail-item", div(class = "detail-label", "Shipment ID"), div(class = "detail-value", rec$shipment_id)),
        div(class = "detail-item", div(class = "detail-label", "Risk Level"), div(class = "detail-value", HTML(risk_badge(as.character(rec$risk_level))))),
        div(class = "detail-item", div(class = "detail-label", "Risk Score"), div(class = "detail-value", round(rec$risk_score, 2))),
        div(class = "detail-item", div(class = "detail-label", "Financial Exposure"), div(class = "detail-value", dollar(rec$financial_exposure))),
        div(class = "detail-item", div(class = "detail-label", "Delay Hours"), div(class = "detail-value", round(rec$delay_hours, 1))),
        div(class = "detail-item", div(class = "detail-label", "Supplier Reliability"), div(class = "detail-value", percent(rec$supplier_reliability_score, accuracy = 0.1))),
        div(
          class = "detail-item",
          div(class = "detail-label", "Recommended Action"),
          div(
            class = "detail-value",
            case_when(
              rec$risk_score >= 0.75 || (rec$delay_flag == "Delayed" && rec$delay_hours >= 12) ~ "Immediate escalation to operations + carrier.",
              rec$risk_score >= 0.50 || rec$financial_exposure >= 200000 ~ "Priority follow-up within 48 hours.",
              TRUE ~ "Routine monitoring; no urgent intervention."
            )
          )
        )
      ),
      div(
        style = "margin-top:0.45rem; color:#4b5563; font-size:0.9rem;",
        HTML(
          paste0(
            "<strong>Timeline:</strong> ",
            format(rec$departure_dt, "%Y-%m-%d %H:%M"),
            " → ",
            format(rec$expected_arrival_dt, "%Y-%m-%d %H:%M"),
            " → ",
            format(rec$actual_arrival_dt, "%Y-%m-%d %H:%M"),
            " (",
            ifelse(rec$delay_flag == "Delayed", paste0("+", round(rec$delay_hours, 1), "h"), "On Time"),
            ")"
          )
        )
      ),
      div(style = "margin-top:0.45rem; color:#4b5563; font-size:0.9rem;", HTML(paste0("<strong>Route weekly change:</strong> ", risk_delta_text))),
      div(style = "margin-top:0.5rem; color:#6b7280; font-size:0.85rem;", "Threshold logic: high risk/long delay => immediate escalation.")
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
      mutate(
        share = if (sum(value, na.rm = TRUE) > 0) {
          value / sum(value, na.rm = TRUE)
        } else {
          0
        }
      ) %>%
      mutate(driver = fct_reorder(driver, value))

    p <- ggplot(
      drivers,
      aes(
        x = driver,
        y = value,
        fill = share,
        text = paste(driver, ":", round(value, 2), "<br>Contribution:", percent(share, accuracy = 0.1))
      )
    ) +
      geom_col(width = 0.6) +
      geom_hline(yintercept = 0.6, color = "#9E3126", linetype = "dashed", linewidth = 0.8) +
      coord_flip() +
      scale_fill_gradient(low = "#E9F0D8", high = "#6E7E31") +
      labs(x = NULL, y = "Driver Weight", caption = "Dashed line: elevated-risk threshold (0.6)") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )

    finalize_chart(p, tooltip = "text")
  })

  output$rm_export_queue <- downloadHandler(
    filename = function() paste0("risk_queue_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      export_df <- risk_table_base() %>%
        transmute(
          risk_rank,
          shipment_id,
          origin,
          destination,
          route,
          shipping_method,
          weather_condition,
          risk_score,
          risk_level = as.character(risk_level),
          financial_exposure,
          delay_flag,
          delay_hours,
          delay_bucket
        )
      readr::write_csv(export_df, file)
    }
  )
}
