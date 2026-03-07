# Warehouse & Historical Analytics Page Server Logic

warehouse_server <- function(input, output, session, shipments, historical_shipments, warehouse_status_by_cargo) {
  warehouse_base <- reactive({
    wh <- warehouse_status_by_cargo()

    if (!is.null(wh) && nrow(wh) > 0) {
      d <- wh %>%
        mutate(
          utilization = as.numeric(utilization),
          predicted_utilization = as.numeric(predicted_utilization),
          throughput = as.numeric(throughput),
          avg_risk = as.numeric(avg_risk),
          delay_rate = as.numeric(delay_rate),
          shipments = as.numeric(shipments)
        ) %>%
        mutate(
          utilization = if_else(!is.na(utilization), pmin(pmax(utilization, 0), 100), NA_real_),
          predicted_utilization = if_else(!is.na(predicted_utilization), pmin(pmax(predicted_utilization, 0), 100), utilization),
          stress_level = case_when(
            predicted_utilization >= 85 ~ "High",
            predicted_utilization >= 70 ~ "Medium",
            TRUE ~ "Low"
          )
        ) %>%
        arrange(desc(predicted_utilization), desc(utilization)) %>%
        slice_head(n = 6)

      if (nrow(d) > 0) return(d)
    }

    shipments() %>%
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
        predicted_utilization = pmin(99, utilization + safe_rescale(avg_risk, c(2, 10))),
        stress_level = case_when(
          predicted_utilization >= 85 ~ "High",
          predicted_utilization >= 70 ~ "Medium",
          TRUE ~ "Low"
        )
      )
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
          "<br>Predicted:", round(predicted_utilization, 1), "%",
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
      arrange(desc(predicted_utilization)) %>%
      slice_head(n = 3)

    tagList(
      lapply(seq_len(nrow(d)), function(i) {
        row <- d[i, ]
        div(
          class = "stress-card",
          div(class = "stress-title", paste0(row$warehouse, " Capacity Stress")),
          div(class = "stress-value", paste0(round(row$predicted_utilization, 1), "%")),
          div(
            class = "stress-title",
            paste0(
              "Predicted stress: ", row$stress_level,
              " | Current Utilization: ", percent(row$utilization / 100, accuracy = 0.1),
              " | Delay Rate: ", percent(row$delay_rate, accuracy = 0.1)
            )
          )
        )
      })
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

  # Return warehouse_base for use in copilot
  warehouse_base
}
