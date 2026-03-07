# Overview Page Server Logic

overview_server <- function(input, output, session, shipments) {
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
}
