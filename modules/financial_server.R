# Financial Risk Page Server Logic

financial_server <- function(input, output, session, shipments) {
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
}
