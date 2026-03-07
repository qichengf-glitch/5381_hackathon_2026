# Overview Page Server Logic

overview_server <- function(input, output, session, shipments) {
  # Keep Overview self-contained: derive clean fields locally
  overview_data <- reactive({
    d <- shipments()
    req(is.data.frame(d), nrow(d) > 0)

    d %>%
      mutate(
        risk_score = as.numeric(risk_score),
        financial_exposure = as.numeric(financial_exposure),
        shipment_value = as.numeric(shipment_value),
        risk_level = case_when(
          risk_score < 0.4 ~ "Low",
          risk_score >= 0.4 & risk_score < 0.7 ~ "Medium",
          TRUE ~ "High"
        ),
        risk_level = factor(risk_level, levels = c("Low", "Medium", "High"))
      )
  })

  # Unified risk palette for the entire Risk Drivers Row
  risk_palette <- c(
    Low = "#BFCB96",    # light olive green
    Medium = "#C8A24A", # amber / muted gold
    High = "#C25B4A"    # muted red
  )

  output$kpi_total_shipments <- renderUI({
    d <- overview_data()
    kpi_card(
      "Total Shipments",
      scales::comma(nrow(d)),
      "Monitored across active routes",
      "boxes-stacked"
    )
  })

  output$kpi_high_risk <- renderUI({
    d <- overview_data()
    high_n <- sum(d$risk_level == "High", na.rm = TRUE)
    high_pct <- percent(high_n / max(1, nrow(d)), accuracy = 0.1)
    kpi_card(
      "High-Risk Shipments",
      scales::comma(high_n),
      glue("{high_pct} of total shipment volume"),
      "triangle-exclamation"
    )
  })

  output$kpi_total_exposure <- renderUI({
    d <- overview_data()
    kpi_card(
      "Total Financial Exposure",
      format_currency(sum(d$financial_exposure, na.rm = TRUE)),
      "Aggregate value at risk",
      "sack-dollar"
    )
  })

  output$kpi_avg_risk <- renderUI({
    d <- overview_data()
    kpi_card(
      "Average Risk Score",
      format_risk(mean(d$risk_score, na.rm = TRUE)),
      "Portfolio-wide risk intensity",
      "chart-line"
    )
  })

  output$overview_risk_dist <- renderChart({
    d <- overview_data() %>%
      count(risk_level, .drop = FALSE)

    p <- ggplot(d, aes(x = risk_level, y = n, fill = risk_level, text = paste("Shipments:", comma(n)))) +
      geom_col(width = 0.65) +
      geom_text(
        aes(label = comma(n)),
        vjust = -0.35,
        size = 3.8,
        color = "#3B3B3B"
      ) +
      scale_fill_manual(values = risk_palette) +
      scale_y_continuous(
        labels = scales::comma,
        expand = expansion(mult = c(0, 0.10))
      ) +
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
    # CHANGED: replace "shipping method count" with avg risk by method
    d <- overview_data() %>%
      group_by(shipping_method) %>%
      summarise(
        avg_risk = mean(risk_score, na.rm = TRUE),
        shipments_n = n(),
        .groups = "drop"
      ) %>%
      mutate(
        risk_level = case_when(
          avg_risk < 0.4 ~ "Low",
          avg_risk < 0.7 ~ "Medium",
          TRUE ~ "High"
        ),
        risk_level = factor(risk_level, levels = c("Low", "Medium", "High")),
        shipping_method = fct_reorder(shipping_method, avg_risk)
      )

    # Dynamic scale to make close risk values visually distinguishable
    min_r <- min(d$avg_risk, na.rm = TRUE)
    max_r <- max(d$avg_risk, na.rm = TRUE)
    pad <- max(0.02, (max_r - min_r) * 0.35)
    y_low <- max(0, min_r - pad)
    y_high <- min(1, max_r + pad)

    p <- ggplot(
      d,
      aes(
        x = shipping_method,
        y = avg_risk,
        fill = avg_risk,
        text = paste(
          shipping_method,
          "<br>Avg risk:", round(avg_risk, 2),
          "<br>Shipments:", comma(shipments_n),
          "<br>Risk band:", risk_level
        )
      )
    ) +
      geom_col(width = 0.65, show.legend = FALSE) +
      geom_text(
        aes(label = sprintf("%.2f", avg_risk)),
        hjust = -0.08,
        size = 3.6,
        color = "#3B3B3B"
      ) +
      coord_flip(ylim = c(y_low, y_high), clip = "off") +
      scale_fill_gradientn(colors = c("#BFCB96", "#C8A24A", "#C25B4A")) +
      scale_y_continuous(
        labels = scales::number_format(accuracy = 0.01)
      ) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(color = "#4B4B4B")
      )

    finalize_chart(p, tooltip = "text")
  })

  output$overview_weather_risk <- renderChart({
    # KEPT concept, CHANGED styling/title consistency for Risk Drivers Row
    d <- overview_data() %>%
      group_by(weather_condition) %>%
      summarise(
        avg_risk = mean(risk_score, na.rm = TRUE),
        shipments_n = n(),
        .groups = "drop"
      ) %>%
      mutate(
        risk_level = case_when(
          avg_risk < 0.4 ~ "Low",
          avg_risk < 0.7 ~ "Medium",
          TRUE ~ "High"
        ),
        risk_level = factor(risk_level, levels = c("Low", "Medium", "High")),
        weather_condition = fct_reorder(weather_condition, avg_risk)
      )

    # Dynamic scale to make close risk values visually distinguishable
    min_r <- min(d$avg_risk, na.rm = TRUE)
    max_r <- max(d$avg_risk, na.rm = TRUE)
    pad <- max(0.02, (max_r - min_r) * 0.35)
    y_low <- max(0, min_r - pad)
    y_high <- min(1, max_r + pad)

    p <- ggplot(
      d,
      aes(
        x = weather_condition,
        y = avg_risk,
        fill = avg_risk,
        text = paste(
          weather_condition,
          "<br>Avg risk:", round(avg_risk, 2),
          "<br>Shipments:", comma(shipments_n),
          "<br>Risk band:", risk_level
        )
      )
      ) +
      geom_col(width = 0.65) +
      geom_text(
        aes(label = sprintf("%.2f", avg_risk)),
        vjust = -0.35,
        size = 3.6,
        color = "#3B3B3B"
      ) +
      coord_cartesian(ylim = c(y_low, y_high), clip = "off") +
      scale_fill_gradientn(colors = c("#BFCB96", "#C8A24A", "#C25B4A")) +
      scale_y_continuous(
        labels = scales::number_format(accuracy = 0.01)
      ) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(face = "bold", color = "#4B4B4B"),
        axis.text.y = element_text(color = "#4B4B4B")
      )

    finalize_chart(p, tooltip = "text")
  })

  output$tbl_high_risk <- renderDT({
    # CHANGED: remove risk level column and emphasize risk score with red background
    d <- overview_data() %>%
      arrange(desc(risk_score), desc(financial_exposure)) %>%
      transmute(
        Origin = origin,
        Destination = destination,
        `Shipping Method` = shipping_method,
        `Risk Score` = round(risk_score, 2),
        `Financial Exposure` = financial_exposure
      ) %>%
      slice_head(n = 10)

    risk_cuts <- unique(as.numeric(stats::quantile(d$`Risk Score`, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)))
    risk_colors_full <- c("#FDECEA", "#FAD5D1", "#F3B3AA", "#E78F82")
    risk_colors <- risk_colors_full[seq_len(length(risk_cuts) + 1)]

    datatable(
      d,
      rownames = FALSE,
      escape = FALSE,
      class = "stripe hover compact",
      options = list(
        dom = "t",
        pageLength = 10,
        autoWidth = TRUE,
        ordering = FALSE
      )
    ) %>%
      formatCurrency("Financial Exposure", currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatStyle(
        "Risk Score",
        backgroundColor = styleInterval(risk_cuts, risk_colors),
        color = "#7F1D1D",
        fontWeight = "700"
      )
  })

  output$tbl_top_exposure <- renderDT({
    # CHANGED: simplified columns for faster homepage scanning
    d <- overview_data() %>%
      arrange(desc(financial_exposure), desc(risk_score)) %>%
      transmute(
        Origin = origin,
        Destination = destination,
        `Shipping Method` = shipping_method,
        `Shipment Value` = shipment_value,
        `Financial Exposure` = financial_exposure,
        `Risk Score` = round(risk_score, 2)
      ) %>%
      slice_head(n = 10)

    exposure_cuts <- unique(as.numeric(stats::quantile(d$`Financial Exposure`, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)))
    exposure_colors_full <- c("#FFFBE6", "#FEF2C7", "#FDE68A", "#FCD34D")
    exposure_colors <- exposure_colors_full[seq_len(length(exposure_cuts) + 1)]

    datatable(
      d,
      rownames = FALSE,
      class = "stripe hover compact",
      options = list(
        dom = "t",
        pageLength = 10,
        autoWidth = TRUE,
        ordering = FALSE
      )
    ) %>%
      formatCurrency(c("Shipment Value", "Financial Exposure"), currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatStyle(
        "Financial Exposure",
        backgroundColor = styleInterval(exposure_cuts, exposure_colors),
        color = "#6B4F00",
        fontWeight = "700"
      )
  })
}
