# Risk Monitor Page Server Logic

risk_monitor_server <- function(input, output, session, shipments) {
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
}
