# Global Map Page Server Logic

global_map_server <- function(input, output, session, shipments) {
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
            origin, "→", destination,
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
                "<b>Route:</b> ", origin, " → ", destination, "<br>",
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
}
