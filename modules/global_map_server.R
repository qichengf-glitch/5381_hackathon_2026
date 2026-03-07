# Global Map Page Server Logic — Floating Overlay Design

global_map_server <- function(input, output, session, shipments) {

  # ── Map style toggle ─────────────────────────────────────────
  map_style <- reactiveVal("Dark")

  observeEvent(input$map_style_choice, {
    map_style(input$map_style_choice)
  })

  output$map_style_buttons <- renderUI({
    style <- map_style()
    theme_class <- switch(style,
      "Dark" = "theme-dark", "Light" = "theme-light",
      "Satellite" = "theme-satellite", "theme-dark"
    )
    make_btn <- function(label, value) {
      cls <- paste("map-style-btn", if (style == value) "active-style")
      tc  <- switch(value,
        "Dark" = "theme-dark", "Light" = "theme-light",
        "Satellite" = "theme-satellite", "theme-dark"
      )
      tags$button(
        label, class = cls,
        onclick = sprintf(
          "Shiny.setInputValue('map_style_choice','%s',{priority:'event'});var c=document.querySelector('.map-fullbleed');if(c){c.className=c.className.replace(/theme-\\w+/,'%s');}",
          value, tc
        )
      )
    }
    div(
      class = "map-style-toggle",
      make_btn("Dark", "Dark"),
      span("|", class = "map-style-sep"),
      make_btn("Light", "Light"),
      span("|", class = "map-style-sep"),
      make_btn("Satellite", "Satellite")
    )
  })

  # ── Data ──────────────────────────────────────────────────────
  map_data_raw <- reactive({
    shipments() %>%
      filter(!is.na(origin_lat), !is.na(origin_lng),
             !is.na(dest_lat),   !is.na(dest_lng))
  })

  observe({
    d <- map_data_raw()
    req(nrow(d) > 0)
    updateSelectizeInput(session, "map_transport_filter",
      choices  = sort(unique(d$shipping_method)),
      selected = sort(unique(d$shipping_method)))
    updateSelectizeInput(session, "map_risk_filter",
      choices  = c("Low", "Medium", "High"),
      selected = c("Low", "Medium", "High"))
    updateSelectizeInput(session, "map_weather_filter",
      choices  = sort(unique(d$weather_condition)),
      selected = sort(unique(d$weather_condition)))
  })

  map_data <- reactive({
    d <- map_data_raw()
    if (!is.null(input$map_transport_filter))
      d <- d %>% filter(shipping_method %in% input$map_transport_filter)
    if (!is.null(input$map_risk_filter))
      d <- d %>% filter(risk_level %in% input$map_risk_filter)
    if (!is.null(input$map_weather_filter))
      d <- d %>% filter(weather_condition %in% input$map_weather_filter)
    d
  })

  # ── Arc helper ────────────────────────────────────────────────
  make_arc <- function(lat1, lng1, lat2, lng2, n = 50) {
    t   <- seq(0, 1, length.out = n)
    lat <- lat1 + t * (lat2 - lat1)
    lng <- lng1 + t * (lng2 - lng1)
    dd  <- sqrt((lat2 - lat1)^2 + (lng2 - lng1)^2)
    offset <- dd * 0.15 * sin(t * pi)
    dl  <- lat2 - lat1
    dn  <- lng2 - lng1
    len <- sqrt(dl^2 + dn^2) + 1e-10
    lat <- lat + offset * (-dn / len)
    lng <- lng + offset * ( dl / len)
    data.frame(lat = lat, lng = lng)
  }

  # ── KPI chips with accent colors ──────────────────────────────
  kpi_chip <- function(label, value, accent = "") {
    div(class = "kpi-chip",
        div(class = "kpi-c-label", label),
        div(class = paste("kpi-c-value", accent), value))
  }

  output$map_kpi_routes <- renderUI({
    d <- map_data()
    kpi_chip("Routes", comma(nrow(d)), "accent-cyan")
  })

  output$map_kpi_avg_risk <- renderUI({
    d <- map_data()
    val <- if (nrow(d) > 0) format_risk(mean(d$risk_score, na.rm = TRUE)) else "\u2014"
    kpi_chip("Avg Risk", val, "accent-amber")
  })

  output$map_kpi_high_pct <- renderUI({
    d <- map_data()
    pct <- if (nrow(d) > 0) paste0(round(100 * mean(d$risk_level == "High"), 1), "%") else "\u2014"
    kpi_chip("High-Risk", pct, "accent-red")
  })

  output$map_kpi_exposure <- renderUI({
    d <- map_data()
    val <- if (nrow(d) > 0) format_currency(sum(d$financial_exposure, na.rm = TRUE)) else "\u2014"
    kpi_chip("Exposure", val, "accent-green")
  })

  # ── Timeline slider ────────────────────────────────────────────
  output$map_timeline_slider <- renderUI({
    d <- map_data_raw()
    req(nrow(d) > 0)
    t_min <- min(d$departure_dt, na.rm = TRUE)
    t_max <- max(d$expected_arrival_dt, na.rm = TRUE)
    req(!is.na(t_min), !is.na(t_max), t_max > t_min)
    t_mid <- t_min + (t_max - t_min) / 2
    # Use ~100 steps so each tick produces visible movement
    step_sec <- max(60, as.numeric(difftime(t_max, t_min, units = "secs")) / 100)
    sliderInput("map_sim_time", NULL,
      min = t_min, max = t_max, value = t_mid,
      step = step_sec,
      timeFormat = "%b %d %H:%M",
      animate = animationOptions(interval = 300, loop = TRUE),
      width = "100%")
  })

  output$map_transit_badge <- renderUI({
    vd <- vehicles_data()
    n <- if (is.null(vd)) 0L else nrow(vd)
    span(class = "tl-badge", paste0(n, " in transit"))
  })

  # ── In-transit vehicles reactive ────────────────────────────────
  vehicles_data <- reactive({
    d <- map_data()
    t <- input$map_sim_time
    req(t)
    # Only keep shipments with valid departure/arrival
    d <- d %>% filter(!is.na(departure_dt), !is.na(expected_arrival_dt))
    # In-transit: departed but not yet arrived at selected time
    d <- d %>% filter(departure_dt <= t, expected_arrival_dt > t)
    if (nrow(d) == 0) return(d)
    # Calculate progress & speed
    d <- d %>% mutate(
      duration_h   = as.numeric(difftime(expected_arrival_dt, departure_dt, units = "hours")),
      elapsed_h    = as.numeric(difftime(t, departure_dt, units = "hours")),
      progress     = pmin(1, pmax(0, elapsed_h / duration_h)),
      speed_kmh    = round(shipping_distance / duration_h, 1),
      dist_remaining = round(shipping_distance * (1 - progress)),
      eta          = expected_arrival_dt
    )
    # Interpolate position & heading along arc
    veh_lat <- numeric(nrow(d))
    veh_lng <- numeric(nrow(d))
    veh_hdg <- numeric(nrow(d))
    for (i in seq_len(nrow(d))) {
      arc <- make_arc(d$origin_lat[i], d$origin_lng[i],
                      d$dest_lat[i], d$dest_lng[i])
      idx <- max(1, min(50, round(d$progress[i] * 49) + 1))
      veh_lat[i] <- arc$lat[idx]
      veh_lng[i] <- arc$lng[idx]
      # Calculate heading from current to next arc point
      idx_next <- min(50, idx + 1)
      dlng <- arc$lng[idx_next] - arc$lng[idx]
      dlat <- arc$lat[idx_next] - arc$lat[idx]
      veh_hdg[i] <- atan2(dlng, dlat) * 180 / pi  # degrees from north
    }
    d$veh_lat <- veh_lat
    d$veh_lng <- veh_lng
    d$veh_hdg <- veh_hdg
    d
  })

  # ── In-transit KPI chip ─────────────────────────────────────────
  output$map_kpi_in_transit <- renderUI({
    vd <- vehicles_data()
    n <- if (is.null(vd)) 0L else nrow(vd)
    kpi_chip("In Transit", n, "accent-cyan")
  })

  # ── Map rendering ────────────────────────────────────────────
  selected_map_shipment <- reactiveVal(NULL)

  output$route_map <- renderMapWidget({
    d     <- map_data()
    req(nrow(d) > 0)
    style <- map_style()

    if (HAS_LEAFLET) {
      pal <- colorFactor(RISK_COLORS,
                         domain = c("Low", "Medium", "High"), ordered = TRUE)

      tile_provider <- switch(style,
        "Dark"      = providers$CartoDB.DarkMatter,
        "Light"     = providers$CartoDB.Positron,
        "Satellite" = providers$Esri.WorldImagery,
        providers$CartoDB.DarkMatter
      )

      vr <- range(d$shipment_value, na.rm = TRUE)
      if (vr[1] == vr[2]) {
        d$lw <- 3
      } else {
        d$lw <- 1.5 + (d$shipment_value - vr[1]) / (vr[2] - vr[1]) * 4.5
      }

      is_dark <- style != "Light"
      lbl_bg <- if (is_dark) "rgba(30,30,30,0.92)" else "rgba(255,255,255,0.95)"
      lbl_fg <- if (is_dark) "#F3F4F6" else "#1F2933"
      lbl_bd <- if (is_dark) "#555"    else "#E7EAEE"
      lbl_opt <- labelOptions(
        noHide = FALSE, direction = "top",
        style  = list(border = "none", background = "transparent",
                      `box-shadow` = "none", padding = "0")
      )

      m <- leaflet(d) %>%
        addProviderTiles(tile_provider) %>%
        setView(lng = 80, lat = 28, zoom = 2)

      # ── Arc routes with glow ────────────────────────────
      for (i in seq_len(nrow(d))) {
        arc <- make_arc(d$origin_lat[i], d$origin_lng[i],
                        d$dest_lat[i],   d$dest_lng[i])
        rc <- pal(d$risk_level[i])
        w  <- d$lw[i]

        m <- m %>% addPolylines(
          lng = arc$lng, lat = arc$lat,
          color = rc, weight = w * 2.5, opacity = 0.12,
          group = "routes"
        )

        lbl_html <- HTML(paste0(
          '<div style="background:', lbl_bg, ';color:', lbl_fg,
          ';border:1px solid ', lbl_bd,
          ';padding:6px 10px;border-radius:6px;font-size:13px;">',
          '<b>', d$origin[i], ' &rarr; ', d$destination[i], '</b><br>',
          '<span style="opacity:.6">Method:</span> ', d$shipping_method[i], '<br>',
          '<span style="opacity:.6">Risk:</span> ',   round(d$risk_score[i], 2),
          ' <span style="color:', rc, ';">&bull;</span> ', d$risk_level[i], '<br>',
          '<span style="opacity:.6">Exposure:</span> ', dollar(d$financial_exposure[i]),
          '</div>'
        ))

        m <- m %>% addPolylines(
          lng = arc$lng, lat = arc$lat,
          color   = rc, weight = w, opacity = 0.7,
          group = "routes",
          layerId = paste0("route_", d$shipment_id[i]),
          label   = lbl_html, labelOptions = lbl_opt,
          highlightOptions = highlightOptions(
            weight = w + 3, opacity = 1, bringToFront = TRUE
          )
        )
      }

      # ── Glowing origin markers ──────────────────────────
      origins <- distinct(d, origin, origin_lat, origin_lng)
      origin_labels <- lapply(origins$origin, function(o) {
        HTML(paste0(
          '<div style="background:', lbl_bg, ';color:', lbl_fg,
          ';border:1px solid ', lbl_bd,
          ';padding:4px 8px;border-radius:4px;font-size:12px;">',
          '<b>Origin:</b> ', o, '</div>'
        ))
      })

      m <- m %>%
        addCircleMarkers(
          data = origins, lng = ~origin_lng, lat = ~origin_lat,
          radius = 12, fillOpacity = 0.25, stroke = FALSE,
          fillColor = "#00E5FF", group = "markers"
        ) %>%
        addCircleMarkers(
          data = origins, lng = ~origin_lng, lat = ~origin_lat,
          radius = 5, fillOpacity = 0.9,
          stroke = TRUE, color = "#FFFFFF", weight = 1.5,
          fillColor = "#00BCD4",
          group = "markers",
          label = origin_labels, labelOptions = lbl_opt
        )

      # ── Destination markers ─────────────────────────────
      # Add small jitter so same-city markers spread when zoomed in
      set.seed(42)
      d$dest_lat_j <- d$dest_lat + runif(nrow(d), -0.08, 0.08)
      d$dest_lng_j <- d$dest_lng + runif(nrow(d), -0.08, 0.08)

      icon_list <- awesomeIcons(
        icon = case_when(
          d$shipping_method == "Truck" ~ "truck",
          d$shipping_method == "Sea"   ~ "ship",
          d$shipping_method == "Air"   ~ "plane",
          d$shipping_method == "Rail"  ~ "train",
          TRUE ~ "circle"
        ),
        markerColor = case_when(
          d$shipping_method == "Truck" ~ "blue",
          d$shipping_method == "Sea"   ~ "cadetblue",
          d$shipping_method == "Air"   ~ "green",
          d$shipping_method == "Rail"  ~ "orange",
          TRUE ~ "gray"
        ),
        iconColor = "white",
        library = "fa"
      )

      dest_labels <- lapply(seq_len(nrow(d)), function(i) {
        HTML(paste0(
          '<div style="background:', lbl_bg, ';color:', lbl_fg,
          ';border:1px solid ', lbl_bd,
          ';padding:6px 10px;border-radius:6px;font-size:13px;">',
          '<b>', d$destination[i], '</b><br>',
          '<span style="opacity:.6">Method:</span> ',   d$shipping_method[i], '<br>',
          '<span style="opacity:.6">Risk:</span> ',     round(d$risk_score[i], 2), '<br>',
          '<span style="opacity:.6">Exposure:</span> ', dollar(d$financial_exposure[i]),
          '</div>'
        ))
      })

      popup_html <- paste0(
        '<div style="font-size:13px;">',
        '<b>Shipment #', d$shipment_id, '</b><br>',
        '<b>Route:</b> ', d$origin, ' &rarr; ', d$destination, '<br>',
        '<b>Method:</b> ', d$shipping_method, '<br>',
        '<b>Weather:</b> ', d$weather_condition, '<br>',
        '<b>Risk Score:</b> ', round(d$risk_score, 2), '<br>',
        '<b>Exposure:</b> ', dollar(d$financial_exposure), '<br>',
        '<b>Delay:</b> ', d$delay_flag, ' (', round(d$delay_hours, 1), 'h)',
        '</div>'
      )

      m <- m %>%
        addAwesomeMarkers(
          data = d, lng = ~dest_lng_j, lat = ~dest_lat_j,
          icon     = icon_list,
          group    = "markers",
          layerId  = ~paste0("dest_", shipment_id),
          label    = dest_labels, labelOptions = lbl_opt,
          popup    = popup_html,
          clusterOptions = markerClusterOptions(
            zoomToBoundsOnClick = TRUE,
            spiderfyOnMaxZoom = TRUE,
            maxClusterRadius = 80
          )
        )

      m

    } else {
      # ── ggplot fallback ─────────────────────────────────
      ggplot(
        d,
        aes(x = origin_lng, y = origin_lat,
            xend = dest_lng, yend = dest_lat,
            color = risk_level)
      ) +
        geom_curve(
          aes(linewidth = shipment_value),
          alpha = 0.3, curvature = 0.2
        ) +
        geom_point(
          data = distinct(d, origin, origin_lat, origin_lng),
          aes(x = origin_lng, y = origin_lat),
          color = "#00BCD4", size = 3, alpha = 0.9,
          inherit.aes = FALSE
        ) +
        geom_point(
          aes(x = dest_lng, y = dest_lat, shape = shipping_method),
          size = 2.1, alpha = 0.85
        ) +
        scale_color_manual(values = RISK_COLORS) +
        scale_linewidth_continuous(range = c(0.3, 2), guide = "none") +
        scale_shape_manual(values = c(Truck = 15, Sea = 16, Air = 17, Rail = 18)) +
        coord_quickmap(xlim = c(-10, 150), ylim = c(-5, 62)) +
        labs(x = NULL, y = NULL, color = "Risk Level", shape = "Method") +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#ECEFF3"),
          legend.position  = "bottom"
        )
    }
  })

  # ── Tile swap via proxy (preserves zoom/pan) ─────────────────
  observeEvent(map_style(), {
    if (HAS_LEAFLET) {
      tile_provider <- switch(map_style(),
        "Dark"      = providers$CartoDB.DarkMatter,
        "Light"     = providers$CartoDB.Positron,
        "Satellite" = providers$Esri.WorldImagery,
        providers$CartoDB.DarkMatter
      )
      leafletProxy("route_map") %>%
        clearTiles() %>%
        addProviderTiles(tile_provider)
    }
  }, ignoreInit = TRUE)

  # ── Layer visibility toggles ────────────────────────────────────
  observeEvent(input$map_show_routes, {
    req(HAS_LEAFLET)
    proxy <- leafletProxy("route_map")
    if (isTRUE(input$map_show_routes)) proxy %>% showGroup("routes")
    else proxy %>% hideGroup("routes")
  }, ignoreInit = TRUE)

  observeEvent(input$map_show_markers, {
    req(HAS_LEAFLET)
    proxy <- leafletProxy("route_map")
    if (isTRUE(input$map_show_markers)) proxy %>% showGroup("markers")
    else proxy %>% hideGroup("markers")
  }, ignoreInit = TRUE)

  # ── Vehicle markers via leafletProxy ────────────────────────────
  observe({
    req(HAS_LEAFLET)
    vd <- vehicles_data()
    proxy <- leafletProxy("route_map")
    proxy %>% clearGroup("vehicles")
    if (is.null(vd) || nrow(vd) == 0) return()

    style <- map_style()
    is_dark <- style != "Light"
    lbl_bg <- if (is_dark) "rgba(30,30,30,0.92)" else "rgba(255,255,255,0.95)"
    lbl_fg <- if (is_dark) "#F3F4F6" else "#1F2933"
    lbl_bd <- if (is_dark) "#555"    else "#E7EAEE"

    # Vehicle icons per method — original awesome marker style
    veh_icons <- list(
      Truck = makeAwesomeIcon(icon = "truck", markerColor = "darkblue",  iconColor = "#fff", library = "fa"),
      Sea   = makeAwesomeIcon(icon = "ship",  markerColor = "darkpurple", iconColor = "#fff", library = "fa"),
      Air   = makeAwesomeIcon(icon = "plane", markerColor = "darkgreen",  iconColor = "#fff", library = "fa"),
      Rail  = makeAwesomeIcon(icon = "train", markerColor = "darkred",   iconColor = "#fff", library = "fa")
    )

    for (i in seq_len(nrow(vd))) {
      pct <- round(vd$progress[i] * 100)
      method <- vd$shipping_method[i]
      veh_icon <- veh_icons[[method]] %||% veh_icons[["Truck"]]

      lbl_html <- HTML(paste0(
        '<div style="background:', lbl_bg, ';color:', lbl_fg,
        ';border:1px solid ', lbl_bd,
        ';padding:6px 10px;border-radius:6px;font-size:12px;min-width:160px;">',
        '<b>#', vd$shipment_id[i], '</b> ', vd$origin[i], ' &rarr; ', vd$destination[i],
        '<br><span style="opacity:.6">Progress:</span> ', pct, '%',
        ' &middot; <span style="opacity:.6">Speed:</span> ', vd$speed_kmh[i], ' km/h',
        '<br><span style="opacity:.6">ETA:</span> ', format(vd$eta[i], "%b %d %H:%M"),
        '</div>'
      ))

      proxy <- proxy %>% addAwesomeMarkers(
        lng = vd$veh_lng[i], lat = vd$veh_lat[i],
        icon = veh_icon,
        group = "vehicles",
        layerId = paste0("veh_", vd$shipment_id[i]),
        label = lbl_html,
        labelOptions = labelOptions(
          noHide = FALSE, direction = "top",
          style = list(border = "none", background = "transparent",
                       `box-shadow` = "none", padding = "0")
        )
      )
    }
  })

  # ── Click handler ─────────────────────────────────────────────
  if (HAS_LEAFLET) {
    # shape_click: arc routes + circle markers
    observeEvent(input$route_map_shape_click, {
      clicked <- input$route_map_shape_click
      if (is.null(clicked$id)) return()
      id_val <- str_remove(clicked$id, "^(route_|dest_|veh_)")
      selected_map_shipment(as.integer(id_val))
    })
    # marker_click: vehicle divIcon markers + awesome markers
    observeEvent(input$route_map_marker_click, {
      clicked <- input$route_map_marker_click
      if (is.null(clicked$id)) return()
      id_val <- str_remove(clicked$id, "^(route_|dest_|veh_)")
      selected_map_shipment(as.integer(id_val))
    })
  }

  # ── Transport summary ────────────────────────────────────────
  output$map_transport_summary <- renderChart({
    d <- map_data() %>%
      count(shipping_method) %>%
      mutate(shipping_method = fct_reorder(shipping_method, n))

    p <- ggplot(d, aes(x = shipping_method, y = n, fill = shipping_method,
                       text = paste(shipping_method, "-", comma(n)))) +
      geom_col(width = 0.6, show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = METHOD_COLORS) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        plot.background    = element_rect(fill = "transparent", color = NA),
        panel.background   = element_rect(fill = "transparent", color = NA)
      )

    finalize_chart(p, tooltip = "text")
  })

  # ── Risk summary table ───────────────────────────────────────
  output$map_risk_summary <- renderDT({
    d <- map_data() %>%
      group_by(risk_level) %>%
      summarise(
        Routes           = n(),
        `Avg Delay (hrs)` = mean(delay_hours, na.rm = TRUE),
        `Avg Exposure`    = mean(financial_exposure, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(match(risk_level, c("Low", "Medium", "High")))

    datatable(
      d, rownames = FALSE,
      options = list(dom = "t", ordering = FALSE, autoWidth = TRUE,
                     initComplete = JS("function(s,d){$(s.nTable).css('background','transparent');$(s.nTable).find('th,td').css({'background':'transparent','border-color':'rgba(128,128,128,0.2)','font-size':'12px'});}"))
    ) %>%
      formatRound("Avg Delay (hrs)", 1) %>%
      formatCurrency("Avg Exposure", currency = "$", digits = 0)
  }, server = FALSE)

  # ── Enhanced legend ───────────────────────────────────────────
  output$risk_legend_widget <- renderUI({
    tagList(
      div(class = "legend-row",
          span(class = "legend-dot", style = paste0("background:", RISK_COLORS["Low"])),
          "Low Risk"),
      div(class = "legend-row",
          span(class = "legend-dot", style = paste0("background:", RISK_COLORS["Medium"])),
          "Medium Risk"),
      div(class = "legend-row",
          span(class = "legend-dot", style = paste0("background:", RISK_COLORS["High"])),
          "High Risk"),
      tags$hr(style = "margin:6px 0;border-color:rgba(128,128,128,0.2)"),
      div(class = "legend-row",
          span(class = "legend-dot",
               style = "background:#00BCD4; box-shadow: 0 0 6px 2px #00E5FF;"),
          "Origin City"),
      tags$hr(style = "margin:6px 0;border-color:rgba(128,128,128,0.2)"),
      div(class = "legend-row", icon("truck"), "Truck"),
      div(class = "legend-row", icon("ship"),  "Sea"),
      div(class = "legend-row", icon("plane"), "Air"),
      div(class = "legend-row", icon("train"), "Rail"),
      tags$hr(style = "margin:6px 0;border-color:rgba(128,128,128,0.2)"),
      div(class = "legend-row",
          tags$span(style = "display:inline-block;width:30px;height:2px;background:#888;vertical-align:middle;border-radius:1px;"),
          "Low value"),
      div(class = "legend-row",
          tags$span(style = "display:inline-block;width:30px;height:6px;background:#888;vertical-align:middle;border-radius:2px;"),
          "High value")
    )
  })

  # ── Route detail (compact bar + expandable) ──────────────────
  output$map_route_detail <- renderUI({
    d <- map_data()
    if (nrow(d) == 0) {
      return(div(style = "font-size:13px;opacity:0.6;", "No routes available."))
    }

    selected_id <- selected_map_shipment()
    rec <- if (is.null(selected_id)) {
      d %>% arrange(desc(risk_score), desc(financial_exposure)) %>% slice(1)
    } else {
      d %>% filter(shipment_id == selected_id) %>% slice(1)
    }

    if (nrow(rec) == 0) {
      return(div(style = "font-size:13px;opacity:0.6;",
                 "Click a route to inspect details"))
    }

    risk_col <- RISK_COLORS[as.character(rec$risk_level)]

    # Check if this shipment is currently in-transit
    vd <- vehicles_data()
    veh_rec <- NULL
    if (!is.null(vd) && nrow(vd) > 0) {
      veh_rec <- vd %>% filter(shipment_id == rec$shipment_id) %>% slice(1)
      if (nrow(veh_rec) == 0) veh_rec <- NULL
    }

    # Compact line — add tracking info if in-transit
    compact_items <- tagList(
      span(class = "dc-id", paste0("#", rec$shipment_id)),
      span(class = "dc-sep", "\u2022"),
      span(paste(rec$origin, "\u2192", rec$destination)),
      span(class = "dc-sep", "\u2022"),
      span(rec$shipping_method),
      span(class = "dc-sep", "\u2022"),
      span(class = "dc-risk", style = paste0("color:", risk_col, ";"),
           paste("Risk", round(rec$risk_score, 2))),
      span(class = "dc-sep", "\u2022"),
      span(dollar(rec$financial_exposure))
    )
    if (!is.null(veh_rec)) {
      compact_items <- tagList(
        compact_items,
        span(class = "dc-sep", "\u2022"),
        span(style = "color:#4DD0C8;font-weight:700;",
             paste0(round(veh_rec$progress * 100), "% \u2022 ",
                    veh_rec$speed_kmh, " km/h"))
      )
    }
    compact_items <- tagList(compact_items,
      span(class = "dc-hint", icon("chevron-down"), " details"))

    # Expanded grid — base + tracking row
    grid_items <- tagList(
      div(class = "detail-item", div(class = "detail-label", "Shipment ID"),      div(class = "detail-value", rec$shipment_id)),
      div(class = "detail-item", div(class = "detail-label", "Route"),            div(class = "detail-value", rec$route)),
      div(class = "detail-item", div(class = "detail-label", "Method"),           div(class = "detail-value", rec$shipping_method)),
      div(class = "detail-item", div(class = "detail-label", "Weather"),          div(class = "detail-value", rec$weather_condition)),
      div(class = "detail-item", div(class = "detail-label", "Risk Score"),       div(class = "detail-value", round(rec$risk_score, 2))),
      div(class = "detail-item", div(class = "detail-label", "Exposure"),         div(class = "detail-value", dollar(rec$financial_exposure))),
      div(class = "detail-item", div(class = "detail-label", "Delay"),            div(class = "detail-value", rec$delay_flag)),
      div(class = "detail-item", div(class = "detail-label", "Delay Hours"),      div(class = "detail-value", round(rec$delay_hours, 1)))
    )
    if (!is.null(veh_rec)) {
      grid_items <- tagList(grid_items,
        div(class = "detail-item", div(class = "detail-label", "Progress"),
            div(class = "detail-value", style = "color:#4DD0C8;",
                paste0(round(veh_rec$progress * 100), "%"))),
        div(class = "detail-item", div(class = "detail-label", "Speed"),
            div(class = "detail-value", paste0(veh_rec$speed_kmh, " km/h"))),
        div(class = "detail-item", div(class = "detail-label", "Remaining"),
            div(class = "detail-value", paste0(comma(veh_rec$dist_remaining), " km"))),
        div(class = "detail-item", div(class = "detail-label", "ETA"),
            div(class = "detail-value", format(veh_rec$eta, "%b %d %H:%M")))
      )
    }

    tagList(
      div(class = "detail-compact", compact_items),
      div(class = "detail-expanded",
          div(class = "detail-grid", grid_items))
    )
  })
}
