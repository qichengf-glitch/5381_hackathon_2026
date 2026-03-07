# Utility Functions

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
      route = paste(origin, destination, sep = " → "),
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
