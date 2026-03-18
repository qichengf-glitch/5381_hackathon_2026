# Utility Functions

# Check for optional packages
HAS_PLOTLY <- requireNamespace("plotly", quietly = TRUE)
HAS_LEAFLET <- requireNamespace("leaflet", quietly = TRUE)

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

normalize_delay_flag <- function(x) {
  x_chr <- as.character(x)
  case_when(
    x_chr %in% c("1", "TRUE", "T", "Delayed", "Yes", "Y", "delay", "delayed") ~ "Delayed",
    is.na(x_chr) ~ "On Time",
    TRUE ~ "On Time"
  )
}

pick_col <- function(df, candidates, default = NA) {
  for (nm in candidates) {
    if (nm %in% names(df)) return(df[[nm]])
  }
  if (length(default) == nrow(df)) {
    return(default)
  }
  rep(default, nrow(df))
}

parse_datetime_flex <- function(x) {
  x_chr <- as.character(x)
  dt <- suppressWarnings(lubridate::mdy_hm(x_chr))
  if (all(is.na(dt))) dt <- suppressWarnings(lubridate::ymd_hms(x_chr, quiet = TRUE))
  if (all(is.na(dt))) dt <- suppressWarnings(lubridate::ymd_hm(x_chr, quiet = TRUE))
  if (all(is.na(dt))) dt <- suppressWarnings(lubridate::ymd(x_chr, quiet = TRUE))
  dt
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

load_supabase_env <- function() {
  env_candidates <- c(".env", "supabase_setup/.env")
  env_loaded <- FALSE

  parse_env_fallback <- function(env_path) {
    lines <- readLines(env_path, warn = FALSE)
    for (line in lines) {
      line_trim <- trimws(line)
      if (!nzchar(line_trim) || startsWith(line_trim, "#")) next
      if (!grepl("=", line_trim, fixed = TRUE)) next

      parts <- strsplit(line_trim, "=", fixed = TRUE)[[1]]
      key <- trimws(parts[1])
      value <- trimws(paste(parts[-1], collapse = "="))
      if (!nzchar(key)) next

      # Remove optional surrounding quotes
      if (startsWith(value, "\"") && endsWith(value, "\"") && nchar(value) >= 2) {
        value <- substring(value, 2, nchar(value) - 1)
      } else if (startsWith(value, "'") && endsWith(value, "'") && nchar(value) >= 2) {
        value <- substring(value, 2, nchar(value) - 1)
      }

      do.call(Sys.setenv, stats::setNames(list(value), key))
    }
  }

  for (env_path in env_candidates) {
    if (file.exists(env_path)) {
      if (requireNamespace("dotenv", quietly = TRUE)) {
        dotenv::load_dot_env(env_path)
      } else {
        parse_env_fallback(env_path)
      }
      env_loaded <- TRUE
    }
  }

  invisible(env_loaded)
}

supabase_ready <- function() {
  use_supabase <- tolower(trimws(Sys.getenv("USE_SUPABASE", "true")))
  if (use_supabase %in% c("false", "0", "no", "off")) {
    return(FALSE)
  }

  load_supabase_env()
  url_ready <- nzchar(Sys.getenv("SUPABASE_URL"))
  key_ready <- nzchar(Sys.getenv("SUPABASE_SERVICE_ROLE_KEY")) || nzchar(Sys.getenv("SUPABASE_ANON_KEY"))
  url_ready && key_ready
}

supabase_key <- function() {
  service_key <- Sys.getenv("SUPABASE_SERVICE_ROLE_KEY")
  if (nzchar(service_key)) {
    return(service_key)
  }
  Sys.getenv("SUPABASE_ANON_KEY")
}

fetch_supabase_table <- function(table_name, page_size = 5000, max_rows = 200000) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required for Supabase integration.")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for Supabase integration.")
  }

  base_url <- Sys.getenv("SUPABASE_URL")
  key <- supabase_key()
  if (!nzchar(base_url) || !nzchar(key)) {
    stop("SUPABASE_URL and key are required.")
  }

  if (!is.numeric(page_size) || page_size <= 0) page_size <- 1000
  if (!is.numeric(max_rows) || max_rows <= 0) max_rows <- 200000

  batches <- list()
  offset <- 0L

  repeat {
    req <- httr2::request(sprintf("%s/rest/v1/%s", base_url, table_name)) %>%
      httr2::req_url_query(
        select = "*",
        limit = as.integer(page_size),
        offset = as.integer(offset)
      ) %>%
      httr2::req_headers(
        apikey = key,
        Authorization = paste("Bearer", key)
      ) %>%
      httr2::req_timeout(seconds = 30)

    resp <- httr2::req_perform(req)
    txt <- httr2::resp_body_string(resp)
    batch <- jsonlite::fromJSON(txt, flatten = TRUE) %>%
      tibble::as_tibble()

    n_batch <- nrow(batch)
    if (n_batch == 0) break

    batches[[length(batches) + 1]] <- batch
    offset <- offset + n_batch

    if (n_batch < page_size || offset >= max_rows) break
  }

  if (length(batches) == 0) return(tibble::tibble())
  dplyr::bind_rows(batches)
}

clean_shipment_data <- function(raw) {
  raw <- tibble::as_tibble(raw)

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

generate_mock_shipments <- function(n = 1200) {
  n <- as.integer(n)
  if (is.na(n) || n < 50) n <- 1200

  set.seed(5381)

  destinations <- setdiff(CITY_COORDS$city, "Shanghai")
  shipping_method <- sample(c("Truck", "Sea", "Rail", "Air"), n, replace = TRUE, prob = c(0.33, 0.24, 0.24, 0.19))
  weather_condition <- sample(c("Clear", "Rain", "Fog", "Storm", "Snow"), n, replace = TRUE, prob = c(0.38, 0.24, 0.14, 0.16, 0.08))
  destination <- sample(destinations, n, replace = TRUE)

  supplier_reliability_score <- pmin(pmax(rnorm(n, mean = 0.78, sd = 0.11), 0.25), 0.99)
  shipment_value <- round(runif(n, min = 30000, max = 620000), 2)
  shipping_distance <- round(runif(n, min = 250, max = 12500), 1)

  shipping_risk <- case_when(
    shipping_method == "Air" ~ 0.34,
    shipping_method == "Rail" ~ 0.44,
    shipping_method == "Truck" ~ 0.54,
    TRUE ~ 0.60
  ) + rnorm(n, 0, 0.07)

  weather_risk <- case_when(
    weather_condition == "Clear" ~ 0.22,
    weather_condition == "Rain" ~ 0.46,
    weather_condition == "Fog" ~ 0.57,
    weather_condition == "Storm" ~ 0.73,
    TRUE ~ 0.80
  ) + rnorm(n, 0, 0.06)

  distance_risk <- safe_rescale(shipping_distance, to = c(0.16, 0.92)) + rnorm(n, 0, 0.05)
  supplier_risk <- (1 - supplier_reliability_score) + rnorm(n, 0, 0.04)

  shipping_risk <- pmin(pmax(shipping_risk, 0), 1)
  weather_risk <- pmin(pmax(weather_risk, 0), 1)
  distance_risk <- pmin(pmax(distance_risk, 0), 1)
  supplier_risk <- pmin(pmax(supplier_risk, 0), 1)

  risk_score <- pmin(
    pmax(
      0.28 * weather_risk +
        0.24 * shipping_risk +
        0.22 * distance_risk +
        0.26 * supplier_risk +
        rnorm(n, 0, 0.05),
      0
    ),
    1
  )

  delayed_prob <- pmin(pmax(0.03 + risk_score * 0.55, 0.03), 0.95)
  delayed <- runif(n) < delayed_prob
  delay_flag <- if_else(delayed, "Delayed", "On Time")
  delay_hours <- if_else(delayed, round(rexp(n, rate = 1 / 8), 1), 0)

  financial_exposure <- round(
    shipment_value * (0.14 + risk_score * 0.95) * (1 + pmin(delay_hours, 36) / 42),
    2
  )

  departure_dt <- Sys.time() - lubridate::days(sample(2:110, n, replace = TRUE)) - lubridate::hours(sample(0:23, n, replace = TRUE))
  transit_hours <- pmax(6, round(shipping_distance / runif(n, min = 45, max = 760)))
  expected_arrival_dt <- departure_dt + lubridate::hours(transit_hours)
  actual_arrival_dt <- expected_arrival_dt + lubridate::dminutes(delay_hours * 60)

  raw <- tibble(
    origin = "Shanghai",
    destination = destination,
    shipping_method = shipping_method,
    weather_condition = weather_condition,
    supplier_reliability_score = supplier_reliability_score,
    shipment_value = shipment_value,
    shipping_distance = shipping_distance,
    delay_flag = delay_flag,
    delay_hours = delay_hours,
    risk_score = risk_score,
    financial_exposure = financial_exposure,
    weather_risk = weather_risk,
    shipping_risk = shipping_risk,
    distance_risk = distance_risk,
    supplier_risk = supplier_risk,
    departure_time = format(departure_dt, "%m/%d/%Y %H:%M"),
    expected_arrival_time = format(expected_arrival_dt, "%m/%d/%Y %H:%M"),
    actual_arrival_time = format(actual_arrival_dt, "%m/%d/%Y %H:%M")
  )

  clean_shipment_data(raw)
}

derive_historical_from_shipments <- function(shipments_df) {
  shipments_df %>%
    transmute(
      shipment_id = as.integer(shipment_id),
      origin = as.character(origin),
      destination = as.character(destination),
      route = as.character(route),
      delay_flag = normalize_delay_flag(delay_flag),
      delay_hours = as.numeric(delay_hours),
      risk_score = as.numeric(risk_score),
      financial_exposure = as.numeric(financial_exposure),
      departure_dt = as.POSIXct(departure_dt)
    )
}

clean_historical_data <- function(raw, fallback_shipments = NULL) {
  raw <- tibble::as_tibble(raw)
  if (nrow(raw) == 0) {
    if (!is.null(fallback_shipments)) return(derive_historical_from_shipments(fallback_shipments))
    return(tibble())
  }

  default_ids <- seq_len(nrow(raw))

  out <- raw %>%
    mutate(
      shipment_id = as.integer(pick_col(raw, c("shipment_id", "id", "shipment_key"), default = default_ids)),
      origin = as.character(pick_col(raw, c("origin", "origin_city", "from_origin", "source"))),
      destination = as.character(pick_col(raw, c("destination", "destination_city", "to_destination", "target"))),
      route = as.character(pick_col(raw, c("route", "route_name"))),
      delay_flag = normalize_delay_flag(pick_col(raw, c("delay_flag", "delay_status", "is_delayed", "delayed"))),
      delay_hours = as.numeric(pick_col(raw, c("delay_hours", "delay_duration_hours", "delay_time"), default = 0)),
      risk_score = as.numeric(pick_col(raw, c("risk_score", "risk", "avg_risk"), default = NA_real_)),
      financial_exposure = as.numeric(pick_col(raw, c("financial_exposure", "exposure", "value_at_risk"), default = NA_real_)),
      departure_dt = parse_datetime_flex(pick_col(raw, c("departure_dt", "departure_time", "event_time", "timestamp", "date")))
    ) %>%
    mutate(
      route = if_else(!is.na(route) & nzchar(route), route, paste(origin, destination, sep = " -> "))
    ) %>%
    select(shipment_id, origin, destination, route, delay_flag, delay_hours, risk_score, financial_exposure, departure_dt)

  if (all(is.na(out$risk_score)) && !is.null(fallback_shipments)) {
    return(derive_historical_from_shipments(fallback_shipments))
  }

  out
}

derive_warehouse_from_shipments <- function(shipments_df) {
  shipments_df %>%
    group_by(destination) %>%
    summarise(
      throughput = sum(shipment_value, na.rm = TRUE),
      avg_risk = mean(risk_score, na.rm = TRUE),
      delay_rate = mean(delay_flag == "Delayed", na.rm = TRUE),
      shipments = n(),
      .groups = "drop"
    ) %>%
    mutate(
      warehouse = paste(destination, "Hub"),
      cargo_type = "All Cargo",
      utilization = pmin(98, 45 + safe_rescale(throughput, c(10, 40)) + delay_rate * 25),
      predicted_utilization = pmin(99, utilization + safe_rescale(avg_risk, c(2, 10))),
      stress_level = case_when(
        predicted_utilization >= 85 ~ "High",
        predicted_utilization >= 70 ~ "Medium",
        TRUE ~ "Low"
      )
    ) %>%
    select(warehouse, cargo_type, utilization, predicted_utilization, stress_level, throughput, avg_risk, delay_rate, shipments)
}

clean_warehouse_status_data <- function(raw, fallback_shipments = NULL) {
  raw <- tibble::as_tibble(raw)
  if (nrow(raw) == 0) {
    if (!is.null(fallback_shipments)) return(derive_warehouse_from_shipments(fallback_shipments))
    return(tibble())
  }

  out <- raw %>%
    mutate(
      warehouse = as.character(pick_col(raw, c("warehouse", "warehouse_name", "destination", "hub", "location"))),
      cargo_type = as.character(pick_col(raw, c("cargo_type", "cargo", "category", "product_type"), default = "All Cargo")),
      utilization = as.numeric(pick_col(raw, c("utilization", "warehouse_utilization", "utilization_pct", "current_utilization", "utilization_rate"))),
      predicted_utilization = as.numeric(pick_col(raw, c("predicted_utilization", "forecast_utilization", "predicted_capacity_stress", "projected_utilization"))),
      throughput = as.numeric(pick_col(raw, c("throughput", "shipment_value", "volume", "current_load"), default = NA_real_)),
      avg_risk = as.numeric(pick_col(raw, c("avg_risk", "risk_score", "risk"), default = NA_real_)),
      delay_rate = as.numeric(pick_col(raw, c("delay_rate", "delayed_rate", "delay_ratio"), default = NA_real_)),
      shipments = as.numeric(pick_col(raw, c("shipments", "shipment_count", "count"), default = NA_real_)),
      stress_level = as.character(pick_col(raw, c("stress_level", "risk_level"))),
      incoming_shipments = as.numeric(pick_col(raw, c("incoming_shipments", "inbound_shipments"), default = NA_real_)),
      outgoing_shipments = as.numeric(pick_col(raw, c("outgoing_shipments", "outbound_shipments"), default = NA_real_)),
      congestion_flag_raw = as.character(pick_col(raw, c("congestion_flag", "is_congested", "congested"), default = "0")),
      predicted_hours_to_90 = as.numeric(pick_col(raw, c("predicted_hours_to_90"), default = NA_real_)),
      predicted_hours_to_full = as.numeric(pick_col(raw, c("predicted_hours_to_full"), default = NA_real_))
    ) %>%
    mutate(
      warehouse = if_else(is.na(warehouse) | !nzchar(warehouse), "Unknown Hub", warehouse),
      utilization = if_else(!is.na(utilization) & utilization <= 1, utilization * 100, utilization),
      predicted_utilization = if_else(!is.na(predicted_utilization) & predicted_utilization <= 1, predicted_utilization * 100, predicted_utilization),
      congestion_flag = congestion_flag_raw %in% c("1", "TRUE", "T", "Yes", "Y", "yes", "true"),
      throughput = coalesce(throughput, incoming_shipments + outgoing_shipments),
      shipments = coalesce(shipments, incoming_shipments + outgoing_shipments),
      pressure = incoming_shipments - outgoing_shipments,
      predicted_utilization = coalesce(
        predicted_utilization,
        utilization +
          if_else(!is.na(pressure), safe_rescale(pressure, c(0, 8)), 0) +
          if_else(congestion_flag, 6, 0) +
          if_else(!is.na(predicted_hours_to_full), (1 - safe_rescale(predicted_hours_to_full, c(0, 1))) * 8, 0)
      ),
      delay_rate = if_else(!is.na(delay_rate) & delay_rate > 1, delay_rate / 100, delay_rate),
      predicted_utilization = pmin(pmax(predicted_utilization, 0), 100),
      stress_level = case_when(
        stress_level %in% c("Low", "Medium", "High") ~ stress_level,
        predicted_utilization >= 85 ~ "High",
        predicted_utilization >= 70 ~ "Medium",
        TRUE ~ "Low"
      )
    ) %>%
    group_by(warehouse) %>%
    summarise(
      cargo_type = paste(unique(cargo_type[!is.na(cargo_type) & nzchar(cargo_type)]), collapse = ", "),
      utilization = mean(utilization, na.rm = TRUE),
      predicted_utilization = mean(predicted_utilization, na.rm = TRUE),
      throughput = sum(throughput, na.rm = TRUE),
      avg_risk = mean(avg_risk, na.rm = TRUE),
      delay_rate = mean(delay_rate, na.rm = TRUE),
      shipments = sum(shipments, na.rm = TRUE),
      stress_level = first(stress_level),
      .groups = "drop"
    ) %>%
    mutate(
      cargo_type = if_else(is.na(cargo_type) | !nzchar(cargo_type), "All Cargo", cargo_type),
      utilization = if_else(is.finite(utilization), utilization, NA_real_),
      predicted_utilization = if_else(is.finite(predicted_utilization), predicted_utilization, utilization),
      throughput = if_else(is.finite(throughput), throughput, NA_real_),
      avg_risk = if_else(is.finite(avg_risk), avg_risk, NA_real_),
      delay_rate = if_else(is.finite(delay_rate), delay_rate, NA_real_),
      shipments = if_else(is.finite(shipments), shipments, NA_real_)
    )

  has_usable_rows <- nrow(out %>% filter(!is.na(utilization), nzchar(warehouse))) > 0

  if (!has_usable_rows && !is.null(fallback_shipments)) {
    return(derive_warehouse_from_shipments(fallback_shipments))
  }

  out
}

load_shipment_data_local <- function(path = DEFAULT_DATA_PATH) {
  tryCatch({
    source_file <- pick_data_source(path)
    ext <- tolower(tools::file_ext(source_file))

    raw <- if (ext %in% c("xlsx", "xls")) {
      readxl::read_excel(source_file)
    } else if (ext == "csv") {
      readr::read_csv(source_file, show_col_types = FALSE)
    } else {
      stop("Unsupported file type: ", ext, ". Use .xlsx/.xls or .csv")
    }

    clean_shipment_data(raw)
  }, error = function(e) {
    warning(
      "Local shipment file unavailable; using generated demo data. Detail: ",
      conditionMessage(e),
      call. = FALSE
    )
    generate_mock_shipments()
  })
}

load_shipment_data <- function(path = DEFAULT_DATA_PATH) {
  tryCatch({
    if (!supabase_ready()) {
      warning(
        "Supabase is not configured; falling back to local shipment file.",
        call. = FALSE
      )
      return(load_shipment_data_local(path))
    }

    raw <- fetch_supabase_table(SUPABASE_TABLE_FINAL)
    if (nrow(raw) == 0) {
      warning(
        "Supabase final table returned 0 rows; falling back to local shipment file.",
        call. = FALSE
      )
      return(load_shipment_data_local(path))
    }
    clean_shipment_data(raw)
  }, error = function(e) {
    warning(
      "Shipment data load failed; using generated demo data. Detail: ",
      conditionMessage(e),
      call. = FALSE
    )
    generate_mock_shipments()
  })
}

load_historical_data <- function(base_shipments = NULL) {
  base_shipments_safe <- if (is.null(base_shipments)) generate_mock_shipments(300) else base_shipments

  tryCatch({
    if (!supabase_ready()) {
      warning(
        "Supabase is not configured for historical data; using derived local history.",
        call. = FALSE
      )
      return(derive_historical_from_shipments(base_shipments_safe))
    }

    raw <- fetch_supabase_table(SUPABASE_TABLE_HISTORICAL)
    res <- clean_historical_data(raw, fallback_shipments = base_shipments_safe)
    if (nrow(res) == 0) {
      warning(
        "Supabase historical table returned no usable rows; using derived local history.",
        call. = FALSE
      )
      return(derive_historical_from_shipments(base_shipments_safe))
    }
    res
  }, error = function(e) {
    warning(
      "Historical data load failed; using derived history. Detail: ",
      conditionMessage(e),
      call. = FALSE
    )
    derive_historical_from_shipments(base_shipments_safe)
  })
}

load_warehouse_status_data <- function(base_shipments = NULL) {
  base_shipments_safe <- if (is.null(base_shipments)) generate_mock_shipments(300) else base_shipments

  tryCatch({
    if (!supabase_ready()) {
      warning(
        "Supabase is not configured for warehouse data; using derived local warehouse status.",
        call. = FALSE
      )
      return(derive_warehouse_from_shipments(base_shipments_safe))
    }

    raw <- fetch_supabase_table(SUPABASE_TABLE_WAREHOUSE)
    res <- clean_warehouse_status_data(raw, fallback_shipments = base_shipments_safe)
    if (nrow(res) == 0) {
      warning(
        "Supabase warehouse table returned no usable rows; using derived local warehouse status.",
        call. = FALSE
      )
      return(derive_warehouse_from_shipments(base_shipments_safe))
    }
    res
  }, error = function(e) {
    warning(
      "Warehouse status load failed; using derived warehouse status. Detail: ",
      conditionMessage(e),
      call. = FALSE
    )
    derive_warehouse_from_shipments(base_shipments_safe)
  })
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
  expr_quoted <- substitute(expr)
  expr_env <- parent.frame()
  if (HAS_PLOTLY) {
    plotly::renderPlotly(expr_quoted, quoted = TRUE, env = expr_env)
  } else {
    renderPlot(expr_quoted, quoted = TRUE, env = expr_env, res = 110)
  }
}

finalize_chart <- function(p, tooltip = "text") {
  if (HAS_PLOTLY) {
    try_plotly <- function(plot_obj, tip) {
      plt <- plotly::ggplotly(plot_obj, tooltip = tip) %>%
        plotly::config(displayModeBar = FALSE)
      # Force eager evaluation so errors are caught here, not at render time
      plotly::plotly_build(plt)
      plt
    }
    tryCatch(
      try_plotly(p, tooltip),
      error = function(e) {
        tryCatch(
          try_plotly(p, c("x", "y")),
          error = function(e2) {
            plotly::plot_ly(type = "scatter", mode = "none") %>%
              plotly::layout(
                xaxis = list(visible = FALSE),
                yaxis = list(visible = FALSE),
                annotations = list(list(
                  text = "Chart unavailable",
                  xref = "paper", yref = "paper",
                  x = 0.5, y = 0.5, showarrow = FALSE,
                  font = list(size = 14, color = "#999")
                ))
              ) %>%
              plotly::config(displayModeBar = FALSE)
          }
        )
      }
    )
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
  expr_quoted <- substitute(expr)
  expr_env <- parent.frame()
  if (HAS_LEAFLET) {
    leaflet::renderLeaflet(expr_quoted, quoted = TRUE, env = expr_env)
  } else {
    renderPlot(expr_quoted, quoted = TRUE, env = expr_env, res = 110)
  }
}
