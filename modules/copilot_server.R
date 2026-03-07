# AI Copilot Page Server Logic

copilot_server <- function(input, output, session, shipments, warehouse_base) {
  clean_num <- function(x, default = NA_real_) {
    out <- suppressWarnings(as.numeric(x))
    out[!is.finite(out)] <- default
    out
  }

  safe_mean_num <- function(x, default = 0) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]
    if (!length(x)) return(default)
    mean(x)
  }

  pull_col <- function(df, name, default = NA) {
    if (name %in% names(df)) return(df[[name]])
    rep(default, nrow(df))
  }

  extract_top_n <- function(q, default_n = 5L, max_n = 12L) {
    m1 <- str_match(q, "top\\s*(\\d{1,2})")
    m2 <- str_match(q, "\\b(\\d{1,2})\\b\\s*(shipments?|routes?|hubs?|warehouses?|risks?|factors?|drivers?)")
    m3 <- str_match(q, "\\u524d\\s*(\\d{1,2})")

    n <- suppressWarnings(as.integer(coalesce(m1[, 2], m2[, 2], m3[, 2])))
    if (is.na(n)) n <- default_n
    as.integer(max(1, min(max_n, n)))
  }

  extract_content_text <- function(content) {
    if (is.null(content)) return("")
    if (is.character(content)) return(paste(content, collapse = "\n"))

    if (is.list(content)) {
      parts <- vapply(content, function(part) {
        if (is.character(part)) return(part[1])
        if (is.list(part) && !is.null(part$text)) return(as.character(part$text[[1]]))
        ""
      }, character(1))
      return(paste(parts[nzchar(parts)], collapse = "\n"))
    }

    ""
  }

  normalize_reply_text <- function(text, fallback = "") {
    lines <- str_split(as.character(text), "\\r?\\n")[[1]]
    lines <- str_trim(lines, side = "both")
    lines <- lines[nzchar(lines)]
    out <- paste(lines, collapse = "\n")

    # Enforce line break after a heading when a bullet starts on same line.
    out <- str_replace_all(out, ":\\s+-\\s+", ":\n- ")

    if (!nzchar(out)) out <- fallback
    out
  }

  build_section <- function(title, lines) {
    lines <- as.character(lines)
    lines <- lines[nzchar(lines)]
    if (!length(lines)) return(title)
    paste0(title, "\n", paste(lines, collapse = "\n"))
  }

  prepare_shipments <- function(d) {
    if (is.null(d)) return(tibble())

    d <- as_tibble(d)
    if (nrow(d) == 0) return(tibble())

    origin_chr <- as.character(pull_col(d, "origin", "Unknown"))
    destination_chr <- as.character(pull_col(d, "destination", "Unknown"))

    route_chr <- if ("route" %in% names(d)) as.character(d$route) else paste(origin_chr, destination_chr, sep = " -> ")
    route_chr[is.na(route_chr) | !nzchar(route_chr)] <- paste(
      origin_chr[is.na(route_chr) | !nzchar(route_chr)],
      destination_chr[is.na(route_chr) | !nzchar(route_chr)],
      sep = " -> "
    )

    shipment_id_num <- clean_num(pull_col(d, "shipment_id", seq_len(nrow(d))), NA_real_)
    shipment_id_num[is.na(shipment_id_num)] <- seq_len(nrow(d))[is.na(shipment_id_num)]

    delay_flag_chr <- str_to_lower(as.character(pull_col(d, "delay_flag", "on time")))

    tibble(
      shipment_id = as.integer(shipment_id_num),
      route = route_chr,
      origin = origin_chr,
      destination = destination_chr,
      risk_score_num = clean_num(pull_col(d, "risk_score", 0), 0),
      financial_exposure_num = clean_num(pull_col(d, "financial_exposure", 0), 0),
      shipment_value_num = clean_num(pull_col(d, "shipment_value", 0), 0),
      delay_flag_chr = delay_flag_chr,
      delay_bool = delay_flag_chr %in% c("delayed", "1", "true", "t", "yes", "y"),
      risk_level_chr = as.character(pull_col(d, "risk_level", NA_character_)),
      departure_dt = as.POSIXct(pull_col(d, "departure_dt", NA)),
      weather_risk_num = clean_num(pull_col(d, "weather_risk", NA_real_), NA_real_),
      shipping_risk_num = clean_num(pull_col(d, "shipping_risk", NA_real_), NA_real_),
      distance_risk_num = clean_num(pull_col(d, "distance_risk", NA_real_), NA_real_),
      supplier_risk_num = clean_num(pull_col(d, "supplier_risk", NA_real_), NA_real_),
      supplier_reliability_num = clean_num(pull_col(d, "supplier_reliability_score", NA_real_), NA_real_)
    )
  }

  prepare_warehouse <- function(wh, d_shipments) {
    if (!is.null(wh) && nrow(wh) > 0) {
      wh_tbl <- as_tibble(wh)
      wh_name <- as.character(pull_col(wh_tbl, "warehouse", NA_character_))
      dest <- as.character(pull_col(wh_tbl, "destination", "Unknown"))
      wh_name[is.na(wh_name) | !nzchar(wh_name)] <- paste(dest[is.na(wh_name) | !nzchar(wh_name)], "Hub")

      out <- tibble(
        warehouse = wh_name,
        utilization = clean_num(pull_col(wh_tbl, "utilization", NA_real_), NA_real_),
        predicted_utilization = clean_num(pull_col(wh_tbl, "predicted_utilization", NA_real_), NA_real_),
        avg_risk = clean_num(pull_col(wh_tbl, "avg_risk", NA_real_), NA_real_),
        delay_rate = clean_num(pull_col(wh_tbl, "delay_rate", NA_real_), NA_real_),
        shipments = clean_num(pull_col(wh_tbl, "shipments", NA_real_), NA_real_)
      ) %>%
        mutate(predicted_utilization = if_else(is.na(predicted_utilization), utilization, predicted_utilization)) %>%
        distinct(warehouse, .keep_all = TRUE) %>%
        filter(!is.na(warehouse), nzchar(warehouse))

      if (nrow(out) > 0) return(out)
    }

    if (nrow(d_shipments) == 0) return(tibble())

    d_shipments %>%
      group_by(destination) %>%
      summarise(
        warehouse = paste(first(destination), "Hub"),
        utilization = pmin(98, 45 + safe_rescale(sum(shipment_value_num, na.rm = TRUE), c(10, 40)) + mean(delay_bool, na.rm = TRUE) * 25),
        predicted_utilization = pmin(99, utilization + safe_rescale(mean(risk_score_num, na.rm = TRUE), c(2, 10))),
        avg_risk = mean(risk_score_num, na.rm = TRUE),
        delay_rate = mean(delay_bool, na.rm = TRUE),
        shipments = n(),
        .groups = "drop"
      ) %>%
      distinct(warehouse, .keep_all = TRUE)
  }

  build_overview_brief <- function(d, wh) {
    if (nrow(d) == 0) return("Overview is unavailable because shipment data is not loaded yet.")

    total_shipments <- nrow(d)
    high_risk_shipments <- sum(d$risk_level_chr == "High" | d$risk_score_num >= 0.7, na.rm = TRUE)
    delay_rate <- safe_mean_num(d$delay_bool)
    avg_risk <- safe_mean_num(d$risk_score_num)
    total_exposure <- sum(d$financial_exposure_num, na.rm = TRUE)

    top_route <- d %>%
      group_by(route) %>%
      summarise(
        avg_risk = mean(risk_score_num, na.rm = TRUE),
        avg_exposure = mean(financial_exposure_num, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(score = 0.6 * safe_rescale(avg_risk) + 0.4 * safe_rescale(avg_exposure)) %>%
      arrange(desc(score)) %>%
      slice_head(n = 1)

    top_hub <- wh %>%
      arrange(desc(predicted_utilization), desc(utilization)) %>%
      slice_head(n = 1)

    route_line <- if (nrow(top_route) == 0) {
      "- Top route to watch: unavailable"
    } else {
      paste0(
        "- Top route to watch: ", top_route$route,
        " | avg risk ", round(top_route$avg_risk, 2),
        " | avg exposure ", dollar(top_route$avg_exposure)
      )
    }

    hub_line <- if (nrow(top_hub) == 0) {
      "- Highest-stress warehouse: unavailable"
    } else {
      paste0(
        "- Highest-stress warehouse: ", top_hub$warehouse,
        " | predicted utilization ", round(top_hub$predicted_utilization, 1), "%"
      )
    }

    paste(
      "Current operational risk snapshot:",
      paste0("- Total shipments: ", format_number(total_shipments)),
      paste0("- High-risk shipments: ", format_number(high_risk_shipments)),
      paste0("- Delay rate: ", percent(delay_rate, accuracy = 0.1)),
      paste0("- Average risk score: ", round(avg_risk, 2)),
      paste0("- Total financial exposure: ", dollar(total_exposure)),
      route_line,
      hub_line,
      sep = "\n"
    )
  }

  build_top_risk_shipments <- function(d, n = 5L) {
    if (nrow(d) == 0) return("I cannot rank shipment risk because shipment data is unavailable.")

    top <- d %>%
      arrange(desc(risk_score_num), desc(financial_exposure_num), desc(delay_bool)) %>%
      slice_head(n = n)

    lines <- paste0(
      "- #", top$shipment_id, " ", top$route,
      " | risk ", round(top$risk_score_num, 2),
      " | exposure ", dollar(top$financial_exposure_num),
      " | delay ", if_else(top$delay_bool, "Delayed", "On Time")
    )

    build_section("Highest-risk shipments to watch:", lines)
  }

  build_delay_risk_shipments <- function(d, n = 5L) {
    if (nrow(d) == 0) return("I cannot evaluate delay risk because shipment data is unavailable.")

    top <- d %>%
      arrange(desc(delay_bool), desc(risk_score_num), desc(financial_exposure_num)) %>%
      slice_head(n = n)

    lines <- paste0(
      "- #", top$shipment_id, " ", top$route,
      " | delay ", if_else(top$delay_bool, "Delayed", "On Time"),
      " | risk ", round(top$risk_score_num, 2),
      " | exposure ", dollar(top$financial_exposure_num)
    )

    build_section("Top shipments with immediate delay risk:", lines)
  }

  build_routes_brief <- function(d, n = 3L) {
    if (nrow(d) == 0) return("Route vulnerability is unavailable because shipment data is missing.")

    top_routes <- d %>%
      group_by(route) %>%
      summarise(
        delay_rate = mean(delay_bool, na.rm = TRUE),
        avg_risk = mean(risk_score_num, na.rm = TRUE),
        avg_exposure = mean(financial_exposure_num, na.rm = TRUE),
        shipments = n(),
        .groups = "drop"
      ) %>%
      mutate(vulnerability_index = 0.45 * delay_rate + 0.35 * safe_rescale(avg_risk) + 0.20 * safe_rescale(avg_exposure)) %>%
      arrange(desc(vulnerability_index)) %>%
      slice_head(n = n)

    lines <- paste0(
      "- ", top_routes$route,
      " | delay ", percent(top_routes$delay_rate, accuracy = 0.1),
      " | avg risk ", round(top_routes$avg_risk, 2),
      " | avg exposure ", dollar(top_routes$avg_exposure)
    )

    build_section("Most vulnerable routes right now:", lines)
  }

  build_drivers_brief <- function(d, n = 5L) {
    if (nrow(d) == 0) return("Risk driver analysis is unavailable because shipment data is missing.")

    drivers <- tibble(
      driver = c("Distance", "Delay Rate", "Shipping", "Weather", "Supplier"),
      score = c(
        safe_mean_num(d$distance_risk_num, NA_real_),
        safe_mean_num(d$delay_bool, NA_real_),
        safe_mean_num(d$shipping_risk_num, NA_real_),
        safe_mean_num(d$weather_risk_num, NA_real_),
        safe_mean_num(d$supplier_risk_num, NA_real_)
      )
    ) %>%
      filter(is.finite(score)) %>%
      arrange(desc(score)) %>%
      slice_head(n = n)

    if (nrow(drivers) == 0) {
      return("Risk driver fields are not available in the current dataset.")
    }

    lines <- paste0("- ", drivers$driver, ": ", round(drivers$score, 2))
    build_section("Top drivers of risk in the current shipment portfolio:", lines)
  }

  build_financial_brief <- function(d, n = 5L) {
    if (nrow(d) == 0) return("Financial exposure analysis is unavailable because shipment data is missing.")

    top_fin <- d %>%
      arrange(desc(financial_exposure_num), desc(risk_score_num)) %>%
      slice_head(n = n)

    lines <- paste0(
      "- #", top_fin$shipment_id, " ", top_fin$route,
      " | exposure ", dollar(top_fin$financial_exposure_num),
      " | risk ", round(top_fin$risk_score_num, 2),
      " | delay ", if_else(top_fin$delay_bool, "Delayed", "On Time")
    )

    build_section("Highest financial exposure shipments:", lines)
  }

  build_high_risk_low_exposure_brief <- function(d, n = 5L) {
    if (nrow(d) == 0) return("High-risk and low-exposure screening is unavailable because shipment data is missing.")

    risk_cut <- as.numeric(quantile(d$risk_score_num, probs = 0.8, na.rm = TRUE))
    exposure_cut <- as.numeric(quantile(d$financial_exposure_num, probs = 0.4, na.rm = TRUE))

    candidates <- d %>%
      filter(risk_score_num >= risk_cut, financial_exposure_num <= exposure_cut) %>%
      arrange(desc(risk_score_num), financial_exposure_num) %>%
      slice_head(n = n)

    if (nrow(candidates) == 0) {
      # Relax threshold so we still return a useful shortlist.
      risk_cut <- as.numeric(quantile(d$risk_score_num, probs = 0.7, na.rm = TRUE))
      exposure_cut <- as.numeric(quantile(d$financial_exposure_num, probs = 0.5, na.rm = TRUE))

      candidates <- d %>%
        filter(risk_score_num >= risk_cut, financial_exposure_num <= exposure_cut) %>%
        arrange(desc(risk_score_num), financial_exposure_num) %>%
        slice_head(n = n)
    }

    if (nrow(candidates) == 0) {
      return("No shipments meet the high-risk and low-exposure filter in the current data load.")
    }

    lines <- paste0(
      "- #", candidates$shipment_id, " ", candidates$route,
      " | risk ", round(candidates$risk_score_num, 2),
      " | exposure ", dollar(candidates$financial_exposure_num),
      " | delay ", if_else(candidates$delay_bool, "Delayed", "On Time")
    )

    intro <- paste0(
      "High-risk shipments with relatively low exposure ",
      "(risk >= ", round(risk_cut, 2), ", exposure <= ", dollar(exposure_cut), "):"
    )
    build_section(intro, lines)
  }

  build_warehouse_brief <- function(wh, n = 3L) {
    if (nrow(wh) == 0) return("Warehouse stress view is unavailable because warehouse data is missing.")

    top_wh <- wh %>%
      arrange(desc(predicted_utilization), desc(utilization)) %>%
      slice_head(n = n)

    lines <- paste0(
      "- ", top_wh$warehouse,
      " | predicted utilization ", round(top_wh$predicted_utilization, 1), "%",
      " | delay ", percent(top_wh$delay_rate, accuracy = 0.1),
      " | avg risk ", round(top_wh$avg_risk, 2)
    )

    build_section("Warehouse stress ranking:", lines)
  }

  build_historical_brief <- function(d) {
    if (nrow(d) == 0) return("Historical trend analysis is unavailable because shipment data is missing.")

    d_hist <- d %>% filter(!is.na(departure_dt))
    if (nrow(d_hist) == 0) {
      return("Historical trend analysis is limited because timestamp data is not available in this dataset.")
    }

    monthly <- d_hist %>%
      mutate(month = floor_date(departure_dt, unit = "month")) %>%
      group_by(month) %>%
      summarise(
        delay_rate = mean(delay_bool, na.rm = TRUE),
        avg_risk = mean(risk_score_num, na.rm = TRUE),
        avg_exposure = mean(financial_exposure_num, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(month)

    recent <- tail(monthly, min(3, nrow(monthly)))

    trend_line <- if (nrow(monthly) >= 2) {
      delta <- tail(monthly$delay_rate, 1) - monthly$delay_rate[1]
      direction <- if (delta > 0.01) "upward" else if (delta < -0.01) "downward" else "stable"
      paste0("- Delay trend over loaded history is ", direction, " (", percent(delta, accuracy = 0.1), ").")
    } else {
      "- Not enough periods to infer long-term delay trend."
    }

    recent_lines <- paste0(
      "- ", format(recent$month, "%b %Y"),
      " | delay ", percent(recent$delay_rate, accuracy = 0.1),
      " | avg risk ", round(recent$avg_risk, 2),
      " | avg exposure ", dollar(recent$avg_exposure)
    )

    paste(
      "Historical reliability and delay trend:",
      trend_line,
      "Recent periods:",
      paste(recent_lines, collapse = "\n"),
      sep = "\n"
    )
  }

  build_risk_monitor_brief <- function(d, n = 5L) {
    paste(
      build_top_risk_shipments(d, n),
      "",
      build_delay_risk_shipments(d, min(3L, n)),
      "",
      build_drivers_brief(d, min(5L, n)),
      sep = "\n"
    )
  }

  build_metric_explanation <- function(q, d) {
    if (nrow(d) == 0) return(NULL)

    if (str_detect(q, "risk score|\\u98ce\\u9669\\u5206")) {
      avg <- safe_mean_num(d$risk_score_num)
      p90 <- as.numeric(quantile(d$risk_score_num, probs = 0.9, na.rm = TRUE))
      return(paste(
        "Risk Score explanation:",
        "- Definition: composite shipment risk indicator (higher means riskier).",
        paste0("- Current average: ", round(avg, 2)),
        paste0("- Current 90th percentile: ", round(p90, 2)),
        "- Operational use: prioritize monitoring above the portfolio baseline.",
        sep = "\n"
      ))
    }

    if (str_detect(q, "financial exposure|exposure|value at risk|\\u8d22\\u52a1|\\u66b4\\u9732|\\u91d1\\u989d")) {
      total <- sum(d$financial_exposure_num, na.rm = TRUE)
      avg <- safe_mean_num(d$financial_exposure_num)
      p90 <- as.numeric(quantile(d$financial_exposure_num, probs = 0.9, na.rm = TRUE))
      return(paste(
        "Financial Exposure explanation:",
        "- Definition: estimated financial impact if risk materializes.",
        paste0("- Current total exposure: ", dollar(total)),
        paste0("- Current average exposure: ", dollar(avg)),
        paste0("- Current 90th percentile: ", dollar(p90)),
        sep = "\n"
      ))
    }

    if (str_detect(q, "delay rate|delay|late|\\u5ef6\\u8bef")) {
      delay_rate <- safe_mean_num(d$delay_bool)
      return(paste(
        "Delay Rate explanation:",
        "- Definition: share of shipments currently flagged as delayed.",
        paste0("- Current portfolio delay rate: ", percent(delay_rate, accuracy = 0.1)),
        "- Operational use: combine with risk score and exposure to prioritize interventions.",
        sep = "\n"
      ))
    }

    if (str_detect(q, "supplier reliability|supplier|\\u4f9b\\u5e94\\u5546")) {
      avg <- safe_mean_num(d$supplier_reliability_num, NA_real_)
      if (!is.finite(avg)) return("Supplier reliability metric is not available in this dataset.")
      return(paste(
        "Supplier Reliability explanation:",
        "- Definition: score representing expected supplier execution consistency.",
        paste0("- Current average supplier reliability: ", round(avg, 2)),
        "- Lower reliability generally raises shipment risk and potential delays.",
        sep = "\n"
      ))
    }

    NULL
  }

  build_dashboard_guide <- function(d, wh) {
    paste(
      "I can explain every dashboard section. You can ask me about:",
      "- Overview: current risk snapshot and KPI interpretation.",
      "- Global Map: most vulnerable routes and corridor risk.",
      "- Risk Monitor: top risk shipments, delay signals, and drivers.",
      "- Financial Risk: highest exposure shipments and concentration.",
      "- Warehouse & Historical: stressed hubs and trend direction.",
      "",
      build_overview_brief(d, wh),
      sep = "\n"
    )
  }

  build_general_brief <- function(d, wh) {
    paste(
      build_overview_brief(d, wh),
      "",
      build_top_risk_shipments(d, 3),
      "",
      build_financial_brief(d, 3),
      "",
      build_warehouse_brief(wh, 2),
      sep = "\n"
    )
  }

  build_ai_context <- function(d, wh, intent, n = 5L) {
    # Keep context focused so answers stay specific to the asked question.
    if (identical(intent, "delay")) return(build_delay_risk_shipments(d, n))
    if (identical(intent, "drivers")) return(build_drivers_brief(d, n))
    if (identical(intent, "top_risk")) return(build_top_risk_shipments(d, n))
    if (identical(intent, "high_risk_low_exposure")) return(build_high_risk_low_exposure_brief(d, n))
    if (identical(intent, "routes")) return(build_routes_brief(d, n))
    if (identical(intent, "financial")) return(build_financial_brief(d, n))
    if (identical(intent, "warehouse")) return(build_warehouse_brief(wh, min(n, 6L)))
    if (identical(intent, "historical")) return(build_historical_brief(d))
    if (identical(intent, "overview")) return(build_overview_brief(d, wh))
    if (identical(intent, "risk_monitor_summary")) return(build_risk_monitor_brief(d, n))

    paste(
      build_overview_brief(d, wh),
      "",
      build_routes_brief(d, 3),
      "",
      build_financial_brief(d, 3),
      sep = "\n"
    )
  }

  fetch_copilot_ai_reply <- function(question, d, wh, baseline_reply, intent = "general", n = 5L) {
    if (exists("load_supabase_env", mode = "function")) {
      try(load_supabase_env(), silent = TRUE)
    }

    api_key <- Sys.getenv("OPENAI_API_KEY", "")
    if (!nzchar(api_key)) api_key <- Sys.getenv("OPEN_AI_API_KEY", "")

    model <- Sys.getenv("OPENAI_MODEL", "gpt-4.1-mini")
    api_base <- Sys.getenv("OPENAI_API_BASE", "https://api.openai.com/v1")

    if (!nzchar(api_key) || !requireNamespace("httr2", quietly = TRUE)) {
      return(baseline_reply)
    }

    focused_context <- build_ai_context(d, wh, intent = intent, n = n)
    overview_context <- build_overview_brief(d, wh)

    req_body <- list(
      model = model,
      temperature = 0.2,
      messages = list(
        list(
          role = "system",
          content = paste(
            "You are RiskRoute AI Copilot for a supply chain risk dashboard.",
            "Answer only the specific user question.",
            "Do not include unrelated sections.",
            "When listing items, use '-' bullets with one item per line.",
            "Always put a line break after any heading ending with ':'.",
            "Use concrete values from context whenever possible.",
            "If focused context contains shipment IDs, do not claim shipment-level data is unavailable.",
            "If data is unavailable, say so briefly and provide next best guidance.",
            "Keep response concise (4-10 lines unless user asks for more)."
          )
        ),
        list(
          role = "user",
          content = paste(
            "Intent:", intent,
            "",
            "User question:", as.character(question),
            "",
            "Target item count (if relevant):", as.character(n),
            "",
            "Focused context:", focused_context,
            "",
            "Portfolio snapshot:", overview_context,
            "",
            "Fallback baseline answer:", baseline_reply,
            sep = "\n"
          )
        )
      )
    )

    tryCatch({
      resp <- httr2::request(paste0(api_base, "/chat/completions")) %>%
        httr2::req_headers(
          Authorization = paste("Bearer", api_key),
          `Content-Type` = "application/json"
        ) %>%
        httr2::req_body_json(req_body, auto_unbox = TRUE) %>%
        httr2::req_timeout(25) %>%
        httr2::req_perform()

      payload <- httr2::resp_body_json(resp, simplifyVector = FALSE)
      content_text <- extract_content_text(payload$choices[[1]]$message$content)
      ai_reply <- normalize_reply_text(content_text, fallback = baseline_reply)

      # Guardrail: if AI incorrectly claims data is unavailable while baseline has shipment IDs,
      # keep the deterministic answer.
      if (
        str_detect(str_to_lower(ai_reply), "not explicitly listed|data .* needed|not available|insufficient data") &&
        str_detect(baseline_reply, "#\\d+")
      ) {
        return(baseline_reply)
      }

      ai_reply
    }, error = function(e) {
      baseline_reply
    })
  }

  compose_copilot_reply <- function(question, d_raw, wh_raw) {
    q <- str_to_lower(str_squish(as.character(question)))
    n <- extract_top_n(q)

    d <- prepare_shipments(d_raw)
    wh <- prepare_warehouse(wh_raw, d)

    if (nrow(d) == 0) {
      return("Shipment data is not available yet. Please refresh data and try again.")
    }

    is_definition_query <- str_detect(
      q,
      "what is|what's|define|definition|meaning|explain|\\u4ec0\\u4e48\\u662f|\\u5b9a\\u4e49|\\u89e3\\u91ca|\\u542b\\u4e49"
    )

    metric_reply <- if (is_definition_query) build_metric_explanation(q, d) else NULL
    if (!is.null(metric_reply)) {
      return(fetch_copilot_ai_reply(
        question = question,
        d = d,
        wh = wh,
        baseline_reply = metric_reply,
        intent = "definition",
        n = n
      ))
    }

    intent <- "general"

    if (
      str_detect(q, "(high|higher|elevated|\\u9ad8).*(risk|\\u98ce\\u9669).*(low|lower|relatively low|\\u4f4e).*(exposure|financial|\\u66b4\\u9732|\\u8d22\\u52a1)") ||
      str_detect(q, "(low|lower|relatively low|\\u4f4e).*(exposure|financial|\\u66b4\\u9732|\\u8d22\\u52a1).*(high|higher|elevated|\\u9ad8).*(risk|\\u98ce\\u9669)")
    ) {
      intent <- "high_risk_low_exposure"
    } else if (str_detect(q, "delay|late|\\u5ef6\\u8bef|\\u5ef6\\u671f")) {
      intent <- "delay"
    } else if (str_detect(q, "driver|factor|cause|why.*risk|\\u9a71\\u52a8|\\u56e0\\u7d20|\\u539f\\u56e0")) {
      intent <- "drivers"
    } else if (str_detect(q, "(top|highest|most|watch|prioriti|\\u6700\\u9ad8|\\u91cd\\u70b9)") &&
               str_detect(q, "risk|shipment|\\u98ce\\u9669|\\u8fd0\\u5355")) {
      intent <- "top_risk"
    } else if (str_detect(q, "route|map|corridor|lane|vulnerable|\\u8def\\u7ebf|\\u8def\\u7531|\\u5730\\u56fe")) {
      intent <- "routes"
    } else if (str_detect(q, "financial|exposure|value at risk|cost|\\u8d22\\u52a1|\\u66b4\\u9732|\\u91d1\\u989d")) {
      intent <- "financial"
    } else if (str_detect(q, "warehouse|hub|capacity|utilization|stress|\\u4ed3\\u5e93|\\u5229\\u7528\\u7387|\\u538b\\u529b")) {
      intent <- "warehouse"
    } else if (str_detect(q, "historical|trend|over time|reliability|week|month|timeline|\\u5386\\u53f2|\\u8d8b\\u52bf")) {
      intent <- "historical"
    } else if (str_detect(q, "overview|snapshot|overall|summary|portfolio|kpi|\\u603b\\u89c8|\\u6982\\u89c8|\\u6458\\u8981")) {
      intent <- "overview"
    } else if (str_detect(q, "dashboard|all sections|explain everything|what can you do|how to use|help|\\u4eea\\u8868\\u76d8|\\u770b\\u677f|\\u5e2e\\u52a9")) {
      intent <- "dashboard"
    } else if (str_detect(q, "risk monitor summary|risk monitor overview|\\u98ce\\u9669\\u76d1\\u63a7.*(\\u603b\\u7ed3|\\u6982\\u89c8)")) {
      intent <- "risk_monitor_summary"
    }

    baseline <- switch(
      intent,
      delay = build_delay_risk_shipments(d, n),
      drivers = build_drivers_brief(d, n),
      top_risk = build_top_risk_shipments(d, n),
      high_risk_low_exposure = build_high_risk_low_exposure_brief(d, n),
      routes = build_routes_brief(d, min(n, 8L)),
      financial = build_financial_brief(d, n),
      warehouse = build_warehouse_brief(wh, min(n, 6L)),
      historical = build_historical_brief(d),
      overview = build_overview_brief(d, wh),
      dashboard = build_dashboard_guide(d, wh),
      risk_monitor_summary = build_risk_monitor_brief(d, min(n, 6L)),
      build_general_brief(d, wh)
    )

    fetch_copilot_ai_reply(
      question = question,
      d = d,
      wh = wh,
      baseline_reply = baseline,
      intent = intent,
      n = n
    )
  }

  chat_log <- reactiveVal(
    tibble(
      role = "assistant",
      text = "Ready to provide an operational risk briefing across all dashboard sections."
    )
  )

  append_chat <- function(role, text) {
    chat_log(bind_rows(chat_log(), tibble(role = role, text = text)))
  }

  submit_copilot_query <- function(query_text) {
    q <- str_squish(as.character(query_text))
    if (!nzchar(q)) return()

    append_chat("user", q)

    d_raw <- tryCatch(shipments(), error = function(e) tibble())
    wh_raw <- tryCatch(warehouse_base(), error = function(e) tibble())

    answer <- tryCatch(
      compose_copilot_reply(q, d_raw, wh_raw),
      error = function(e) {
        d <- prepare_shipments(d_raw)
        wh <- prepare_warehouse(wh_raw, d)
        build_general_brief(d, wh)
      }
    )

    append_chat("assistant", answer)
  }

  observeEvent(input$ask_copilot, {
    submit_copilot_query(input$copilot_query)
    updateTextInput(session, "copilot_query", value = "")
  })

  observeEvent(input$q_dashboard, {
    prompt <- "Explain this dashboard end-to-end and how to read each section."
    updateTextInput(session, "copilot_query", value = prompt)
    submit_copilot_query(prompt)
  })

  observeEvent(input$q_overview, {
    prompt <- "Overview: What's our current risk snapshot?"
    updateTextInput(session, "copilot_query", value = prompt)
    submit_copilot_query(prompt)
  })

  observeEvent(input$q_routes, {
    prompt <- "Global Map: Which routes are most vulnerable now?"
    updateTextInput(session, "copilot_query", value = prompt)
    submit_copilot_query(prompt)
  })

  observeEvent(input$q_delay, {
    prompt <- "Risk Monitor: Which shipments face highest delay risk?"
    updateTextInput(session, "copilot_query", value = prompt)
    submit_copilot_query(prompt)
  })

  observeEvent(input$q_drivers, {
    prompt <- "Risk Monitor: What factors drive shipment risk most?"
    updateTextInput(session, "copilot_query", value = prompt)
    submit_copilot_query(prompt)
  })

  observeEvent(input$q_exposure, {
    prompt <- "Financial Risk: Which shipments drive top exposure?"
    updateTextInput(session, "copilot_query", value = prompt)
    submit_copilot_query(prompt)
  })

  observeEvent(input$q_warehouse, {
    prompt <- "Warehouse: Which hub is under the most stress?"
    updateTextInput(session, "copilot_query", value = prompt)
    submit_copilot_query(prompt)
  })

  output$copilot_chat <- renderUI({
    log_tbl <- chat_log()
    tagList(
      lapply(seq_len(nrow(log_tbl)), function(i) {
        row <- log_tbl[i, ]
        is_user <- identical(row$role[[1]], "user")

        div(
          class = paste("copilot-msg", if (is_user) "user" else "assistant"),
          if (is_user) div(class = "copilot-msg-meta", "You") else NULL,
          div(class = "copilot-msg-bubble", row$text)
        )
      })
    )
  })
}