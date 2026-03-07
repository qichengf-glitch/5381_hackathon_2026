# Financial Risk Page Server Logic (Financial-Risk variant)

financial_server <- function(input, output, session, shipments) {
  high_exposure_quantile <- 0.90

  as_numeric_vec <- function(x) {
    # parse_number handles values like "$194,692" or "194,692.00"
    suppressWarnings(readr::parse_number(as.character(x)))
  }

  safe_mean_numeric <- function(x) {
    valid <- x[is.finite(x)]
    if (!length(valid)) return(0)
    mean(valid)
  }

  calculate_high_exposure_threshold <- function(d) {
    exposure <- as_numeric_vec(d$financial_exposure)
    exposure <- exposure[is.finite(exposure)]
    if (!length(exposure)) return(NA_real_)
    as.numeric(stats::quantile(exposure, probs = high_exposure_quantile, na.rm = TRUE, names = FALSE))
  }

  calculate_high_exposure_count <- function(d, threshold) {
    exposure <- as_numeric_vec(d$financial_exposure)
    exposure <- exposure[is.finite(exposure)]
    if (!length(exposure) || !is.finite(threshold)) return(0L)
    as.integer(sum(exposure >= threshold))
  }

  default_financial_summary <- function() {
    list(
      key_finding = c(
        "Most financial exposure is concentrated in a small set of high-value, high-risk shipments.",
        "A limited subset of routes accounts for a disproportionate share of exposure."
      ),
      main_driver = c(
        "High shipment values combined with elevated risk scores are the primary contributors.",
        "Delay-prone high-value shipments on major routes amplify total financial exposure."
      ),
      recommended_action = c(
        "Prioritize daily monitoring for the top exposed shipments and their route-level risk signals.",
        "Trigger mitigation actions first for shipments with both high value and high delay risk."
      ),
      source = "fallback",
      model = Sys.getenv("OPENAI_MODEL", "gpt-4.1-mini")
    )
  }

  build_financial_context <- function(d, threshold) {
    d_ctx <- d %>%
      mutate(
        financial_exposure_num = as_numeric_vec(financial_exposure),
        risk_score_num = as_numeric_vec(risk_score)
      )

    top_routes <- d_ctx %>%
      filter(is.finite(financial_exposure_num)) %>%
      group_by(route) %>%
      summarise(
        total_exposure = sum(financial_exposure_num, na.rm = TRUE),
        avg_risk = mean(risk_score_num, na.rm = TRUE),
        delay_rate = mean(delay_flag == "Delayed", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(total_exposure)) %>%
      slice_head(n = 3)

    route_summary <- if (!nrow(top_routes)) {
      "none"
    } else {
      paste0(
        top_routes$route,
        " (exposure ", dollar(top_routes$total_exposure),
        ", avg risk ", round(top_routes$avg_risk, 2),
        ", delay ", percent(top_routes$delay_rate, accuracy = 0.1), ")",
        collapse = "; "
      )
    }

    top_shipments <- d_ctx %>%
      filter(is.finite(financial_exposure_num)) %>%
      arrange(desc(financial_exposure_num), desc(risk_score_num)) %>%
      slice_head(n = 5)

    shipment_summary <- if (!nrow(top_shipments)) {
      "none"
    } else {
      paste0(
        "#", top_shipments$shipment_id,
        " ", top_shipments$route,
        " (exposure ", dollar(top_shipments$financial_exposure_num),
        ", risk ", round(top_shipments$risk_score_num, 2), ")",
        collapse = "; "
      )
    }

    avg_exposure <- safe_mean_numeric(d_ctx$financial_exposure_num)
    high_count <- calculate_high_exposure_count(d_ctx, threshold)

    paste(
      glue("Total shipments: {nrow(d_ctx)}."),
      glue("Average financial exposure: {dollar(avg_exposure)}."),
      glue("High-exposure threshold ({percent(high_exposure_quantile, accuracy = 1)} quantile): {ifelse(is.finite(threshold), dollar(threshold), 'NA')}."),
      glue("High-exposure shipment count: {high_count}."),
      glue("Top exposure routes: {route_summary}."),
      glue("Top exposed shipments: {shipment_summary}."),
      sep = "\n"
    )
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

  normalize_bullets <- function(items, fallback_items) {
    if (is.null(items)) items <- character(0)
    if (!is.character(items)) items <- as.character(items)

    if (length(items) == 1) {
      items <- unlist(str_split(items, "\\n|;"), use.names = FALSE)
    }

    items <- str_squish(items)
    items <- str_replace(items, "^[-*â€¢]\\s*", "")
    items <- str_replace(items, "^[0-9]+[\\).]\\s*", "")
    items <- items[nzchar(items)]

    if (!length(items)) items <- fallback_items
    unique(head(items, 4))
  }

  parse_summary_sections <- function(text, fallback) {
    lines <- str_split(as.character(text), "\\r?\\n")[[1]]
    lines <- str_squish(lines)
    lines <- lines[nzchar(lines)]

    sections <- list(
      key_finding = character(0),
      main_driver = character(0),
      recommended_action = character(0)
    )

    current <- NULL

    for (line in lines) {
      lower <- str_to_lower(line)

      if (str_detect(lower, "^key\\s*finding\\s*:?") ) {
        current <- "key_finding"
        remainder <- str_squish(str_replace(line, regex("^key\\s*finding\\s*:?") , ""))
        if (nzchar(remainder)) sections[[current]] <- c(sections[[current]], remainder)
        next
      }

      if (str_detect(lower, "^main\\s*driver\\s*:?") ) {
        current <- "main_driver"
        remainder <- str_squish(str_replace(line, regex("^main\\s*driver\\s*:?") , ""))
        if (nzchar(remainder)) sections[[current]] <- c(sections[[current]], remainder)
        next
      }

      if (str_detect(lower, "^recommended\\s*action\\s*:?") ) {
        current <- "recommended_action"
        remainder <- str_squish(str_replace(line, regex("^recommended\\s*action\\s*:?") , ""))
        if (nzchar(remainder)) sections[[current]] <- c(sections[[current]], remainder)
        next
      }

      cleaned <- str_squish(str_replace(line, "^[-*â€¢]\\s*", ""))
      cleaned <- str_squish(str_replace(cleaned, "^[0-9]+[\\).]\\s*", ""))

      if (!is.null(current) && nzchar(cleaned)) {
        sections[[current]] <- c(sections[[current]], cleaned)
      }
    }

    list(
      key_finding = normalize_bullets(sections$key_finding, fallback$key_finding),
      main_driver = normalize_bullets(sections$main_driver, fallback$main_driver),
      recommended_action = normalize_bullets(sections$recommended_action, fallback$recommended_action)
    )
  }

  fetch_financial_ai_summary <- function(d, threshold) {
    fallback <- default_financial_summary()
    api_key <- Sys.getenv("OPENAI_API_KEY", "")
    if (!nzchar(api_key)) api_key <- Sys.getenv("OPEN_AI_API_KEY", "")

    model <- Sys.getenv("OPENAI_MODEL", "gpt-4.1-mini")
    api_base <- Sys.getenv("OPENAI_API_BASE", "https://api.openai.com/v1")
    fallback$model <- model

    if (!nzchar(api_key) || !requireNamespace("httr2", quietly = TRUE)) {
      return(fallback)
    }

    prompt_context <- build_financial_context(d, threshold)

    req_body <- list(
      model = model,
      temperature = 0.2,
      messages = list(
        list(
          role = "system",
          content = paste(
            "You are a financial risk analyst for logistics operations.",
            "Return plain text in exactly three sections with these headings:",
            "Key Finding",
            "Main driver",
            "Recommended action",
            "Under each heading, provide 2-3 concise bullet points using '-' prefix.",
            "Do not add any intro or footer."
          )
        ),
        list(
          role = "user",
          content = paste(
            "Use this context to write the summary:",
            prompt_context,
            sep = "\n\n"
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
      parsed <- parse_summary_sections(content_text, fallback)

      list(
        key_finding = parsed$key_finding,
        main_driver = parsed$main_driver,
        recommended_action = parsed$recommended_action,
        source = "ai",
        model = model
      )
    }, error = function(e) {
      fallback
    })
  }

  high_exposure_threshold <- reactive({
    d <- shipments()
    calculate_high_exposure_threshold(d)
  })

  output$kpi_financial_avg <- renderUI({
    d <- shipments()
    avg_exposure <- safe_mean_numeric(as_numeric_vec(d$financial_exposure))

    kpi_card(
      "Average Exposure per Shipment",
      dollar(avg_exposure),
      "Mean value at risk per route movement",
      "chart-column"
    )
  })

  output$kpi_financial_focus <- renderUI({
    d <- shipments()
    threshold <- high_exposure_threshold()
    high_count <- calculate_high_exposure_count(d, threshold)
    high_count_num <- suppressWarnings(as.numeric(high_count))
    if (!is.finite(high_count_num)) {
      high_count_num <- 0
    }

    subtitle <- if (is.finite(threshold)) {
      glue("Exposure >= {dollar(threshold)} ({percent(high_exposure_quantile, accuracy = 1)} quantile)")
    } else {
      "High-exposure threshold unavailable"
    }

    kpi_card(
      "High-Exposure Shipments",
      scales::comma(high_count_num),
      subtitle,
      "triangle-exclamation"
    )
  })

  output$financial_scatter <- renderChart({
    d_plot <- shipments() %>%
      mutate(
        shipment_value_num = as_numeric_vec(shipment_value),
        risk_score_num = as_numeric_vec(risk_score),
        financial_exposure_num = as_numeric_vec(financial_exposure)
      ) %>%
      filter(
        is.finite(shipment_value_num),
        is.finite(risk_score_num),
        is.finite(financial_exposure_num)
      )

    if (!nrow(d_plot)) {
      p <- ggplot() +
        annotate("text", x = 0, y = 0, label = "No valid data for financial scatter.") +
        theme_void()
      return(finalize_chart(p, tooltip = "text"))
    }

    p <- ggplot(
      d_plot,
      aes(
        x = shipment_value_num,
        y = risk_score_num,
        color = risk_level,
        size = financial_exposure_num,
        text = paste0(
          "Shipment #", shipment_id,
          "<br>Route: ", route,
          "<br>Shipment Value: ", dollar(shipment_value_num),
          "<br>Risk Score: ", round(risk_score_num, 2),
          "<br>Exposure: ", dollar(financial_exposure_num)
        )
      )
    ) +
      geom_point(alpha = 0.42, stroke = 0.2) +
      scale_color_manual(values = RISK_COLORS) +
      scale_size_continuous(range = c(1.2, 5.8), trans = "sqrt") +
      labs(x = "Shipment Value", y = "Risk Score", color = "Risk Level", size = "Exposure") +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        legend.key.height = grid::unit(0.6, "lines")
      )

    finalize_chart(p, tooltip = "text")
  })

  output$financial_top_table <- renderDT({
    d <- shipments() %>%
      mutate(
        origin = as.character(origin),
        destination = as.character(destination),
        shipping_method = as.character(shipping_method),
        shipment_value_num = as_numeric_vec(shipment_value),
        financial_exposure_num = as_numeric_vec(financial_exposure),
        risk_score_num = as_numeric_vec(risk_score)
      ) %>%
      arrange(
        desc(replace_na(financial_exposure_num, -Inf)),
        desc(replace_na(risk_score_num, -Inf))
      ) %>%
      transmute(
        Origin = origin,
        Destination = destination,
        `Shipping Method` = shipping_method,
        `Shipment Value` = shipment_value_num,
        `Financial Exposure` = financial_exposure_num,
        `Risk Score` = round(risk_score_num, 2)
      ) %>%
      slice_head(n = 10)

    if (nrow(d) == 0) {
      return(datatable(
        tibble(Note = "No financial exposure rows available."),
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE)
      ))
    }

    exposure_vals <- d$`Financial Exposure`[is.finite(d$`Financial Exposure`)]
    exposure_cuts <- if (length(exposure_vals) > 0) {
      unique(as.numeric(stats::quantile(exposure_vals, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)))
    } else {
      numeric(0)
    }
    exposure_colors_full <- c("#FFFBE6", "#FEF2C7", "#FDE68A", "#FCD34D")
    exposure_colors <- exposure_colors_full[seq_len(length(exposure_cuts) + 1)]

    datatable(
      d,
      rownames = FALSE,
      class = "stripe hover compact",
      options = list(
        dom = "t",
        ordering = FALSE,
        autoWidth = TRUE,
        pageLength = 10
      )
    ) %>%
      formatCurrency(c("Shipment Value", "Financial Exposure"), currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatStyle(
        "Financial Exposure",
        backgroundColor = styleInterval(exposure_cuts, exposure_colors),
        color = "#6B4F00",
        fontWeight = "700"
      )
  }, server = FALSE)

  financial_ai_summary <- reactive({
    d <- shipments()
    threshold <- high_exposure_threshold()
    fetch_financial_ai_summary(d, threshold)
  })

  output$financial_ai_summary <- renderUI({
    s <- financial_ai_summary()

    section_block <- function(title, items) {
      div(
        style = "margin-bottom: 1rem;",
        tags$p(tags$strong(title), style = "margin-bottom: 0.35rem; font-size: 1.14rem;"),
        tags$ul(
          style = "margin-bottom: 0; padding-left: 1.35rem; font-size: 1.04rem; line-height: 1.6;",
          lapply(items, function(item) {
            tags$li(tags$span(item))
          })
        )
      )
    }

    div(
      class = "stacked-panel",
      section_block("Key Finding", s$key_finding),
      section_block("Main driver", s$main_driver),
      section_block("Recommended action", s$recommended_action),
      tags$p(
        "AI-generated content, please verify.",
        style = "font-size: 0.85rem; color: #6B7280; margin-top: 0.25rem; margin-bottom: 0;"
      )
    )
  })
}
