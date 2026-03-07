# AI Copilot Page Server Logic

copilot_server <- function(input, output, session, shipments, warehouse_base) {
  compose_copilot_reply <- function(question, d, wh) {
    q <- str_to_lower(question)

    if (str_detect(q, "highest risk of delay|delay.*highest risk|highest risk.*delay")) {
      top_delay <- d %>%
        mutate(delay_signal = if_else(delay_flag == "Delayed", 1, 0)) %>%
        arrange(desc(delay_signal), desc(risk_score), desc(financial_exposure)) %>%
        slice_head(n = 5)

      lines <- paste0(
        "- #", top_delay$shipment_id, " ", top_delay$route,
        " | risk ", round(top_delay$risk_score, 2),
        " | exposure ", dollar(top_delay$financial_exposure)
      )
      return(paste("Top shipments with immediate delay risk:", paste(lines, collapse = "\n")))
    }

    if (str_detect(q, "routes.*vulnerable|most vulnerable|vulnerable route|which routes")) {
      top_routes <- d %>%
        group_by(route) %>%
        summarise(
          delay_rate = mean(delay_flag == "Delayed", na.rm = TRUE),
          avg_risk = mean(risk_score, na.rm = TRUE),
          exposure = mean(financial_exposure, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(vulnerability_index = 0.45 * delay_rate + 0.35 * safe_rescale(avg_risk) + 0.20 * safe_rescale(exposure)) %>%
        arrange(desc(vulnerability_index)) %>%
        slice_head(n = 3)

      lines <- paste0(
        "- ", top_routes$route,
        " | delay ", percent(top_routes$delay_rate, accuracy = 0.1),
        " | avg risk ", round(top_routes$avg_risk, 2)
      )
      return(paste("Most vulnerable routes right now:", paste(lines, collapse = "\n")))
    }

    if (str_detect(q, "driving shipment risk|factors|drivers")) {
      drivers <- d %>%
        summarise(
          weather = mean(weather_risk, na.rm = TRUE),
          shipping = mean(shipping_risk, na.rm = TRUE),
          distance = mean(distance_risk, na.rm = TRUE),
          supplier = mean(supplier_risk, na.rm = TRUE),
          delay = mean(delay_flag == "Delayed", na.rm = TRUE)
        ) %>%
        pivot_longer(everything(), names_to = "driver", values_to = "score") %>%
        arrange(desc(score))

      lines <- paste0("- ", str_to_title(drivers$driver), ": ", round(drivers$score, 2))
      return(paste("Top drivers of risk in the current shipment portfolio:", paste(lines, collapse = "\n")))
    }

    if (str_detect(q, "highest financial exposure|highest exposure|financial risk")) {
      top_fin <- d %>%
        arrange(desc(financial_exposure), desc(risk_score)) %>%
        slice_head(n = 5)
      lines <- paste0(
        "- #", top_fin$shipment_id, " ", top_fin$route,
        " | exposure ", dollar(top_fin$financial_exposure),
        " | risk ", round(top_fin$risk_score, 2)
      )
      return(paste("Highest financial exposure shipments:", paste(lines, collapse = "\n")))
    }

    if (str_detect(q, "warehouse|stress|capacity")) {
      top_wh <- wh %>% arrange(desc(utilization)) %>% slice(1)
      return(
        paste0(
          top_wh$warehouse, " is currently under the highest predicted stress at ",
          round(top_wh$utilization, 1), "% utilization. ",
          "Delay rate is ", percent(top_wh$delay_rate, accuracy = 0.1),
          " and average route risk is ", round(top_wh$avg_risk, 2), "."
        )
      )
    }

    "I can help with shipment risk, route vulnerability, delay signals, exposure prioritization, and warehouse stress. Please ask a logistics risk question tied to this dashboard context."
  }

  chat_log <- reactiveVal(
    tibble(
      role = "assistant",
      text = "Ask about delays, vulnerable routes, risk drivers, or financial exposure to get a concise operational briefing."
    )
  )

  append_chat <- function(role, text) {
    chat_log(bind_rows(chat_log(), tibble(role = role, text = text)))
  }

  submit_copilot_query <- function(query_text) {
    q <- str_squish(query_text)
    if (!nzchar(q)) return()
    append_chat("user", q)
    answer <- compose_copilot_reply(q, shipments(), warehouse_base())
    append_chat("assistant", answer)
  }

  observeEvent(input$ask_copilot, {
    submit_copilot_query(input$copilot_query)
    updateTextInput(session, "copilot_query", value = "")
  })

  observeEvent(input$q_delay, {
    prompt <- "Which shipments are at highest risk of delay?"
    updateTextInput(session, "copilot_query", value = prompt)
    submit_copilot_query(prompt)
  })

  observeEvent(input$q_routes, {
    prompt <- "Which routes are most vulnerable?"
    updateTextInput(session, "copilot_query", value = prompt)
    submit_copilot_query(prompt)
  })

  observeEvent(input$q_drivers, {
    prompt <- "What factors are driving shipment risk?"
    updateTextInput(session, "copilot_query", value = prompt)
    submit_copilot_query(prompt)
  })

  observeEvent(input$q_exposure, {
    prompt <- "Which shipments create the highest financial exposure?"
    updateTextInput(session, "copilot_query", value = prompt)
    submit_copilot_query(prompt)
  })

  observeEvent(input$q_warehouse, {
    prompt <- "Which warehouse is under the most stress?"
    updateTextInput(session, "copilot_query", value = prompt)
    submit_copilot_query(prompt)
  })

  output$copilot_chat <- renderUI({
    log_tbl <- chat_log()
    tagList(
      lapply(seq_len(nrow(log_tbl)), function(i) {
        row <- log_tbl[i, ]
        div(
          class = paste("chat-msg", if_else(row$role == "user", "chat-user", "chat-assistant")),
          div(class = "chat-role", if_else(row$role == "user", "You", "RiskRoute AI Copilot")),
          div(class = "chat-bubble", row$text)
        )
      })
    )
  })
}
