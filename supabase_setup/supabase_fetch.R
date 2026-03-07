suppressPackageStartupMessages({
  library(dotenv)
  library(httr2)
  library(jsonlite)
  library(dplyr)
})

# Load env file from project root or this folder
if (file.exists(".env")) {
  dotenv::load_dot_env(".env", override = TRUE)
}
if (file.exists("supabase_setup/.env")) {
  dotenv::load_dot_env("supabase_setup/.env", override = TRUE)
}

supabase_url <- Sys.getenv("SUPABASE_URL")
supabase_key <- Sys.getenv("SUPABASE_SERVICE_ROLE_KEY")

if (!nzchar(supabase_url) || !nzchar(supabase_key)) {
  stop("Missing SUPABASE_URL or SUPABASE_SERVICE_ROLE_KEY in environment.")
}

fetch_table <- function(table_name, select_cols = "*", limit = 5000) {
  req <- request(sprintf("%s/rest/v1/%s", supabase_url, table_name)) |>
    req_url_query(select = select_cols, limit = limit) |>
    req_headers(
      apikey = supabase_key,
      Authorization = paste("Bearer", supabase_key)
    )

  resp <- req_perform(req)
  txt <- resp_body_string(resp)
  fromJSON(txt, flatten = TRUE) |> as_tibble()
}

# Your three tables
final_shipment <- fetch_table("final_shipment")
historical_shipments <- fetch_table("historical_shipments")
warehouse_status_by_cargo <- fetch_table("warehouse_status_by_cargo")

cat("Loaded rows:\n")
cat("final_shipment:", nrow(final_shipment), "\n")
cat("historical_shipments:", nrow(historical_shipments), "\n")
cat("warehouse_status_by_cargo:", nrow(warehouse_status_by_cargo), "\n")
