# RiskRoute AI - Shiny Application

Enterprise-style Shiny dashboard for supply chain risk intelligence, with six tabs:

- Overview
- Global Map
- Risk Monitor
- Financial Risk
- Warehouse & Historical Analytics
- AI Copilot

## Project Structure

The app is modularized for maintainability and team collaboration.

### Main Files

- `app.R` - application entry point; loads modules, initializes reactives, and starts Shiny
- `constants.R` - shared constants (table names, colors, required columns, coordinates)
- `utils.R` - shared helpers (data loading/cleaning, formatting, chart wrappers)
- `ui_global.R` - global theme, styles, and top-level navbar

### Modules (`modules/`)

#### Overview
- `overview_ui.R`
- `overview_server.R`

#### Global Map
- `global_map_ui.R`
- `global_map_server.R`

#### Risk Monitor
- `risk_monitor_ui.R`
- `risk_monitor_server.R`

#### Financial Risk
- `financial_ui_financialRisk.R`
- `financial_server_financialRisk.R`

#### Warehouse & Historical Analytics
- `warehouse_ui.R`
- `warehouse_server.R`

#### AI Copilot
- `copilot_ui.R`
- `copilot_server.R`

## Run the App

In R:

```r
shiny::runApp("app.R")
```

In terminal:

```bash
Rscript -e "shiny::runApp('app.R')"
```

If port `3838` is occupied:

```bash
Rscript -e "shiny::runApp('app.R', port = 3840)"
```

## Deploy (Docker Platforms)

This repo now includes a root `Dockerfile` for platforms that require component auto-detection (for example, DigitalOcean App Platform).

If a platform shows `No components detected`, verify:

- Repository root contains `Dockerfile` (this project does).
- Source directory is set to repository root (`/`).
- Build type is Dockerfile (not Node/Python auto-detect).

Required runtime environment variables:

- `SUPABASE_URL`
- `SUPABASE_SERVICE_ROLE_KEY` (or `SUPABASE_ANON_KEY`)
- `PORT` (usually injected by the platform)

Container start command uses:

- host: `0.0.0.0`
- port: `Sys.getenv('PORT', '8080')`

## Data Source (Current Version)

`app.R` currently loads data from Supabase via:

- `load_shipment_data()` -> `final_shipment`
- `load_historical_data()` -> `historical_shipments`
- `load_warehouse_status_data()` -> `warehouse_status_by_cargo`

Environment variables are loaded from `.env` or `supabase_setup/.env`.

Required:

- `SUPABASE_URL`
- `SUPABASE_SERVICE_ROLE_KEY` (preferred) or `SUPABASE_ANON_KEY`

Notes:

- Supabase fetch uses pagination (`limit` + `offset`) to avoid the 1000-row cap.
- Keep `.env` out of git history.

## Development Notes

- Optional packages: `plotly`, `leaflet` (app degrades gracefully if unavailable)
- Local cleaning helpers still exist in `utils.R` (e.g., `load_shipment_data_local()`), but main app flow is Supabase-first in this version.
