# RiskRoute AI - Shiny Application

## Project Structure

This Shiny application has been modularized for better code organization and maintainability.

### Main Files

- **`app.R`** - Main entry point that loads all modules and initializes the Shiny app
- **`constants.R`** - All constants, configuration values, and data structures
- **`utils.R`** - Utility functions used across the application
- **`ui_global.R`** - Global UI components including styles and main navigation structure

### Modules Directory (`modules/`)

Each page/tab has been separated into its own UI and Server files:

#### Overview Page
- `overview_ui.R` - Overview page UI components
- `overview_server.R` - Overview page server logic

#### Global Map Page
- `global_map_ui.R` - Global map page UI components
- `global_map_server.R` - Global map page server logic

#### Risk Monitor Page
- `risk_monitor_ui.R` - Risk monitor page UI components
- `risk_monitor_server.R` - Risk monitor page server logic

#### Financial Risk Page
- `financial_ui.R` - Financial risk page UI components
- `financial_server.R` - Financial risk page server logic

#### Warehouse & Historical Analytics Page
- `warehouse_ui.R` - Warehouse page UI components
- `warehouse_server.R` - Warehouse page server logic

#### AI Copilot Page
- `copilot_ui.R` - AI Copilot page UI components
- `copilot_server.R` - AI Copilot page server logic

## Running the Application

Simply run:

```r
shiny::runApp("app.R")
```

Or from the command line:

```bash
Rscript -e "shiny::runApp('app.R')"
```

## Data File

The application expects `Final_shipment.xlsx` in the project directory or at the path specified in `constants.R` (`DEFAULT_DATA_PATH`).
