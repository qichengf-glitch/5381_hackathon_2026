# Constants and Configuration

DEFAULT_DATA_PATH <- "/Users/qichengfu/Desktop/5381_hackathon/Final_shipment.xlsx"
SUPABASE_TABLE_FINAL <- "final_shipment"
SUPABASE_TABLE_HISTORICAL <- "historical_shipments"
SUPABASE_TABLE_WAREHOUSE <- "warehouse_status_by_cargo"

RISK_COLORS <- c(
  Low = "#2E7D32",
  Medium = "#D39B33",
  High = "#C6473B"
)

METHOD_COLORS <- c(
  Truck = "#5B7CFA",
  Sea = "#0A6E8A",
  Air = "#4AA6A0",
  Rail = "#7B8D40"
)

CITY_COORDS <- tribble(
  ~city, ~lat, ~lng,
  "Shanghai", 31.2304, 121.4737,
  "Istanbul", 41.0082, 28.9784,
  "Berlin", 52.5200, 13.4050,
  "Tokyo", 35.6762, 139.6503,
  "Seoul", 37.5665, 126.9780,
  "Bangkok", 13.7563, 100.5018,
  "Kuala Lumpur", 3.1390, 101.6869,
  "Singapore", 1.3521, 103.8198,
  "Paris", 48.8566, 2.3522,
  "Hangzhou", 30.2741, 120.1551,
  "Moscow", 55.7558, 37.6173,
  "Dubai", 25.2048, 55.2708,
  "Shenzhen", 22.5431, 114.0579,
  "Guangzhou", 23.1291, 113.2644,
  "Nanjing", 32.0603, 118.7969,
  "Beijing", 39.9042, 116.4074
)

required_cols <- c(
  "origin", "destination", "shipping_method", "weather_condition",
  "supplier_reliability_score", "shipment_value", "shipping_distance",
  "delay_flag", "delay_hours", "risk_score", "financial_exposure",
  "weather_risk", "shipping_risk", "distance_risk", "supplier_risk"
)
