FROM rocker/geospatial:4.4.2

WORKDIR /app

# rocker/geospatial already includes: sf, terra, units, leaflet, raster, and all system deps
# Only need to install the remaining app-specific packages
RUN R -e "install.packages(c( \
  'shiny','bslib','DT','readxl','scales','lubridate','glue', \
  'plotly','httr2','jsonlite','dotenv','future','promises' \
), repos='https://cloud.r-project.org')"

# Verify leaflet is available (it comes with rocker/geospatial)
RUN R -e "if (!requireNamespace('leaflet', quietly=TRUE)) stop('leaflet not available')"

COPY . /app

EXPOSE 8080
ENV PORT=8080

CMD ["R", "-e", "options(shiny.host='0.0.0.0', shiny.port=as.numeric(Sys.getenv('PORT','8080'))); shiny::runApp('/app', launch.browser=FALSE)"]
