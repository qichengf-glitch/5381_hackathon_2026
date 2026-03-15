FROM rocker/shiny-verse:4.3.3

WORKDIR /app

# Geospatial/system dependencies needed by leaflet -> sf/s2/terra/units on cloud builders
RUN apt-get update && apt-get install -y --no-install-recommends \
  cmake \
  gdal-bin \
  libgdal-dev \
  libgeos-dev \
  libproj-dev \
  libudunits2-dev \
  libabsl-dev \
  && rm -rf /var/lib/apt/lists/*

# Avoid inherited local Makevars/CMAKE paths (e.g. macOS CMake path)
ENV R_MAKEVARS_USER=/dev/null
ENV CMAKE=/usr/bin/cmake

# Install required R packages for this app
RUN R -e "install.packages(c( \
  'shiny','bslib','DT','readxl','scales','lubridate','glue', \
  'plotly','leaflet','httr2','jsonlite','dotenv' \
), repos='https://cloud.r-project.org')"

# Install leaflet separately and verify it loaded
RUN R -e "install.packages('leaflet', repos='https://cloud.r-project.org'); if (!requireNamespace('leaflet', quietly=TRUE)) stop('leaflet failed to install')"

COPY . /app

EXPOSE 8080
ENV PORT=8080

CMD ["R", "-e", "options(shiny.host='0.0.0.0', shiny.port=as.numeric(Sys.getenv('PORT','8080'))); shiny::runApp('/app', launch.browser=FALSE)"]
