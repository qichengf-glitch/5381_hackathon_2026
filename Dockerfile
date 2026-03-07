FROM rocker/shiny-verse:4.3.3

WORKDIR /app

# Install required R packages for this app
RUN R -e "install.packages(c( \
  'bslib','DT','readxl','scales','lubridate','glue', \
  'plotly','leaflet','httr2','jsonlite','dotenv' \
), repos='https://cloud.r-project.org')"

COPY . /app

EXPOSE 8080
ENV PORT=8080

CMD ["R", "-e", "options(shiny.host='0.0.0.0', shiny.port=as.numeric(Sys.getenv('PORT','8080'))); shiny::runApp('/app', launch.browser=FALSE)"]
