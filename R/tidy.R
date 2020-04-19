# Import package operators
#' @importFrom dplyr "%>%"
#' @export
dplyr::`%>%`


#' Tidy the JHU COVID-19 Data
#'
#' @param filepath Path to JHU COVID-19 data
#' @return Tidy data filepath
#' @export
tidy_covid_data <- function(filepath) {
  
  filepath <- normalizePath(filepath)
  df <- data.table::fread(filepath)
  
  columns <- colnames(df)
  
  # Find the county column
  county <- grep(pattern = "^Admin2", x = columns)
  if (length(county) > 0) {
    df <- df %>% dplyr::rename(County = Admin2)
  }
  
  # Find the Province/State column
  province_state <- grep(pattern = "^Province", x = columns)
  province_state <- columns[province_state]
  
  if (province_state == "Province/State") {
    df <- df %>%
      dplyr::rename(
        `Province_State` = `Province/State`,
        `Country_Region` = `Country/Region`
      )
  }
  
  # Rename Latitude and Longtitude columns
  long_ <- grep(pattern = "^Long_", x = columns)
  
  if (length(long_) > 0) {
    df <- df %>% dplyr::rename(Longtitude = Long_)
  }
  
  else {
    df <- df %>% dplyr::rename(Longtitude = Long)
  }
  
  df <- df %>% dplyr::rename(Latitude = Lat)
  
  
  # Find the data columns
  date_columns <- sapply(columns, function(x) {
    x <- as.character(x)
    x <- as.Date(x, format = "%m/%d/%Y")
    x <- is.na(x)
    x <- !all(x)
  })
  date_columns <- columns[date_columns]
  
  
  # Pivot timeseries data into single observation values.
  df <- tidyr::pivot_longer(
    data = df,
    cols = date_columns,
    names_to = "date",
    values_to = "x_total") %>%
    
    dplyr::group_by(Province_State, Country_Region) %>%
    
    # Find the number of cases reported by day.
    # Lag() will default the first value to NA.
    # Make it the first value of list (usually 0),
    #   so that the subtraction doesn't invalidate itself.
    dplyr::mutate(
      x_per_day = ( x_total - dplyr::lag(x = x_total, default = dplyr::first(x_total)) )
    )
  
  
  # Update the columns based on the filename
  filename <- basename(filepath)
  
  confirmed <- grepl(
    pattern = "*_confirmed",
    x = filename,
    ignore.case = TRUE
  )
  
  deaths <- grepl(
    pattern = "*_deaths",
    x = filename,
    ignore.case = TRUE
  )
  
  recovered <- grepl(
    pattern = "*_recovered",
    x = filename,
    ignore.case = TRUE
  )
  
  if (confirmed) {
    df <- dplyr::rename(
      .data = df,
      confirmed_per_day = x_per_day,
      confirmed_total = x_total
    )
  }
  
  else if (deaths) {
    df <- dplyr::rename(
      .data = df,
      deaths_per_day = x_per_day,
      deaths_total = x_total
    )
  }
  
  else if (recovered) {
    df <- dplyr::rename(
      .data = df,
      recovered_per_day = x_per_day,
      recovered_total = x_total
    )
  }
  
  else {
    stop("Unable to determine the file type. Filename should contain either 'deaths', 'confirmed', or 'recovered'")
  }
  
  # Export the tidy data
  filepath <- dirname(filepath)
  filename <- paste0("tidy_", filename)
  filepath <- file.path(filepath, filename)
  
  data.table::fwrite(
    x = df,
    file = filepath
  )
  
  print(paste("File exported:", filepath))
  return(filepath)
  
}

# TODO: remove 
tidy_all_covid <- function() {
  filepath <- "../inst/extdata/"
  files <- list.files(filepath)
  files <- files[grep("^time_", files)]
  files <- lapply(files, function(x) {
    normalizePath(file.path(filepath, x))
  })
  files <- unlist(files)
  
  for (file in files) {
    tidy_covid_data(file)
  }  
}

tidy_mobility_reports <- function() {
  df <- "../inst/extdata/applemobilitytrends-2020-04-13.csv"
  df <- data.table::fread(df)
  
  columns <- colnames(df)
  date_columns <- colnames(df)[-1:-3]
  
  df <- df %>%
    dplyr::filter(.data$geo_type == "country/region") %>%
    dplyr::select(-.data$geo_type) %>%
    dplyr::group_by(.data$region, .data$transportation_type) %>%
    tidyr::pivot_longer(
      cols = -c(.data$region, .data$transportation_type),
      names_to = "date",
      values_to = "values"
    ) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    tidyr::pivot_wider(
      names_from = .data$transportation_type,
      values_from = .data$values
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      iso3c = countrycode::countrycode(.data$region,
                                       origin = "country.name",
                                       destination = "iso3c")
    ) %>%
    dplyr::select(
      .data$iso3c, dplyr::everything(), -.data$region
    ) %>%
    dplyr::mutate(timestamp = Sys.time())
  
  
  View(df)
  
}

tidy_mobility_reports()




