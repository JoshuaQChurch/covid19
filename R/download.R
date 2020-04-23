#' Download and tidy the United States Google Mobility Report data
#'
#' @return A data table containing the data.
#' @export
download_google_mr_us <- function() {
  url <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"

  dt <- data.table::fread(url) %>%
    dplyr::filter(.data$country_region_code == "US") %>%
    dplyr::rename(
      country = country_region,
      state = sub_region_1,
      county = sub_region_2,
      retail_and_recreation = retail_and_recreation_percent_change_from_baseline,
      grocery_and_pharmacy = grocery_and_pharmacy_percent_change_from_baseline,
      parks = parks_percent_change_from_baseline,
      transit_stations = transit_stations_percent_change_from_baseline,
      workplaces = workplaces_percent_change_from_baseline,
      residential = residential_percent_change_from_baseline
    ) %>%
    dplyr::mutate(county = gsub(" County", "", county, ignore.case = TRUE))

  # Remove "Parish" from "Louisiana" counties
  dt[dt$state == "Louisiana", ]$county <- gsub(" Parish", "", dt[dt$state == "Louisiana", ]$county)

  return(dt)
}


#' Download and tidy the Johns Hopkins University COVID-19 data for the United States

#' @return A data table containing the data.
#' @export
download_jhu_covid_19 <- function() {

  # JHU GitHub COVID-19 repo
  covid_19_repo <- paste0(
    "https://raw.githubusercontent.com/",
    "CSSEGISandData/COVID-19/master/csse_covid_19_data/",
    "csse_covid_19_time_series/"
  )

  confirmed_us <- paste0(covid_19_repo, "time_series_covid19_confirmed_US.csv")
  deaths_us <- paste0(covid_19_repo, "time_series_covid19_deaths_US.csv")

  # Tidy the US-specific confirmed and death datasets.
  tidy_us <- function(dt, case_type = "confirmed") {

    start_date <- grep("1/22/20", colnames(dt))
    end_date <- ncol(dt)
    date_columns <- colnames(dt)[start_date:end_date]

    keep_cols <- c(
      "UID",
      "FIPS",
      "Admin2",
      "Province_State",
      "Country_Region",
      "Lat",
      "Long_",
      date_columns
    )

    dt <- dt %>%
      dplyr::select(dplyr::one_of(keep_cols)) %>%
      dplyr::rename(
        fips = FIPS,
        county = Admin2,
        state = Province_State,
        country = Country_Region,
        latitude = Lat,
        longtitude = Long_
      ) %>%
      dplyr::group_by(UID) %>%
      tidyr::pivot_longer(
        cols = date_columns,
        names_to = "date",
        values_to = "x_total"
      ) %>%
      dplyr::mutate(
        x_per_day = ( x_total - dplyr::lag(x = x_total, default = dplyr::first(x_total)) )
      )

    if (case_type == "confirmed") {
      dt <- dt %>%
        dplyr::rename(
          confirmed_total = x_total,
          confirmed_per_day = x_per_day
        )
    }

    else {
      dt <- dt %>%
        dplyr::rename(
          deaths_total = x_total,
          deaths_per_day = x_per_day
        )
    }

    # Drop rows without a valid FIPS code.
    dt <- dt[complete.cases(dt[, "fips"]), ]

    return(dt)
  }

  # Tidy the "Confirmed" US cases
  dt_confirmed_us <- data.table::fread(confirmed_us)
  dt_confirmed_us <- tidy_us(dt_confirmed_us)

  # Tidy the "Deaths" US cases
  dt_deaths_us <- data.table::fread(deaths_us)
  dt_deaths_us <- tidy_us(dt_deaths_us, case_type = "deaths")

  # Merge the datasets into one.
  dt <- dplyr::right_join(
    x = dt_confirmed_us,
    y = dt_deaths_us,
    by = c(
      "UID",
      "fips",
      "county",
      "state",
      "country",
      "latitude",
      "longtitude",
      "date"
    )
  )

  dt$UID <- NULL
  dt <- data.table::as.data.table(dt)

  # Create state-specific data
  states_dt <- data.table::data.table()

  for (.state in unique(dt$state)) {

    state_dt <- dt[dt$state == .state, ]

    if (length(unique(state_dt$county)) == 1) { next }

    dates <- unique(state_dt$date)

    fips <- tryCatch({ usmap::fips(.state) })
    if (is.na(fips)) { fips <- state_dt$fips[1] }

    confirmed_total <- lapply(dates, function(x) {
      sum(state_dt[state_dt$date == x, ]$confirmed_total)
    })

    confirmed_per_day <- lapply(dates, function(x) {
      sum(state_dt[state_dt$date == x, ]$confirmed_per_day)
    })

    deaths_total <- lapply(dates, function(x) {
      sum(state_dt[state_dt$date == x, ]$deaths_total)
    })

    deaths_per_day <- lapply(dates, function(x) {
      sum(state_dt[state_dt$date == x, ]$deaths_per_day)
    })

    x <- length(dates)

    state <- data.table::data.table(
      fips = rep(fips, x),
      county = rep('', x),
      state = rep(.state, x),
      country = rep("US", x),
      latitude = rep('', x),
      longtitude = rep('', x),
      date = dates,
      confirmed_total = as.numeric(confirmed_total),
      confirmed_per_day = as.numeric(confirmed_per_day),
      deaths_total = as.numeric(deaths_total),
      deaths_per_day = as.numeric(deaths_per_day)
    )

    states_dt <- rbind(
      states_dt, state
    )
  }

  # Bind to the original data
  dt <- rbind(dt, states_dt)

  return(dt)

}



#' Download and tidy the US Department of Agriculture data for Education, Unemployment/Income, and Population
#'
#' @param path File download path
#' @export
download_usda <- function() {

  # Download and tidy the "Education" data
  education_url <- "https://www.ers.usda.gov/webdocs/DataFiles/48747/Education.csv?v=568.3"
  education_dt <- data.table::fread(education_url)
  columns <- colnames(education_dt)
  education_levels <- columns[grep("*2014-", columns)]
  columns <- c(columns[1:3], education_levels)
  education_dt <- education_dt[, columns, with = FALSE] %>%
    dplyr::rename(fips = `FIPS Code`)

  # Download and tidy the "Income" data
  unemployment_url <- "https://www.ers.usda.gov/webdocs/DataFiles/48747/Unemployment.csv?v=2564.4"
  unemployment_dt <- data.table::fread(unemployment_url)
  unemployment_dt <- unemployment_dt %>%
    dplyr::select(FIPS, Median_Household_Income_2018) %>%
    dplyr::mutate(Median_Household_Income_2018 = as.numeric(gsub("[\\$,]", "", Median_Household_Income_2018))) %>%
    dplyr::rename(
      fips = FIPS,
      median_household_income_2018 = Median_Household_Income_2018
    )

  # Download and tidy the "Populuation" data
  population_url <- "https://www.ers.usda.gov/webdocs/DataFiles/48747/PopulationEstimates.csv?v=3011.3"
  population_dt <- data.table::fread(population_url)
  population_dt <- population_dt %>%
    dplyr::select(FIPS, POP_ESTIMATE_2018) %>%
    dplyr::rename(
      fips = FIPS,
      population_est_2018 = POP_ESTIMATE_2018
    ) %>%
    dplyr::mutate(
      population_est_2018 = as.numeric(gsub("[\\$,]", "", population_est_2018))
    )

  # Merge into one dataset
  dt <- dplyr::right_join(
    x = education_dt,
    y = unemployment_dt,
    by = "fips"
  )

  dt <- dplyr::right_join(
    x = dt,
    y = population_dt,
    by = "fips"
  ) %>%

  dplyr::rename(
    state = State,
    county = `Area name`,
    less_than_highschool_2014_2018 = `Less than a high school diploma, 2014-18`,
    high_school_2014_2018 = `High school diploma only, 2014-18`,
    some_college_or_associates_2014_2018 = `Some college or associate's degree, 2014-18`,
    bachelors_or_higher_2014_2018 = `Bachelor's degree or higher, 2014-18`,
    perc_less_than_highschool_2014_2018 = `Percent of adults with less than a high school diploma, 2014-18`,
    perc_high_school_2014_2018 = `Percent of adults with a high school diploma only, 2014-18`,
    perc_some_college_or_associates_2014_2018 = `Percent of adults completing some college or associate's degree, 2014-18`,
    perc_bachelors_or_higher_2014_2018 = `Percent of adults with a bachelor's degree or higher, 2014-18`
  ) %>%

  dplyr::mutate(
    state = openintro::abbr2state(state),
    county = gsub(" County", "", county, ignore.case = TRUE),
    less_than_highschool_2014_2018 = as.numeric(gsub("[\\$,]", "", less_than_highschool_2014_2018)),
    high_school_2014_2018 = as.numeric(gsub("[\\$,]", "", high_school_2014_2018)),
    some_college_or_associates_2014_2018 = as.numeric(gsub("[\\$,]", "", some_college_or_associates_2014_2018)),
    bachelors_or_higher_2014_2018 = as.numeric(gsub("[\\$,]", "", bachelors_or_higher_2014_2018))
  )

  dt <- dt[complete.cases(dt), ]

  # Remove "Parish" from "Louisiana" counties
  dt[dt$state == "Louisiana", ]$county <- gsub(" Parish", "", dt[dt$state == "Louisiana", ]$county)

  # Remove duplicate state in county section.
  dt[match(unique(dt$state), dt$state), ]$county <- ''

  return(dt)
}


#' Download all data for COVID-19 dashboard
#'
#' @param path file download path
#' @export
download_all_data <- function(path = NULL) {

  if (!curl::has_internet()) {
    stop("A stable internet connection is required to download data.")
  }

  if (is.null(path)) {
    path <- system.file("apps", "covid-19", "data", package = "covid19")
  }

  path <- normalizePath(path)

  # Download the JHU data.
  data.table::fwrite(
    x = download_jhu_covid_19(),
    file = file.path(path, "jhu_covid_19.csv")
  )

  # Download Google mobility reports
  data.table::fwrite(
    x = download_google_mr_us(),
    file = file.path(path, "google_mobility_report.csv")
  )

  # Download the USDA data
  data.table::fwrite(
    x = download_usda(),
    file = file.path(path, "usda_2014_2018.csv")
  )

}
