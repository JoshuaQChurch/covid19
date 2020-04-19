
create_stacked_bar <- function(data, .state, .county = NULL,
                               .type = "total", scale = "linear",
                               barmode = c("stacked", "group")) {

  title <- "Confirmed vs. Deaths"

  if (.type == "total") { title <- paste(title, "-", "Total<br>") }
  else { title <- paste(title, "-", "Per Day<br>") }

  barmode <- barmode[1]

  if (is.null(.county)) {
    .county <- ''
    title <- paste0(title, .state)
  }

  else {
    title <- paste0(title, .county, ", ", .state)
  }

  dt <- data[data$state == .state & data$county == .county, ]

  confirmed <- dt[[paste0("confirmed_", .type)]]
  deaths <- dt[[paste0("deaths_", .type)]]

  p <- plotly::plot_ly(
    x = as.Date(dt$date, format = "%m/%d/%y"),
    y = ~ confirmed,
    type = "bar",
    name = "Confirmed"
  ) %>%

  plotly::add_trace(
    y = ~ deaths,
    name = "Deaths"
  ) %>%

  plotly::layout(
    title = title,
    xaxis = list(title = "Date"),
    yaxis = list(
      title = "Count",
      type = scale
    ),
    barmode = barmode
  )

  print(p)

}



create_mobility_trends <- function(data, .state, .county = NULL) {

  if (is.null(.county)) {
    .county <- ''
    title <- paste0("Google Mobility Reports for ", .state)
  }

  else {
    title <- paste0("Google Mobility Reports for ", .county, ", ", .state)
  }

  dt <- data[data$state == .state & data$county == .county, ]

  p <- plotly::plot_ly(
    x = as.Date(dt$date),
    y = dt$retail_and_recreation,
    type = "scatter",
    mode = "lines+markers",
    name = "Retail and Recreation",
    connectgaps = TRUE) %>%

    plotly::add_trace(
      y = dt$grocery_and_pharmacy,
      name = "Grocery and Pharmacy"
    ) %>%

    plotly::add_trace(
      y = dt$parks,
      name = "Parks"
    ) %>%

    plotly::add_trace(
      y = dt$transit_stations,
      name = "Transit Stations"
    ) %>%

    plotly::add_trace(
      y = dt$workplaces,
      name = "Workplaces"
    ) %>%

    plotly::add_trace(
      y = dt$residential,
      name = "Residential"
    ) %>%

    plotly::layout(
      title = title,
      xaxis = list(title = "Date"),
      yaxis = list(title = "Percentage from Baseline (%)")
    )

  return(p)

}

dt <- data.table::fread("./inst/extdata/google_mobility_report.csv")
print(create_mobility_trends(dt, "Mississippi", "Warren"))


create_pie_chart <- function(data, .state, .county = NULL) {

  if (is.null(.county)) {
    .county <- .state
    title <- paste0("Education for ", .state)
  }

  else {
    title <- paste0("Education for ", .county, ", ", .state)
  }

  dt <- data[data$state == .state & data$county == .county, ]

  labels <- c(
    "Less than a high school diploma",
    "High school diploma only",
    "Some college or associate's degree",
    "Bachelor's degree or higher"
  )

  values <- c(
    dt$less_than_highschool_2014_2018,
    dt$high_school_2014_2018,
    dt$some_college_or_associates_2014_2018,
    dt$bachelors_or_higher_2014_2018
  )

  p <- plotly::plot_ly(
    type = "pie",
    labels = labels,
    values = values,
    textinfo = "label+percent"
  ) %>%

    plotly::layout(
      title = title,
      showlegend = FALSE
    )

  return(p)

}


get_median_income <- function(data, .state, .county = NULL) {

  if (is.null(.county)) { .county <- .state }
  dt <- data[data$state == .state & data$county == .county, ]
  return(dt$median_household_income_2018)
}




