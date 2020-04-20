#' Render a data table
#'
#' @param data Data table object
#' @return DT::datatable()
create_table <- function(data) {
  return(
    DT::datatable(
      data = data,
      filter = list(position = "top", clear = FALSE),
      options = list(
        search = list(regex = TRUE, caseInsensitive = TRUE),
        pageLength = 10,
        lengthMenu = c(5, 10, 15, 20),
        scrollX = TRUE
      )
    )
  )
}


#' Create a stacked bar chart
#'
#' @param data Data table
#' @param .state Selected state
#' @param .county Selected county
#' @param .type c("total", "per_day")
#' @param scale c("linear", "log")
#' @param barmode c("stacked", "group")
#' @return stacked bar chart
create_stacked_bar <- function(data, .state, .county = '',
                               .type = c("total", "per_day"),
                               scale = c("linear", "log"),
                               barmode = c("stacked", "group")) {

  barmode <- barmode[1]
  scale <- scale[1]
  .type <- .type[1]

  title <- "Confirmed vs. Deaths"

  if (.type == "total") { title <- paste(title, "-", "Total<br>") }
  else { title <- paste(title, "-", "Per Day<br>") }

  if (.county == '') {
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
      barmode = barmode,
      hovermode = "compare"
    )

  return(p)

}


#' Create Google mobility reports
#'
#' @param data Data table object
#' @param .state State to filter data
#' @param .county County to filter data
#' @return mobility trend plot
#' @export
create_mobility_trends <- function(data, .state, .county = '') {

  title <- "Google Mobility Reports<br>"

  if (.county == '') {
    title <- paste0(title, .state)
  }

  else {
    title <- paste0(title, .county, ", ", .state)
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
      yaxis = list(title = "Percentage from Baseline (%)"),
      hovermode = "compare"
    )

  return(p)

}


#' Create Education pie chart
#'
#' @param data Data table object
#' @param .state State to filter data
#' @param .county County to filter data
#' @return Pie chart
#' @export
create_pie_chart <- function(data, .state, .county = '') {

  title <- "Education<br>"

  if (.county == '') {
    title <- paste0(title, .state)
  }

  else {
    title <- paste0(title, .county, ", ", .state)
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
    marker = list(
      colors = c(
        "#5DA5DA",
        "#60BD68",
        "#FAA43A",
        "#F15854"
      )
    ),
    textinfo = "percent" # label+percent
  ) %>%

  plotly::layout(
    title = title,
    xaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    ),
    yaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    ),
    showlegend = FALSE
  )

  return(p)

}


#' Get median income of area
#'
#' @param data Data table object
#' @param .state State to filter data
#' @param .county County to filter data
#' @return Median income
#' @export
get_median_income <- function(data, .state, .county = '') {
  dt <- data[data$state == .state & data$county == .county, ]
  return(dt$median_household_income_2018)
}
