library(dplyr)


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
        autoWidth = TRUE,
        columnDefs = list(list(width = '200px', targets = "_all")),
        search = list(regex = TRUE, caseInsensitive = TRUE),
        pageLength = 15,
        lengthMenu = c(5, 15, 25, 50, 100),
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
        title = paste0("Count", " (", scale, ")"),
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
#' @param showlegend Whether or not to show the legend
#' @return mobility trend plot
#' @export
create_mobility_trends <- function(data, .state, .county = '',
                                   showlegend = TRUE) {

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
      hovermode = "compare",
      showlegend = showlegend
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
create_pie_chart <- function(data, .state, .county = '',
                             showlegend = TRUE) {

  title <- "Education - "

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
    dt$perc_less_than_highschool_2014_2018,
    dt$perc_high_school_2014_2018,
    dt$perc_some_college_or_associates_2014_2018,
    dt$perc_bachelors_or_higher_2014_2018
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
    showlegend = showlegend
  )

  return(p)

}


#' Create trend line plot based on population
#'
#' @param jhu_data Case and death data
#' @param usda_data Data to extract population
#' @param state_1 State 1
#' @param state_2 State 2
#' @param showlegend Whether or not to show the legend
#' @param scale c("linear", "log")
#' @return Trend plot scaled by population
#' @export
create_population_trends <- function(jhu_data, usda_data,
                                     state_1, state_2,
                                     showlegend = TRUE,
                                     scale = "log") {


  population_1 <- get_population(usda_data, state_1)
  population_2 <- get_population(usda_data, state_2)

  dt_1 <- jhu_data[jhu_data$state == state_1 & jhu_data$county == '', ]
  dt_2 <- jhu_data[jhu_data$state == state_2 & jhu_data$county == '', ]

  digits <- 3

  confirmed_total_1 <- ( dt_1$confirmed_total / population_1 ) * 100
  confirmed_total_1 <- signif(confirmed_total_1, digits = digits)

  confirmed_total_2 <- ( dt_2$confirmed_total / population_2 ) * 100
  confirmed_total_2 <- signif(confirmed_total_2, digits = digits)

  deaths_total_1 <- ( dt_1$deaths_total / population_1 ) * 100
  deaths_total_1 <- signif(deaths_total_1, digits = digits)

  deaths_total_2 <- ( dt_2$deaths_total / population_2 ) * 100
  deaths_total_2 <- signif(deaths_total_2, digits = digits)


  x <- as.Date(unique(jhu_data$date), format = "%m/%d/%y")
  c1 <- "#1f77b4"
  c2 <- "#ff7f0e"

  p1 <- plotly::plot_ly(
    x = x,
    y = ~ confirmed_total_1,
    type = "scatter",
    mode = "lines+markers",
    legendgroup = "state1_group",
    name = state_1,
    marker = list(color = c1),
    line = list(color = c1),
    connectgaps = TRUE
  ) %>%

  plotly::add_trace(
    y = ~ confirmed_total_2,
    legendgroup = "state2_group",
    name = state_2,
    marker = list(color = c2),
    line = list(color = c2)
  ) %>%

  plotly::layout(
    annotations = list(
      text = "Confirmed (%) - [ Cases / Population ]",
      font = list(
        size = 18,
        color = "black"
      ),
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = 1,
      showarrow = FALSE
    ),
    yaxis = list(
      title = paste("Percentage (%) - Scale:", scale)
    )
  )

  p2 <- plotly::plot_ly(
    x = x,
    y = ~ deaths_total_1,
    type = "scatter",
    mode = "lines+markers",
    legendgroup = "state1_group",
    name = state_1,
    marker = list(color = c1),
    line = list(color = c1),
    connectgaps = TRUE,
    showlegend = FALSE
  ) %>%

  plotly::add_trace(
    y = ~ deaths_total_2,
    legendgroup = "state2_group",
    name = state_2,
    marker = list(color = c2),
    line = list(color = c2),
    showlegend = FALSE
  ) %>%

  plotly::layout(
    annotations = list(
      text = "Deaths (%) - [ Cases / Population ]",
      font = list(
        size = 18,
        color = "black"
      ),
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = 1,
      showarrow = FALSE
    )
  )

  p <- plotly::subplot(
    p1, p2,
    shareX = FALSE, shareY = TRUE,
    titleX = FALSE, titleY = TRUE
  ) %>%

  plotly::layout(
    yaxis = list(
      type = scale
    ),
    hovermode = "compare",
    showlegend = showlegend,
    legend = list(
      x = 1,
      y = 1,
      traceorder = "normal",
      bgcolor = "LightSteelBlue",
      bordercolor = "Black",
      borderwidth = 2
    )
  )

  return(p)

}



#' Get median income of area
#'
#' @param data Data table object
#' @param .state State to filter data
#' @param .county County to filter data
#' @param comma_sep Whether to return string version with comma sep.
#' @return Median income
#' @export
get_median_income <- function(data, .state, .county = '', comma_sep = FALSE) {
  dt <- data[data$state == .state & data$county == .county, ]
  med_income <- dt$median_household_income_2018

  if (comma_sep) {
    med_income <- format(med_income, big.mark = ",", scientific = FALSE)
  }

  return(med_income)
}


#' Get 2018 populuation of area
#'
#' @param data Data table object
#' @param .state State to filter data
#' @param .county County to filter data
#' @param comma_sep Whether to return string version with comma sep.
#' @return 2018 populutation
#' @export
get_population <- function(data, .state, .county = '', comma_sep = FALSE) {
  dt <- data[data$state == .state & data$county == .county, ]
  population <- dt$population_est_2018

  if (comma_sep) {
    population <- format(population, big.mark = ",", scientific = FALSE)
  }

  return(population)
}
