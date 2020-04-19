#' Create a gear selection object to allow user to adjust parameters.
#'
#' @inheritDotParams shinyWidgets::dropdownButton
#' @return Adjustable gear for parameter selection
#' @export
create_gear <- function(...) {

  return(
    shinyWidgets::dropdownButton(

      shiny::tags$h3("Parameter Selection:"),
      shiny::tags$hr(),
      ...,
      circle = FALSE,
      status = "danger",
      icon = shiny::icon("gear"),
      width = "300px",
      size = "sm",
      tooltip = shinyWidgets::tooltipOptions(
        placement = "right",
        title = "Click to change parameters."
      )
    )
  )

}

gear_scatter_menu <- function(case_type, params) {

  return(
    shiny::tags$div(
      shiny::selectInput(
        inputId = paste0(case_type, "_scatter_x"),
        label = "Select X Parameter",
        choices = params,
        selected = params[1],
        multiple = FALSE,
        selectize = TRUE,
        width = "100%"
      )
    )
  )

}






create_table <- function(data) {

  return(
    DT::datatable(
      data = data,
      options = list(
        pageLength = 15,
        lengthMenu = c(5, 10, 15, 20),
        scrollX = TRUE
      )
    )
  )

}


create_scatter <- function(data, x, y, name = NULL) {


    return(
      plotly::plot_ly(
        data = data,
        type = "scatter",
        name = "name",
        x = data[[x]],
        y = data[[y]],
        colors = "Viridis",

        xaxis = list(
          title = "X"
        )
      )
    )

}

create_line <- function() {

}

create_stacked_area <- function() {

}

create_stacked_bar <- function() {

  Animals <- c("giraffes", "orangutans", "monkeys")
  SF_Zoo <- c(20, 14, 23)
  LA_Zoo <- c(12, 18, 29)
  data <- data.frame(Animals, SF_Zoo, LA_Zoo)

  fig <- plotly::plot_ly(data, x = ~Animals, y = ~SF_Zoo, type = 'bar', name = 'SF Zoo')
  fig <- fig %>% plotly::add_trace(y = ~LA_Zoo, name = 'LA Zoo')
  fig <- fig %>% plotly::layout(yaxis = list(title = 'Count'), barmode = 'stack')
  print(fig)

}

create_map <- function() {

}

create_mobility <- function() {

}


