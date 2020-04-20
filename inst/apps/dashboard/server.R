#' Server code for the COVID-19 Shiny dashboard
#'
#' @export
server <- function(input, output, session) {

  # Import all the required data
  # ---
  data_path <- file.path(getwd(), "..", "..", "extdata")
  data_path <- normalizePath(data_path)

  jhu_data <- file.path(data_path, "jhu_covid_19.csv")
  jhu_data <- data.table::fread(jhu_data)

  edu_income_data <- file.path(data_path, "education_income.csv")
  edu_income_data <- data.table::fread(edu_income_data)

  mobility_data <- file.path(data_path, "google_mobility_report.csv")
  mobility_data <- data.table::fread(mobility_data)

  # State / County
  # ------------------------------------

  state_selection <<- NULL

  # Create plots
  # ---
  output$select_state_county <- shiny::renderUI({
    shiny::fluidRow(

      shiny::column(width = 1),
      shiny::column(
        width = 4,
        shiny::selectInput(
          inputId = "select_state_single",
          label = "Select State",
          choices = sort(unique(jhu_data$state)),
          selected = "Mississippi",
          selectize = TRUE,
          width = "100%"
        )
      ),
      shiny::column(width = 2),
      shiny::column(
        width = 4,

        shiny::selectInput(
          inputId = "select_county",
          label = "Select County",
          choices = c(),
          selected = NULL,
          selectize = TRUE,
          width = "100%"
        )
      ),

      shiny::column(width = 1)
    )
  })

  # Update the counties based on the state selection
  # ---
  shiny::observeEvent(input$select_state_single, {

    counties <- jhu_data[jhu_data$state == input$select_state_single, ]$county
    counties <- sort(unique(counties))

    shiny::updateSelectInput(
      session,
      inputId = "select_county",
      choices = counties
    )
  })


  # Plot the selected values
  # ---
  shiny::observe({

    .state <- input$select_state_single
    .county <- input$select_county
    if (is.null(.state)) { return() }

    if (is.null(state_selection) || .state != state_selection) {
      .county <- ''
      state_selection <<- .state
    }

    .county <- sub("\\s+$", "", .county)

    output$state_county_plot_1 <- plotly::renderPlotly({
      create_stacked_bar(
        jhu_data, .state, .county,
        .type = "per_day",
        scale = input$state_county_plot_1_scale,
        barmode = input$state_county_plot_1_barmode
      )
    })

    output$state_county_plot_2 <- plotly::renderPlotly({
      create_stacked_bar(
        jhu_data, .state, .county,
        .type = "total",
        scale = input$state_county_plot_2_scale,
        barmode = input$state_county_plot_2_barmode
      )
    })

    output$state_county_income <- bs4Dash::renderbs4InfoBox({
      bs4Dash::bs4InfoBox(
        width = 12,
        title = "Median Income",
        value = get_median_income(edu_income_data, .state, .county),
        icon = "dollar-sign",
        status = "success"
      )
    })

    output$state_county_plot_3 <- plotly::renderPlotly({
      create_pie_chart(edu_income_data, .state, .county)
    })

    output$state_county_plot_4 <- plotly::renderPlotly({
      create_mobility_trends(mobility_data, .state, .county)
    })

  })


  # State / State
  # ------------------------------------

  output$select_state_state <- shiny::renderUI({
    shiny::fluidRow(

      shiny::column(width = 1),
      shiny::column(
        width = 4,
        shiny::selectInput(
          inputId = "select_state_1",
          label = "Select State 1",
          choices = sort(unique(jhu_data$state)),
          selected = "Mississippi",
          selectize = TRUE,
          width = "100%"
        )
      ),
      shiny::column(width = 2),
      shiny::column(
        width = 4,

        shiny::selectInput(
          inputId = "select_state_2",
          label = "Select State 2",
          choices = sort(unique(jhu_data$state)),
          selected = "New York",
          selectize = TRUE,
          width = "100%"
        )
      ),

      shiny::column(width = 1)
    )
  })

  # Plot the selected values
  # ---
  shiny::observe({

    state_1 <- input$select_state_1
    state_2 <- input$select_state_2

    if (is.null(state_1)) { return() }

    output$state_state_plot_1 <- plotly::renderPlotly({
      create_stacked_bar(
        jhu_data, .state = state_1, .county = '',
        .type = input$state_state_plot_1_type,
        scale = input$state_state_plot_1_scale,
        barmode = input$state_state_plot_1_barmode
      )
    })

    output$state_state_plot_2 <- plotly::renderPlotly({
      create_stacked_bar(
        jhu_data, .state = state_2, .county = '',
        .type = input$state_state_plot_2_type,
        scale = input$state_state_plot_2_scale,
        barmode = input$state_state_plot_2_barmode
      )
    })

    output$state_state_plot_3 <- plotly::renderPlotly({
      create_mobility_trends(mobility_data, .state = state_1, .county = '')
    })

    output$state_state_plot_4 <- plotly::renderPlotly({
      create_mobility_trends(mobility_data, .state = state_2, .county = '')
    })

    output$state_state_plot_5 <- plotly::renderPlotly({
      create_pie_chart(edu_income_data, .state = state_1, .county = '')
    })

    output$state_state_plot_6 <- plotly::renderPlotly({
      create_pie_chart(edu_income_data, .state = state_2, .county = '')
    })


    output$state_state_income_1 <- bs4Dash::renderbs4InfoBox({
      bs4Dash::bs4InfoBox(
        width = 12,
        title = "Median Income",
        value = get_median_income(edu_income_data, .state = state_1, .county = ''),
        icon = "dollar-sign",
        status = "success"
      )
    })

    output$state_state_income_2 <- bs4Dash::renderbs4InfoBox({
      bs4Dash::bs4InfoBox(
        width = 12,
        title = "Median Income",
        value = get_median_income(edu_income_data, .state = state_2, .county = ''),
        icon = "dollar-sign",
        status = "success"
      )
    })

  })

  # -----------------------------------------------
  # Render Data Table
  # ---
  output$jhu_data <- DT::renderDataTable({
    create_table(jhu_data)
  })

  output$mobility_data <- DT::renderDataTable({
    create_table(mobility_data)
  })

  output$edu_income_data <- DT::renderDataTable({
    create_table(edu_income_data)
  })


}
