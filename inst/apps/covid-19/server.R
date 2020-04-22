#' Server code for the COVID-19 Shiny dashboard
#'
#' @export
server <- function(input, output, session) {

  # Import all the required data
  # ---
  data_path <- file.path(getwd(), "data")
  data_path <- normalizePath(data_path)

  jhu_data <- file.path(data_path, "jhu_covid_19.csv")
  jhu_classes <- list(
    factor = c(
      "fips",
      "county",
      "state",
      "country",
      "latitude",
      "longtitude",
      "date"
    ),
    numeric = c(
      "confirmed_total",
      "confirmed_per_day",
      "deaths_total",
      "deaths_per_day"
    )
  )
  jhu_data <- data.table::fread(jhu_data, colClasses = jhu_classes)

  usda_data <- file.path(data_path, "usda_2014_2018.csv")
  usda_classes <- list(
    factor = c(
      "fips",
      "state",
      "county"
    ),
    numeric = c(
      "less_than_highschool_2014_2018",
      "high_school_2014_2018",
      "some_college_or_associates_2014_2018",
      "bachelors_or_higher_2014_2018",
      "perc_less_than_highschool_2014_2018",
      "perc_high_school_2014_2018",
      "perc_some_college_or_associates_2014_2018",
      "perc_bachelors_or_higher_2014_2018",
      "median_household_income_2018"
    )
  )
  usda_data <- data.table::fread(usda_data, colClasses = usda_classes)


  mobility_data <- file.path(data_path, "google_mobility_report.csv")
  mobility_classes <- list(
    factor = c(
      "country_region_code",
      "country",
      "state",
      "county",
      "date"
    ),
    numeric = c(
      "retail_and_recreation",
      "grocery_and_pharmacy",
      "parks",
      "transit_stations",
      "workplaces",
      "residential"
    )
  )
  mobility_data <- data.table::fread(mobility_data, colClasses = mobility_classes)

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

    output$state_county_population <- bs4Dash::renderbs4InfoBox({
      bs4Dash::bs4InfoBox(
        width = 12,
        title = "Population (2018)",
        value = get_population(usda_data, .state, .county, comma_sep = TRUE),
        icon = "users",
        status = "info"
      )
    })

    output$state_county_income <- bs4Dash::renderbs4InfoBox({
      bs4Dash::bs4InfoBox(
        width = 12,
        title = "Median Income (2014-2018)",
        value = get_median_income(usda_data, .state, .county, comma_sep = TRUE),
        icon = "dollar-sign",
        status = "success"
      )
    })

    output$state_county_plot_3 <- plotly::renderPlotly({
      create_pie_chart(usda_data, .state, .county)
    })

    output$state_county_plot_4 <- plotly::renderPlotly({
      create_mobility_trends(
        mobility_data, .state, .county,
        showlegend = input$state_county_plot_4_showlegend
      )
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

    if (is.null(input$select_state_1)) { return() }

    output$state_state_plot_1 <- plotly::renderPlotly({
      create_stacked_bar(
        jhu_data, .state = input$select_state_1, .county = '',
        .type = input$state_state_plot_1_type,
        scale = input$state_state_plot_1_scale,
        barmode = input$state_state_plot_1_barmode
      )
    })

    output$state_state_plot_2 <- plotly::renderPlotly({
      create_stacked_bar(
        jhu_data, .state = input$select_state_2, .county = '',
        .type = input$state_state_plot_2_type,
        scale = input$state_state_plot_2_scale,
        barmode = input$state_state_plot_2_barmode
      )
    })

    output$state_state_plot_3 <- plotly::renderPlotly({
      create_mobility_trends(
        mobility_data, .state = input$select_state_1, .county = '',
        showlegend = input$state_state_plot_3_showlegend
      )
    })

    output$state_state_plot_4 <- plotly::renderPlotly({
      create_mobility_trends(
        mobility_data, .state = input$select_state_2, .county = '',
        showlegend = input$state_state_plot_4_showlegend
      )
    })

    output$state_state_plot_5 <- plotly::renderPlotly({
      create_pie_chart(usda_data, .state = input$select_state_1, .county = '')
    })

    output$state_state_plot_6 <- plotly::renderPlotly({
      create_pie_chart(usda_data, .state = input$select_state_2, .county = '')
    })

    output$state_state_plot_7 <- plotly::renderPlotly({
      create_population_trends(
        jhu_data = jhu_data,
        usda_data = usda_data,
        state_1 = input$select_state_1,
        state_2 = input$select_state_2,
        scale = input$state_state_plot_7_scale,
        showlegend = input$state_state_plot_7_showlegend
      )
    })

    output$state_state_population_1 <- bs4Dash::renderbs4InfoBox({
      bs4Dash::bs4InfoBox(
        width = 12,
        title = "Population (2018)",
        value = get_population(usda_data, .state = input$select_state_1, .county = '', comma_sep = TRUE),
        icon = "users",
        status = "info"
      )
    })

    output$state_state_income_1 <- bs4Dash::renderbs4InfoBox({
      bs4Dash::bs4InfoBox(
        width = 12,
        title = "Median Income",
        value = get_median_income(usda_data, .state = input$select_state_1, .county = '', comma_sep = TRUE),
        icon = "dollar-sign",
        status = "success"
      )
    })

    output$state_state_population_2 <- bs4Dash::renderbs4InfoBox({
      bs4Dash::bs4InfoBox(
        width = 12,
        title = "Population (2018)",
        value = get_population(usda_data, .state = input$select_state_2, .county = '', comma_sep = TRUE),
        icon = "users",
        status = "info"
      )
    })

    output$state_state_income_2 <- bs4Dash::renderbs4InfoBox({
      bs4Dash::bs4InfoBox(
        width = 12,
        title = "Median Income",
        value = get_median_income(usda_data, .state = input$select_state_2, .county = '', comma_sep = TRUE),
        icon = "dollar-sign",
        status = "success"
      )
    })

  })

  # Render Data Table
  # ---
  output$jhu_data <- DT::renderDataTable({
    create_table(jhu_data)
  })

  output$mobility_data <- DT::renderDataTable({
    create_table(mobility_data)
  })

  output$usda_data <- DT::renderDataTable({
    create_table(usda_data)
  })


}
