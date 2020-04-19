server <- function(input, output, session) {

  # Import all the required data
  # ---
  data_path <- file.path(getwd(), "data")

  confirmed_us <- file.path(data_path, "confirmed_us.csv")
  confirmed_us <- data.table::fread(confirmed_us)

  deaths_us <- file.path(data_path, "deaths_us.csv")
  deaths_us <- data.table::fread(deaths_us)

  recovered_global <- file.path(data_path, "recovered_global.csv")
  recovered_global <- data.table::fread(recovered_global)

  education_data <- file.path(data_path, "atlas-data", "People.csv")
  education_data <- data.table::fread(education_data)

  income_data <- file.path(data_path, "atlas-data", "Income.csv")
  income_data <- data.table::fread(income_data)


  # Plot Options
  plot_options <- c(
    "Scatter 2D" = "scatter",
    "Histogram" = "histogram"
  )


  # Quick Facts
  # ---
  output$total_cases <- bs4Dash::renderbs4InfoBox({
    bs4Dash::bs4InfoBox(
      title = "Total Cases Reported",
      value = 100000,
      status = "primary",
      icon = "procedures",
      width = 4
    )
  })

  output$total_recovered <- bs4Dash::renderbs4InfoBox({
    bs4Dash::bs4InfoBox(
      title = "Total Recovered",
      value = 100000,
      status = "success",
      icon = "smile",
      width = 4
    )
  })

  output$total_deaths <- bs4Dash::renderbs4InfoBox({
    bs4Dash::bs4InfoBox(
      title = "Total Deaths",
      value = 100000,
      status = "danger",
      icon = "frown",
      width = 4
    )
  })







  # Confirmed Cases
  # ---
  output$confirmed_gear <- shiny::renderUI({
    create_gear(
      shiny::uiOutput(outputId = "confirmed_gear_menu")
    )
  })




  shiny::observeEvent(input$confirmed_plot_options, {

    choice <- input$confirmed_plot_options


    if (choice == "scatter") {
      p <- create_scatter(
        data = confirmed_us,
        x = "4/5/20",
        y = "4/6/20"
      )
    }

    else if (choice == "histogram") {
      p <- create_scatter(
        data = confirmed_us,
        x = "4/2/20",
        y = "4/3/20"
      )
    }


    # Plot the selected choice
    output$confirmed_plots <- plotly::renderPlotly({ p })


  })


  output$confirmed_table <- DT::renderDataTable({
    create_table(confirmed_us)
  })


  # Death Cases
  # ---


}
