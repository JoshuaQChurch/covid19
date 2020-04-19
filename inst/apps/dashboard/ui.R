#' User interface code for the COVID-19 Shiny dashboard
#'
#' @export
ui <- function() {

  bs4Dash::bs4DashPage(

    title = "COVID-19 Dashboard",
    old_school = FALSE,
    sidebar_mini = TRUE,
    sidebar_collapsed = TRUE,
    controlbar_collapsed = TRUE,
    controlbar_overlay = TRUE,
    enable_preloader = FALSE,
    loading_duration = 2,
    loading_background = "#1E90FF",

    # Navigation Bar
    navbar = bs4Dash::bs4DashNavbar(
      skin = "dark",
      status = NULL,
      border = TRUE,
      sidebarIcon = "bars",
      compact = FALSE,
      controlbarIcon = "th",
      fixed = FALSE
    ),

    # Sidebar
    sidebar = bs4Dash::bs4DashSidebar(
      inputId = NULL,
      disable = FALSE,
      title = "COVID-19",
      skin = "dark",
      status = "primary",
      brandColor = "gray-light",
      url = NULL, # Add the domain name later
      src = file.path("images", "virus.png"),
      elevation = 4,
      opacity = 0.8,
      expand_on_hover = TRUE,
      fixed = TRUE,

      # Sidebar Menu Options
      bs4Dash::bs4SidebarMenu(
        id = NULL,
        flat = FALSE,
        compact = FALSE,
        child_indent = TRUE,

        # Tab - Overview
        bs4Dash::bs4SidebarMenuItem(
          text = "Overview",
          tabName = "tab-overview",
          icon = "home",
          startExpanded = FALSE
        ),

        # Tab - Dashboard
        bs4Dash::bs4SidebarMenuItem(
          text = "COVID-19 Cases",
          tabName = "tab-cases",
          icon = "procedures",
          startExpanded = FALSE
        )
      )
    ), # bs4Dash::bs4DashSidebar()

    # Body
    body = bs4Dash::bs4DashBody(
      bs4Dash::bs4TabItems(

        # Overview Tab
        bs4Dash::bs4TabItem(
          tabName = "tab-overview",

          shiny::tags$h1("Overview"),

          # TODO:
          # - Talk about the purpose
          # - Acredit data sources

        ),

        # Tab - Recovered
        bs4Dash::bs4TabItem(
          tabName = "tab-cases",

          shiny::tags$div(
            shiny::tags$h1("COVID-19 - United States - CSE 8990")
          ),

          shiny::fluidRow(
            shiny::column(
              width = 8,

              shiny::dateRangeInput(
                inputId = "data_range_filter",
                label = "Filter dates",
                start  = "2001-01-01",
                end    = "2010-12-31",
                min    = "2001-01-01",
                max    = "2012-12-21",
                format = "mm/dd/yy",
                separator = " to "
              )
            ),

            shiny::column(
              width = 4,

              shiny::tags$div(



                bs4Dash::bs4Badge(
                  "Quick Facts",
                  status = "dark"
                ),


                shiny::tags$h1("Quick Facts"),
                shiny::tags$hr(),

                shiny::fluidRow(

                  bs4Dash::infoBoxOutput(
                    outputId = "total_cases",
                    width = 4
                  ),

                  bs4Dash::infoBoxOutput(
                    outputId = "total_recovered",
                    width = 4
                  ),

                  bs4Dash::infoBoxOutput(
                    outputId = "total_deaths",
                    width = 4
                  )
                )
              ),

              DT::dataTableOutput(outputId = "confirmed_table")
            )
          )

        )
      ) # bs4TabItems()
    ),

    # Controlbar
    controlbar = bs4Dash::bs4DashControlbar(
      title = "Download Current Data",

      shiny::tags$div("Content")

      # TODO: Complete this section

    ),

    # Footer
    footer = NULL

  )
}
