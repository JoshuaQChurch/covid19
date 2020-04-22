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
      url = "https://www.cdc.gov/coronavirus/2019-ncov/index.html",
      src = file.path("images", "covid19.jpeg"),
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

        # Tab - Dashboard
        bs4Dash::bs4SidebarMenuItem(
          text = "Dashboard",
          tabName = "tab-dashboard",
          icon = "tachometer-alt",
          startExpanded = TRUE,

          bs4Dash::bs4SidebarMenuSubItem(
            text = "State & County",
            tabName = "tab-dashboard-state-county",
            icon = "map-marker-alt"
          ),

          bs4Dash::bs4SidebarMenuSubItem(
            text = "State vs. State",
            tabName = "tab-dashboard-state-state",
            icon = "map"
          )
        ),

        # Tab - Data Table
        bs4Dash::bs4SidebarMenuItem(
          text = "Data Table",
          tabName = "tab-data-table",
          icon = "table"
        ),

        # Tab - Data Sources
        bs4Dash::bs4SidebarMenuItem(
          text = "Data Sources",
          tabName = "tab-data-sources",
          icon = "database"
        )
      )
    ), # bs4Dash::bs4DashSidebar()

    # Body
    body = bs4Dash::bs4DashBody(

      bs4Dash::bs4TabItems(

        # Tab - State & County
        bs4Dash::bs4TabItem(
          tabName = "tab-dashboard-state-county",

          shinycssloaders::withSpinner(
            shiny::uiOutput("select_state_county")
          ),

          shiny::fluidRow(
            bs4Dash::bs4Sortable(
              width = 6,

              bs4Dash::bs4Card(
                title = "Confirmed vs. Deaths - Per Day",
                width = 12,
                status = NULL,
                gradientColor = NULL,
                collapsible = TRUE,
                closable = FALSE,
                maximizable = TRUE,
                enable_sidebar = TRUE,
                sidebar_content = shiny::tags$div(
                  shiny::radioButtons(
                    inputId = "state_county_plot_1_scale",
                    choices = c(
                      "Linear" = "linear",
                      "Logarithmic" = "log"
                    ),
                    selected = "log",
                    label = "Select Scale:"
                  ),

                  shiny::tags$br(),

                  shiny::radioButtons(
                    inputId = "state_county_plot_1_barmode",
                    choices = c(
                      "Stack" = "stack",
                      "Group" = "group"
                    ),
                    selected = "group",
                    label = "Select Barmode:"
                  )
                ),
                shiny::tags$div(
                  style = "padding: 20px 0px 20px 10px;",
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput("state_county_plot_1")
                  )
                )
              )
            ),

            bs4Dash::bs4Sortable(
              width = 6,
              bs4Dash::bs4Card(
                title = "Confirmed vs. Deaths - Total",
                width = 12,
                status = NULL,
                gradientColor = NULL,
                collapsible = TRUE,
                closable = FALSE,
                maximizable = TRUE,
                enable_sidebar = TRUE,
                sidebar_content = shiny::tags$div(
                  shiny::radioButtons(
                    inputId = "state_county_plot_2_scale",
                    choices = c(
                      "Linear" = "linear",
                      "Logarithmic" = "log"
                    ),
                    selected = "log",
                    label = "Select Scale:"
                  ),

                  shiny::tags$br(),

                  shiny::radioButtons(
                    inputId = "state_county_plot_2_barmode",
                    choices = c(
                      "Stack" = "stack",
                      "Group" = "group"
                    ),
                    selected = "group",
                    label = "Select Barmode:"
                  )
                ),
                shiny::tags$div(
                  style = "padding: 20px 0px 20px 10px;",
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput("state_county_plot_2")
                  )
                )
              )
            )
          ),

          shiny::fluidRow(
            bs4Dash::bs4Sortable(
              width = 6,
              bs4Dash::bs4Card(
                title = "Population, Education, and Median Income",
                width = 12,
                status = NULL,
                gradientColor = NULL,
                collapsible = TRUE,
                closable = FALSE,
                maximizable = TRUE,
                shiny::tags$div(
                  style = "padding: 20px 0px 20px 10px;",
                  shiny::fluidRow(
                    shiny::column(
                      width = 4,

                      bs4Dash::infoBoxOutput("state_county_population", width = 12),
                      bs4Dash::infoBoxOutput("state_county_income", width = 12)
                    ),
                    shiny::column(
                      width = 7,
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("state_county_plot_3")
                      )
                    ),
                    shiny::column(width = 1)
                  )
                )
              )
            ),

            bs4Dash::bs4Sortable(
              width = 6,
              bs4Dash::bs4Card(
                title = "Google Mobility Trends",
                width = 12,
                status = NULL,
                gradientColor = NULL,
                collapsible = TRUE,
                closable = FALSE,
                maximizable = TRUE,
                enable_sidebar = TRUE,
                sidebar_content = shiny::tags$div(
                  shiny::checkboxInput(
                    inputId = "state_county_plot_4_showlegend",
                    label = "Show Legend",
                    value = TRUE,
                    width = "100%"
                  )
                ),
                shiny::tags$div(
                  style = "padding: 20px 0px 20px 10px;",
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput("state_county_plot_4")
                  )
                )
              )
            )
          )
        ), # bs4TabItem()


        # --------------------------------------------------


        # Tab - State & State Comparision
        bs4Dash::bs4TabItem(
          tabName = "tab-dashboard-state-state",

          shinycssloaders::withSpinner(
            shiny::uiOutput("select_state_state")
          ),

          shiny::fluidRow(
            bs4Dash::bs4Sortable(
              width = 6,

              bs4Dash::bs4Card(
                title = "Confirmed vs. Deaths",
                width = 12,
                status = NULL,
                gradientColor = NULL,
                collapsible = TRUE,
                closable = FALSE,
                maximizable = TRUE,
                enable_sidebar = TRUE,
                sidebar_content = shiny::tags$div(

                  shiny::radioButtons(
                    inputId = "state_state_plot_1_type",
                    choices = c(
                      "Total" = "total",
                      "Per Day" = "per_day"
                    ),
                    selected = "total",
                    label = "Select Aggregration:"
                  ),

                  shiny::radioButtons(
                    inputId = "state_state_plot_1_scale",
                    choices = c(
                      "Linear" = "linear",
                      "Logarithmic" = "log"
                    ),
                    selected = "log",
                    label = "Select Scale:"
                  ),

                  shiny::tags$br(),

                  shiny::radioButtons(
                    inputId = "state_state_plot_1_barmode",
                    choices = c(
                      "Stack" = "stack",
                      "Group" = "group"
                    ),
                    selected = "group",
                    label = "Select Barmode:"
                  )
                ),
                shiny::tags$div(
                  style = "padding: 20px 0px 20px 10px;",
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput("state_state_plot_1")
                  )
                )
              )
            ),

            bs4Dash::bs4Sortable(
              width = 6,

              bs4Dash::bs4Card(
                title = "Confirmed vs. Deaths",
                width = 12,
                status = NULL,
                gradientColor = NULL,
                collapsible = TRUE,
                closable = FALSE,
                maximizable = TRUE,
                enable_sidebar = TRUE,
                sidebar_content = shiny::tags$div(

                  shiny::radioButtons(
                    inputId = "state_state_plot_2_type",
                    choices = c(
                      "Total" = "total",
                      "Per Day" = "per_day"
                    ),
                    selected = "total",
                    label = "Select Aggregration:"
                  ),

                  shiny::radioButtons(
                    inputId = "state_state_plot_2_scale",
                    choices = c(
                      "Linear" = "linear",
                      "Logarithmic" = "log"
                    ),
                    selected = "log",
                    label = "Select Scale:"
                  ),

                  shiny::tags$br(),

                  shiny::radioButtons(
                    inputId = "state_state_plot_2_barmode",
                    choices = c(
                      "Stack" = "stack",
                      "Group" = "group"
                    ),
                    selected = "group",
                    label = "Select Barmode:"
                  )
                ),
                shiny::tags$div(
                  style = "padding: 20px 0px 20px 10px;",
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput("state_state_plot_2")
                  )
                )
              )
            )
          ), # fluidRow()

          shiny::fluidRow(
            bs4Dash::bs4Sortable(
              width = 12,

              bs4Dash::bs4Card(
                title = "Confirmed vs. Deaths - Total - Normalized by Population",
                width = 12,
                status = NULL,
                gradientColor = NULL,
                collapsible = TRUE,
                closable = FALSE,
                maximizable = TRUE,
                enable_sidebar = TRUE,
                sidebar_content = shiny::tags$div(

                  shiny::radioButtons(
                    inputId = "state_state_plot_7_scale",
                    choices = c(
                      "Linear" = "linear",
                      "Logarithmic" = "log"
                    ),
                    selected = "log",
                    label = "Select Scale:"
                  ),

                  shiny::tags$br(),

                  shiny::checkboxInput(
                    inputId = "state_state_plot_7_showlegend",
                    label = "Show Legend",
                    value = TRUE,
                    width = "100%"
                  )
                ),
                shiny::tags$div(
                  style = "padding: 20px 0px 20px 10px;",
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput("state_state_plot_7")
                  )
                )
              )
            )
          ), # fluidRow()

          shiny::fluidRow(
            bs4Dash::bs4Sortable(
              width = 6,
              bs4Dash::bs4Card(
                title = "Google Mobility Trends",
                width = 12,
                status = NULL,
                gradientColor = NULL,
                collapsible = TRUE,
                closable = FALSE,
                maximizable = TRUE,
                enable_sidebar = TRUE,
                sidebar_content = shiny::tags$div(
                  shiny::checkboxInput(
                    inputId = "state_state_plot_3_showlegend",
                    label = "Show Legend",
                    value = TRUE,
                    width = "100%"
                  )
                ),
                shiny::tags$div(
                  style = "padding: 20px 0px 20px 10px;",
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput("state_state_plot_3")
                  )
                )
              )
            ),

            bs4Dash::bs4Sortable(
              width = 6,
              bs4Dash::bs4Card(
                title = "Google Mobility Trends",
                width = 12,
                status = NULL,
                gradientColor = NULL,
                collapsible = TRUE,
                closable = FALSE,
                maximizable = TRUE,
                enable_sidebar = TRUE,
                sidebar_content = shiny::tags$div(
                  shiny::checkboxInput(
                    inputId = "state_state_plot_4_showlegend",
                    label = "Show Legend",
                    value = TRUE,
                    width = "100%"
                  )
                ),
                shiny::tags$div(
                  style = "padding: 20px 0px 20px 10px;",
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput("state_state_plot_4")
                  )
                )
              )
            )
          ), # fluidRow()

          shiny::fluidRow(
            bs4Dash::bs4Sortable(
              width = 6,
              bs4Dash::bs4Card(
                title = "Population, Education, and Median Income",
                width = 12,
                status = NULL,
                gradientColor = NULL,
                collapsible = TRUE,
                closable = FALSE,
                maximizable = TRUE,
                shiny::tags$div(
                  style = "padding: 20px 0px 20px 10px;",
                  shiny::fluidRow(
                    shiny::column(
                      width = 4,
                      bs4Dash::infoBoxOutput("state_state_population_1", width = 12),
                      bs4Dash::infoBoxOutput("state_state_income_1", width = 12)
                    ),
                    shiny::column(
                      width = 7,
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("state_state_plot_5")
                      )
                    ),
                    shiny::column(width = 1)
                  )
                )
              )
            ),

            bs4Dash::bs4Sortable(
              width = 6,
              bs4Dash::bs4Card(
                title = "Population, Education, and Median Income",
                width = 12,
                status = NULL,
                gradientColor = NULL,
                collapsible = TRUE,
                closable = FALSE,
                maximizable = TRUE,
                shiny::tags$div(
                  style = "padding: 20px 0px 20px 10px;",
                  shiny::fluidRow(
                    shiny::column(
                      width = 4,
                      bs4Dash::infoBoxOutput("state_state_population_2", width = 12),
                      bs4Dash::infoBoxOutput("state_state_income_2", width = 12)
                    ),
                    shiny::column(
                      width = 7,
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput("state_state_plot_6")
                      )
                    ),
                    shiny::column(width = 1)
                  )
                )
              )
            )
          ) # fluidRow()
        ), # bs4TabItem()

        # --------------------------------------------------

        # Tab - Data Table
        bs4Dash::bs4TabItem(
          tabName = "tab-data-table",

          bs4Dash::bs4TabSetPanel(

            id = NULL,
            side = "left",
            status = NULL,
            tabStatus = NULL,
            vertical = FALSE,
            type = NULL,

            bs4Dash::bs4TabPanel(
              tabName = "JHU",

              shiny::tags$div(
                style = "text-align: center; padding: 10px;",
                shiny::tags$h1(
                  "Johns Hopkins University - COVID-19 Data",
                  style = "padding-bottom: 25px;"
                ),
                DT::dataTableOutput("jhu_data")
              ),
              active = TRUE
            ),

            bs4Dash::bs4TabPanel(
              tabName = "Mobility Reports",

              shiny::tags$div(
                style = "text-align: center; padding: 10px;",
                shiny::tags$h1(
                  "Google Mobility Reports",
                  style = "padding-bottom: 25px;"
                ),
                DT::dataTableOutput("mobility_data")
              ),
              active = FALSE
            ),

            bs4Dash::bs4TabPanel(
              tabName = "USDA",
              shiny::tags$div(
                style = "text-align: center; padding: 10px;",
                shiny::tags$h1(
                  "Education and Income Levels - 2014-2018",
                  style = "padding-bottom: 25px;"
                ),
                DT::dataTableOutput("usda_data")
              ),
              active = FALSE
            )
          )
        ), # bs4TabItem()

        # Overview Tab
        bs4Dash::bs4TabItem(
          tabName = "tab-data-sources",

          bs4Dash::bs4ValueBox(
            value = shiny::tags$h3("Johns Hopkins University"),
            subtitle = "Github Repo with most up-to-date COVID-19 data",
            icon = "github",
            status = "primary",
            width = 12,
            href = "https://github.com/CSSEGISandData/COVID-19"
          ),

          bs4Dash::bs4ValueBox(
            value = shiny::tags$h3("Google Mobility Reports"),
            subtitle = "Economic Research Service",
            icon = "google",
            status = "info",
            width = 12,
            href = "https://www.google.com/covid19/mobility/"
          ),

          bs4Dash::bs4ValueBox(
            value = shiny::tags$h3("US Department of Agriculture"),
            subtitle = "Economic Research Service",
            icon = "external-link-alt",
            status = "success",
            width = 12,
            href = "https://www.ers.usda.gov/"
          )
        ) # bs4TabItem()
      ) # bs4TabItems()
    ),

    # Control bar
    controlbar = NULL,

    # Footer
    footer = NULL

  )
}
