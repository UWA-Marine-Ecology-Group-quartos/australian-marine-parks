ui <- page_navbar(
  title = div(
    "Australian Marine Parks Dashboard",
  ),
  nav_spacer(),

  # Sidebar with radio button and dropdowns
  sidebar = sidebar(
    width = 300,

    radioButtons(
      inputId = "toggle",
      label = "Investigate:",
      choices = c("Network", "Marine Park"),
      selected = "Network"
    ),

    #     # First radio button for selecting network
    radioButtons("network", "Choose a Network:",
                 choices = c(unique(all_data$file_info$network)
                 ),

                 selected = "South-west"),
    #     # Conditionally display the park input based on filterpark selection
    conditionalPanel(
      condition = "input.toggle == 'Marine Park'",

      # Radio button for selecting the park, initially set to 'south_west'
      radioButtons("marine_park",
                   "Marine Park:",
                   choices = south_west)
    ),

  ),

  # Main panel with conditional content
  nav_panel(
    title = "Dashboard",
    fluidRow(
      column(
        width = 7,
        div(

          # Conditional panels for name of the view ----
          conditionalPanel(
            condition = "input.toggle == 'Network'",
            uiOutput("network_name")),

          conditionalPanel(
            condition = "input.toggle == 'Marine Park'",

            uiOutput("marinepark_name")),


          # Row of dropdowns -----
          layout_columns(

            col_widths = c(3, 3, 6),

            selectInput(
              inputId = "metric",
              label = "Ecosystem component:",
              choices = unique(all_data$dropdown_data$metric),
              selected = unique(all_data$dropdown_data$metric)[1]
            ),

            uiOutput("dynamic_ecosystem_subcomponent"),
            uiOutput("dynamic_options")
          ),

          uiOutput("condition_plot_ui"),
          plotOutput("temporal_plot"),
        )

      ),
      column(
        width = 5,
        div(
          id = "map-container",
          style = "position: sticky; top: 0; height: 100vh;",
          leafletOutput("australia_map", height = "85%")
        )
      )
    )
  ),
  nav_panel(
    title = "FishNClips"),
  nav_panel(
    title = "Summary Statistics"),

  nav_item(input_dark_mode()),
  nav_item(tags$img(src = "https://marineecology.io/images/meg_logo_and_title.png", height = "30px", style = "float: right;"))
)
