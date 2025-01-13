ui <- page_navbar(
  id = "navbar_id", # Add an ID to track the active panel
  title = div(
    HTML(paste0(
      #<b>
      "Australian Marine Parks Dashboard <i>(this is a draft and contains fake data DO NOT USE FOR INTERPRETATION)</i>"
      # </b>
    )),
  ),
  nav_spacer(),

  # Sidebar with radio button and dropdowns
  sidebar = sidebar(
    id = "main_sidebar", # Add an ID to the sidebar
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

      uiOutput("dynamic_marine_park")#,

      #   # Radio button for selecting the park, initially set to 'south_west'
      #   radioButtons("marine_park",
      #                "Marine Park:",
      #                choices = south_west)
    ),

  ),

  useShinyjs(), # Enable shinyjs

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
            uiOutput("network_name_1")),

          conditionalPanel(
            condition = "input.toggle == 'Marine Park'",

            uiOutput("marinepark_name_1")),


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

          uiOutput("metric_name"),
          # h6("Condition:"),
          uiOutput("condition_plot_ui"),

          uiOutput("dynamic_text"),
          # h6("Temporal:"),
          uiOutput("temporal_plot_ui"),
        )

      ),
      column(
        width = 5,
        # div(
          card(full_screen = TRUE, max_height = "100%",
          id = "map-container",
          # style = "position: sticky; top: 0; height: 100vh;",
          style = "height: 85vh;",
          leafletOutput("australia_map"
                        ))
        # )
      )
    )
  ),
  nav_panel(
    title = "FishNClips",
    leafletOutput("fishnclips", height = "85%")),

  nav_panel(
    title = "Summary Statistics",

    # Conditional panels for name of the view ----
    conditionalPanel(
      condition = "input.toggle == 'Network'",

      uiOutput("network_name_2")#,
      # uiOutput("ui_network")
    ),

    conditionalPanel(
      condition = "input.toggle == 'Marine Park'",

      uiOutput("marinepark_name_2")#,
      # uiOutput("ui_marine_park")
    ),

    page_fillable(
      layout_column_wrap(height = 200, fill = FALSE,
        value_box(
          title = "Fish counted",
          theme = "primary",
          value = textOutput("fish_counted"),
          showcase = icon("fish")
        ),

        value_box(
          title = "Fish species identified",
          theme = "primary",
          value = textOutput("fish_species"),
          showcase = icon("fish")
        ),

        value_box(
          title = "Total hours of video watched",
          theme = "primary",
          value = textOutput("hours_watched"),
          showcase = bs_icon("clock")
        ),

        value_box(
          title = "stereo-BRUVs deployed",
          theme = "primary",
          value = textOutput("bruvs_deployed"),
          showcase = img(src = "stereo-BRUV_filled_transparent.png",
                         height = "80px",
                         style = "margin-left: 15px;" # Adjust the value as needed)
        )
        ))),

    conditionalPanel(
      condition = "input.toggle == 'Network'",

      # uiOutput("network_name_2"),
      uiOutput("ui_network")
    ),

    conditionalPanel(
      condition = "input.toggle == 'Marine Park'",

      # uiOutput("marinepark_name_2"),
      uiOutput("ui_marine_park")
    ),



  ),

  nav_item(input_dark_mode()),
  nav_item(tags$img(src = "https://marineecology.io/images/meg_logo_and_title.png", height = "30px", style = "float: right;"))
)
