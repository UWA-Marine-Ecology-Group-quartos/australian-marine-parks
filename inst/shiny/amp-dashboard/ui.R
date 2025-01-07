
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
                             # TODO I' ve changed this to only invclude plots where we have dummy data
                             # "Coral Sea",
                             # "North",
                             # "North-west",
                             # "South-east",
                             # "South-west"#,
                             # "Temperate East"
                 ),

                 selected = "South-west"),
    # ),
#
#
#     # Conditionally display the park input based on filterpark selection
    conditionalPanel(
      condition = "input.toggle == 'Marine Park'",

      # Radio button for selecting the park, initially set to 'south_west'
      radioButtons("marine_park",
                   "Marine Park:",
                   choices = south_west)
    ),


    # selectInput(
    #   inputId = "metric",
    #   label = "Choose a Metric:",
    #   choices = unique(dropdown_data$metric),
    #   selected = unique(dropdown_data$metric)[1]
    # ),
    #
    # uiOutput("dynamic_ecosystem_component"),
    # uiOutput("dynamic_options")
  ),

  # Main panel with conditional content
nav_panel(
  title = "Dashboard",
  fluidRow(
    column(
      width = 7,
      div(
        # style = "overflow-y: auto; max-height: 100vh;",

        conditionalPanel(
          condition = "input.toggle == 'Network'",
          uiOutput("network_name")),

        conditionalPanel(
          condition = "input.toggle == 'Marine Park'",

          uiOutput("marinepark_name")),

        # fluidRow(column(width = 3,

        layout_columns(

          col_widths = c(3, 3, 6),

          selectInput(
          inputId = "metric",
          label = "Ecosystem component:",
          choices = unique(all_data$dropdown_data$metric),
          selected = unique(all_data$dropdown_data$metric)[1]
        ),

        uiOutput("dynamic_ecosystem_subcomponent"),#),

        uiOutput("dynamic_options")),

        # fluidRow(

        conditionalPanel(
          condition = "input.toggle == 'Network'",

          conditionalPanel(
            condition = "input.network == 'South-west'",

          conditionalPanel(
            condition = "input.options == 'Abundance of large-bodied generalist carnivores greater than Lm'",
            plotOutput("sw_lbc", height = 450)),

          conditionalPanel(
            condition = "input.options == 'Community Temperature Index'",
            plotOutput("sw_cti", height = 450))

          ),


        conditionalPanel(
          condition = "input.network == 'North-west'",

          conditionalPanel(
            condition = "input.options == 'Abundance of large-bodied generalist carnivores greater than Lm'",
            plotOutput("nw_lbc", height = 350)),

        conditionalPanel(
          condition = "input.options == 'Community Temperature Index'",
          plotOutput("nw_cti", height = 350)),
        )
        ),

        conditionalPanel(
          condition = "input.toggle == 'Marine Park'",

        conditionalPanel(
          condition = "input.options == 'Abundance of large-bodied generalist carnivores greater than Lm'",
              uiOutput("abundance_of_large_bodied_generalist_carnivores_plots"),
        ),

        conditionalPanel(
          condition = "input.options == 'Community Temperature Index'",
          uiOutput("community_temperature_index_plots"),
        ),

        ),


          #     accordion(open = FALSE,
          #               accordion_panel(
          #                 "Expand to see more plots",
          #                 layout_column_wrap(
          #                   width = 1/3,
          #                   fill = FALSE,
          #                   card(full_screen = TRUE, plotOutput("geo_lm", height = 200)),
          #                   card(full_screen = TRUE, plotOutput("geo_sr", height = 200)),
          #                   card(full_screen = TRUE, plotOutput("geo_cti", height = 200))
          #     )
          #   )
          # )
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
