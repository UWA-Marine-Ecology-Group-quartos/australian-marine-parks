ui <- page_navbar(
  # thematic::thematic_shiny()
  title = div(
    "Australian Marine Parks Dashboard",

  ),
  sidebar = sidebar(

    width = 300,

    accordion(
      accordion_panel(
        "Filter Data",

        # First radio button for selecting network
        radioButtons("network", "Choose a Network:",

                     choices = c(
                       "Coral Sea",
                       "North",
                       "North-west",
                       "South-east",
                       "South-west",
                       "Temperate East"
                     ),

                     selected = "South-west"),
      # ),


      # Second radio button for selecting filter type (Network or Marine Park)
        radioButtons("filterpark",
                     "Plot by Network or Marine Park:",
                     choices = c("Network", "Marine Park"),
                     selected = "Network"),

      # Conditionally display the park input based on filterpark selection
      conditionalPanel(
        condition = "input.filterpark == 'Marine Park'",

        # Radio button for selecting the park, initially set to 'south_west'
        radioButtons("park",
                     "Park:",
                     choices = south_west)
      )
      )
    )
  ),
  nav_spacer(),  # Add space between sidebar and content
  nav_panel(
    title = "Natural Values",

    navset_card_pill(
      nav_panel(
        title = "Demersal fish",

        layout_column_wrap(
          width = 1 / 3,
          fill = FALSE,
          min_height = 100,
          max_height = 300,

          card(
            full_screen = TRUE,

            card_title(">Lm large-bodied generalist carnivores"), # abundance by size class

            plotOutput("geo_lm", height = 200)#, height = "100%")#,
            # plotOutput(),
            # card_body(
            #   fill = FALSE, gap = 0,
            #
            #
            #   #p(class = "text-muted", "Caption for plot of >Lm large-bodied generalist carnivores abundance by size class")
            # )
          ),

          card(
            full_screen = TRUE,

            card_title("Species richness"),

            plotOutput("geo_sr", height = 200)#,
            # plotOutput(),
            # card_body(
            #   fill = FALSE, gap = 0,
            #   # card_title("<Lm large-bodied generalist carnivores abundance by size class"), # change back later
            #
            #
            #   # p(class = "text-muted", "Caption for plot of <Lm large-bodied generalist carnivores abundance by size class") # change back later
            #   #p(class = "text-muted", "Caption for species richness")
            # )
          ),

          card(
            full_screen = TRUE,

            card_title("CTI"),

            plotOutput("geo_cti", height = 200)#,
            # plotOutput(),
            # card_body(
            #   fill = FALSE, gap = 0,
            #
            #
            #   #p(class = "text-muted", "Caption for plot of CTI")
            # )
          )
        ),

        card(
          min_height = 400,
          # card_header("Map"),
          full_screen = TRUE,
          card_title("Spatial model outputs"),
          leafletOutput("demersal_fish_map"),
          # card_body(
          #   fill = FALSE, gap = 0,
          #
          #   # p(class = "text-muted", "All the raster outputs will go on a leaflet here?
          #     # Where you can switch between layers?")
          # )
        )
      ),
      nav_panel(
        title = "Mobile macro invertebrates",
        p("Content for Mobile macro invertebrates tab.")
      ),
      nav_panel(
        title = "Sessile invertebrates",
        p("Content for Sessile invertebrates tab.")
      ),
      nav_panel(
        title = "Macroalgae",
        p("Content for Macroalgae tab.")
      ),
      nav_panel(
        title = "Seagrass",
        p("Content for Seagrass tab.")
      ),
      nav_panel(
        title = "Rock",
        p("Content for Rock tab.")
      ),
      nav_panel(
        title = "Benthic ecosystem map",
        p("Content for Benthic ecosystem map tab.")
      ),
      nav_panel(
        title = "Suberged Aquatic Vegetation",
        p("Content for Extent/% cover of vegetation map tab.")
      ),
    )
    # )
  ),
  nav_panel(
    "Socio-economic Values",
    navset_card_pill(
      nav_panel(
        title = "Knowledge",
        p("Content")
      ),
      nav_panel(
        title = "Attitude",
        p("Content")
      ),
      nav_panel(
        title = "Practice",
        p("Content")
      ),
      nav_panel(
        title = "Use",
        p("Content")
      )
    )
  ),
  nav_panel(
    "Pressures",

    navset_card_pill(
      nav_panel(
        title = "Practice - Use and catch data",
        p("Content")
        ),
      nav_panel(
        title = "Ocean acidity",
        p("Content")
      ),
      nav_panel(
        title = "Sea level anomaly",
        p("Content")
      ),
      nav_panel(
        title = "Chlorophyll",
        p("Content")
      ),
      nav_panel(
        title = "Sea surface temperature",
        p("Content")
      ),
      nav_panel(
        title = "Degree heating weeks",
        p("Content")
      )
)
  ),
  nav_item(input_dark_mode()),
  nav_item(tags$img(src = "https://marineecology.io/images/meg_logo_and_title.png", height = "30px", style = "float: right;"))
)
