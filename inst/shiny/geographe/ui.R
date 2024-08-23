ui <- page_navbar(
  # thematic::thematic_shiny()
  title = div(
    "Geographe Marine Park Dashboard", # NOTE An item that would get changed

  ),
  sidebar = sidebar(
    # bg = "white",
    accordion(
      accordion_panel(
        "Primary controls",
        "Primary controls go here"
      ),
      accordion_panel(
        "Other controls",
        "Extra controls go here"
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
          min_height = 200,

          card(
            full_screen = TRUE,
            # plotOutput(),
            card_body(
              fill = FALSE, gap = 0,
              card_title(">Lm large-bodied generalist carnivores abundance by size class"),
              p(class = "text-muted", "Caption for plot of >Lm large-bodied generalist carnivores abundance by size class")
            )
          ),

          card(
            full_screen = TRUE,
            # plotOutput(),
            card_body(
              fill = FALSE, gap = 0,
              card_title("<Lm large-bodied generalist carnivores abundance by size class"),
              p(class = "text-muted", "Caption for plot of <Lm large-bodied generalist carnivores abundance by size class")
            )
          ),

          card(
            full_screen = TRUE,
            # plotOutput(),
            card_body(
              fill = FALSE, gap = 0,
              card_title("CTI"),
              p(class = "text-muted", "Caption for plot of CTI")
            )
          )
        ),

        card(
          # card_header("Map"),
          full_screen = TRUE,
          leafletOutput("demersal_fish_map"),
          card_body(
            fill = FALSE, gap = 0,
            card_title("Raster outputs"),
            p(class = "text-muted", "All the raster outputs will go on a leaflet here?
              Where you can switch between layers?")
          )
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
        title = "Suberged Aquatic Vegetation*",
        p("Content for Extent/% cover of vegetation map tab.")
      ),
    )
    # )
  ),
  nav_panel(
    "Socio-economic values",
    card(
      title = "Socio-economic values",
      p("Content for the Socio-economic values section.")
      # Add more content here
    )
  ),
  nav_panel(
    "Pressures",
    card(
      title = "Pressures",
      p("Content for the Pressures section.")
      # Add more content here
    )
  ),
  nav_item(input_dark_mode()),
  nav_item(tags$img(src = "https://marineecology.io/images/meg_logo_and_title.png", height = "30px", style = "float: right;"))
)
