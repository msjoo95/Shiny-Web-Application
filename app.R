if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, leaflet, dplyr, lubridate, sp, adehabitatHR, readr, geosphere, viridis)

# Load data
d <- read_csv("./data/Food_for_thought_Cebus_capucinus_Bob_DaVinci.csv")
d <- d %>%
  dplyr::select(timestamp, `location-long`, `location-lat`, `individual-local-identifier`) %>%
  mutate(timestamp = ymd_hms(timestamp)) %>%
  arrange(`individual-local-identifier`, timestamp)

# Filter valid coordinates only
d <- d %>% filter(!is.na(`location-long`) & !is.na(`location-lat`))

# SpatialPointsDataFrame for MCP
spdf <- SpatialPointsDataFrame(
  coords = dplyr::select(d, `location-long`, `location-lat`),
  data = d,
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

# UI
ui <- fluidPage(
  titlePanel("Capuchin Monkey GPS Viewer"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("ids", "Select Individuals:",
                         choices = unique(d$`individual-local-identifier`),
                         selected = unique(d$`individual-local-identifier`)),
      checkboxInput("show_mcp", "Show MCP", TRUE),
      sliderInput("mcp_percent", "MCP Percent:", min = 50, max = 100, value = 95, step = 5),
      checkboxInput("show_all", "Show All Dates", FALSE),
      sliderInput("date_slider", "Select Date:",
                  min = as.Date(min(d$timestamp)),
                  max = as.Date(max(d$timestamp)),
                  value = as.Date(min(d$timestamp)),
                  timeFormat = "%Y-%m-%d",
                  step = 1,
                  animate = animationOptions(interval = 1500, loop = TRUE))
    ),
    mainPanel(
      leafletOutput("map", height = 700)
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive MCP based on user input
  mcp_reactive <- reactive({
    req(input$mcp_percent)
    mcp(spdf[, "individual-local-identifier"], percent = input$mcp_percent)
  })

  output$map <- renderLeaflet({
    df <- d %>% filter(
      `individual-local-identifier` %in% input$ids,
      if (input$show_all) TRUE else as.Date(timestamp) == input$date_slider
    )

    pal_id <- colorFactor("viridis", domain = unique(df$`individual-local-identifier`))

    m <- leaflet(df) %>%
      addProviderTiles("CartoDB.Positron")

    for (id in input$ids) {
      m <- m %>% addCircleMarkers(
        data = df %>% filter(`individual-local-identifier` == id),
        lng = ~`location-long`, lat = ~`location-lat`,
        color = ~pal_id(`individual-local-identifier`),
        radius = 2, stroke = FALSE, fillOpacity = 0.7)
    }

    if (input$show_mcp) {
      m <- m %>% addPolygons(data = mcp_reactive(),
                             color = 'blue', weight = 2,
                             fillOpacity = 0.2)
    }

    m
  })
}

shinyApp(ui, server)
