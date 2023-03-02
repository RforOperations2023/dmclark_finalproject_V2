library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(DT)
library(sp)
library(maps)
library(maptools)
library(lubridate)
#install.packages("maps")
#install.packages("maptools")
# Get the US state map

usmap <- map("state", plot = FALSE, fill = TRUE)

# Convert to a SpatialPolygonsDataFrame object
states <- map2SpatialPolygons(usmap, IDs = usmap$names, proj4string = CRS("+proj=longlat +datum=WGS84"))

#osncojnwnsocna;jvnsojvnjvnarjvnaejrnvaeojrvnaejrvneajrnvaejvnaejfvnafjvnafjna

# load the dataset
data <- read.csv("US Police shootings in from 2015-22.csv")

# Define UI
ui <- fluidPage(
  
  # Title
  titlePanel("US Police Shootings 2015-2022"),
  
  # Tabs with input commands and output panels
  tabsetPanel(
    
    # Tab 1: Input commands
    tabPanel(
      "Filters",
      sidebarLayout(
        sidebarPanel(
          # Filter by date range
          dateRangeInput("date_range", "Filter by date range:",
                         start = as.Date("2015-01-01"), end = Sys.Date()),
          # Filter by race
          selectInput("race_input", "Filter by race:",
                      choices = c("All", unique(data$race))),
          # Filter by state
          selectInput("state_input", "Filter by state:",
                      choices = c("All", unique(data$state)))
        ),
        
        # Main panel with Download button
        mainPanel(
          downloadButton("download_data", "Download filtered data")
        )
      )
    ),
    
    # Tab 2: Leaflet map
    tabPanel(
      "Map of the U.S.",
      leafletOutput("map")
    ),
    
    # Tab 3: Datatable
    tabPanel(
      "Data Table",
      dataTableOutput("table")
    ),
    
    # Tab 4: Plotly graph 1
    tabPanel(
      "Shootings by Race",
      plotlyOutput("graph1")
    ),
    
    # Tab 5: Plotly graph 2
    tabPanel(
      "Shootings by State",
      plotlyOutput("graph2")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  
  # Initialize leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      # Polygons for each state
      addPolygons(data = states,
                  fillColor = "blue", fillOpacity = 0.2, weight = 2,
                  options = list(clickable = FALSE))
  })
  
  # Filter the data based on input 
  filtered_data <- reactive({
    data %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
      filter(ifelse(input$race_input == "All", TRUE, race == input$race_input)) %>%
      filter(ifelse(input$state_input == "All", TRUE, state == input$state_input))
  })
  
  # Leaflet map with markers and polygons
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      # Markers for each shooting
      addMarkers(data = filtered_data(),
                 clusterOptions = markerClusterOptions()) %>%
      # Polygons for each state
      addPolygons(data = states,
                  fillColor = "blue", fillOpacity = 0.2, weight = 2,
                  options = list(clickable = FALSE))
  })
  
  # Datatable with raw data
  output$table <- renderDataTable({
    filtered_data()
  })
  
  output$graph1 <- renderPlotly({
    filtered_data() %>%
      # Convert date column to date format
      mutate(date = as.Date(date)) %>%
      # Group by race and month, and calculate the count of shootings
      group_by(race, month = format(date, "%Y-%m")) %>%
      summarise(shootings = n()) %>%
      # Filter by the selected races
      filter(ifelse(input$race_input == "All", TRUE, race %in% input$race_input)) %>%
      # Create a line chart with traces for each race
      plot_ly(x = ~month, y = ~shootings, color = ~race, type = "scatter", mode = "lines") %>%
      layout(title = "Shootings by race and month", xaxis = list(title = "Month"), yaxis = list(title = "Number of shootings"))
  })

  
  # Plotly graph 2: Pie chart of number of shootings by state
  output$graph2 <- renderPlotly({
    plot_ly(filtered_data(), labels = ~state) %>%
      add_pie(values = ~1) %>%
      layout(title = "Number of shootings by state", margin = list(b = 50))
  })
  
  # Download button for filtered data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("filtered_data", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(filtered_data(), file)
    }
  )
}

# Run the App 
shinyApp(ui,server)
