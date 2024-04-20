library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(leaflet)
library(RColorBrewer) 

# Load and clean the data
aqs_data <- read_csv("annual_conc_by_monitor_2022.csv") %>%
  distinct() %>%
  mutate(
    `Local Site Name` = if_else(is.na(`Local Site Name`), "Unknown", `Local Site Name`),
    `Method Name` = if_else(is.na(`Method Name`), "Unknown", `Method Name`),
    Date = as.Date(`1st Max DateTime`),
    YearMonth = format(Date, "%Y-%m"),  # Creating a YearMonth column for aggregation
    `Arithmetic Mean` = replace_na(`Arithmetic Mean`, 0)  
  ) %>%
  drop_na()

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Air Quality Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("pollutant", "Select Pollutant:", choices = unique(aqs_data$`Parameter Name`)),  
      selectInput("state", "Select State:", choices = unique(aqs_data$`State Name`)),
      dateRangeInput("dateRange", "Select Date Range:", start = min(aqs_data$Date), end = max(aqs_data$Date)),
      sliderInput("maxConcentration", "Maximum Concentration:", min = min(aqs_data$`Arithmetic Mean`), 
                  max = max(aqs_data$`Arithmetic Mean`), value = c(min(aqs_data$`Arithmetic Mean`), max(aqs_data$`Arithmetic Mean`))),
      downloadButton("downloadData", "Download Filtered Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Trend Analysis", plotOutput("trendPlot")),
        tabPanel("Comparative Analysis", plotOutput("comparativePlot"), plotOutput("histPlot")),
        tabPanel("Geospatial Analysis", leafletOutput("mapPlot")),
        tabPanel("Data Table", DTOutput("dataView"))
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Dynamic UI updates
  observe({
    updateSelectInput(session, "pollutant", choices = unique(aqs_data$`Parameter Name`))
  })
  
  output$trendPlot <- renderPlot({
    filtered_data <- aqs_data %>%
      filter(`Parameter Name` == input$pollutant, between(Date, input$dateRange[1], input$dateRange[2])) %>%
      group_by(YearMonth) %>%
      summarize(`Mean Concentration` = mean(`Arithmetic Mean`, na.rm = TRUE), .groups = 'drop')
    
    ggplot(filtered_data, aes(x = YearMonth, y = `Mean Concentration`, group = 1)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = paste("Monthly Trend of", input$pollutant), x = "Month", y = "Mean Concentration") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$comparativePlot <- renderPlot({
    state_data <- aqs_data %>%
      filter(`State Name` == input$state, `Arithmetic Mean` <= input$maxConcentration)
    
    ggplot(state_data, aes(x = `County Name`, y = `Arithmetic Mean`, fill = `County Name`)) +
      geom_violin(trim = FALSE) +
      labs(title = paste("Pollution Levels by County in", input$state), x = "County", y = "Mean Concentration") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$histPlot <- renderPlot({
    state_data <- aqs_data %>%
      filter(`State Name` == input$state, `Arithmetic Mean` <= input$maxConcentration)
    
    ggplot(state_data, aes(x = `Arithmetic Mean`, fill = `County Name`)) +
      geom_histogram(bins = 30, alpha = 0.6) +
      facet_wrap(~`County Name`) +
      labs(title = "Distribution of Pollution Levels", x = "Mean Concentration", y = "Frequency")
  })
  
  output$mapPlot <- renderLeaflet({
    filtered_data <- aqs_data %>%
      filter(`Arithmetic Mean` <= input$maxConcentration, between(Date, input$dateRange[1], input$dateRange[2]))
    
    pal <- colorNumeric(palette = "YlOrRd", domain = filtered_data$`Arithmetic Mean`)
    
    leaflet(filtered_data) %>%
      addTiles() %>%
      addCircles(
        lng = ~Longitude, lat = ~Latitude, weight = 1, 
        radius = ~`Arithmetic Mean` * 100, popup = ~paste(`Parameter Name`, `Arithmetic Mean`, sep = "<br>"),
        color = ~pal(`Arithmetic Mean`), fillOpacity = 0.8, stroke = TRUE, fillColor = ~pal(`Arithmetic Mean`)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~`Arithmetic Mean`,
                title = "Mean Concentration", opacity = 1)
  })
  
  output$dataView <- renderDT({
    aqs_data %>%
      filter(`Parameter Name` == input$pollutant, `State Name` == input$state)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("filtered-data-", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(aqs_data %>%
                  filter(`Parameter Name` == input$pollutant, `State Name` == input$state),
                file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
