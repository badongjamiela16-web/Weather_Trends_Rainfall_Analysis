
library(shiny)
library(ggplot2)
library(dplyr)

# Load and prepare the dataset
weather <- read.csv("weather_trends_rainfall.csv")
weather$Date <- as.Date(weather$Date)

ui <- fluidPage(
  titlePanel("Weather Trends and Rainfall Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        "dates",
        "Select date range:",
        start = min(weather$Date, na.rm = TRUE),
        end = max(weather$Date, na.rm = TRUE),
        min = min(weather$Date, na.rm = TRUE),
        max = max(weather$Date, na.rm = TRUE)
      ),
      
      selectInput(
        "weather_type",
        "Weather type:",
        choices = c("All", sort(unique(weather$WeatherType))),
        selected = "All"
      )
    ),
    
    mainPanel(
      h3("Summary"),
      fluidRow(
        column(4, wellPanel(
          h4("Average max temperature"),
          textOutput("avg_temp")
        )),
        column(4, wellPanel(
          h4("Total rainfall"),
          textOutput("total_rain")
        )),
        column(4, wellPanel(
          h4("Days in selection"),
          textOutput("day_count")
        ))
      ),
      
      h3("Daily temperature"),
      plotOutput("temperature_plot"),
      
      h3("Daily rainfall"),
      plotOutput("rainfall_plot"),
      
      h3("Filtered weather data"),
      tableOutput("weather_table")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_weather <- reactive({
    data <- weather %>%
      filter(Date >= input$dates[1], Date <= input$dates[2])
    
    if (input$weather_type != "All") {
      data <- data %>% filter(WeatherType == input$weather_type)
    }
    
    data
  })
  
  output$avg_temp <- renderText({
    data <- filtered_weather()
    if (nrow(data) == 0) return("No data")
    paste0(round(mean(data$MaxTemp_C, na.rm = TRUE), 1), " °C")
  })
  
  output$total_rain <- renderText({
    data <- filtered_weather()
    if (nrow(data) == 0) return("No data")
    paste0(round(sum(data$Rainfall_mm, na.rm = TRUE), 1), " mm")
  })
  
  output$day_count <- renderText({
    nrow(filtered_weather())
  })
  
  output$temperature_plot <- renderPlot({
    data <- filtered_weather()
    validate(need(nrow(data) > 0, "No records match these filters."))
    
    ggplot(data, aes(x = Date)) +
      geom_line(aes(y = MaxTemp_C, color = "Maximum"), linewidth = 1) +
      geom_line(aes(y = MinTemp_C, color = "Minimum"), linewidth = 1) +
      labs(
        x = "Date",
        y = "Temperature (°C)",
        color = "Temperature",
        title = "Maximum and minimum temperature by day"
      ) +
      theme_minimal()
  })
  
  output$rainfall_plot <- renderPlot({
    data <- filtered_weather()
    validate(need(nrow(data) > 0, "No records match these filters."))
    
    ggplot(data, aes(x = Date, y = Rainfall_mm)) +
      geom_col() +
      labs(
        x = "Date",
        y = "Rainfall (mm)",
        title = "Daily rainfall"
      ) +
      theme_minimal()
  })
  
  output$weather_table <- renderTable({
    filtered_weather()
  })
}

shinyApp(ui = ui, server = server)
