library(shiny)
library(ggplot2)
library(viridis)

# Define UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      uiOutput("year_slider")
      ), 
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server

server <- function(input, output) {
  
  df <- read.csv("top10_countries.csv") 
  
  output$year_slider <- renderUI({
    sliderInput("year_range", "Select Year Range:",
                min = min(df$Year), max = max(df$Year),
                value = c(min(df$Year), max(df$Year)), 
                step = 1)
    
  })
  output$plot <- renderPlot({
    # Filter data based on selected year range
    filtered_df <- subset(df, Year >= input$year_range[1] & Year <= input$year_range[2])
    
    # Plot
    p <- ggplot(filtered_df, aes(x = Year, y = cum_num_objects, fill = Entity)) + 
      geom_bar(stat = "identity") + 
      scale_fill_viridis_d() + 
      theme_minimal() + 
      labs(title = paste("Top 10 Countries' Total Number of Objects in Space from", input$year_range[1], "-", input$year_range[2]),
           y = "Number of Objects in Space", fill = "Country") + 
      theme(plot.title = element_text(hjust = 0.5, size = 22), 
            axis.title = element_text(size = 18), 
            axis.text = element_text(size = 16))
    
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)
