# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)

# Load the dataset from Google Drive
food_data <- read.csv("https://drive.google.com/uc?export=download&id=1wIzAgpz-U6_CpDsnQILKMYDjeQibB99h")

# Define UI for the application
ui <- fluidPage(
  titlePanel("NYC Restaurant Food Orders - Interactive Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cuisine", "Select Cuisine Type:", choices = unique(food_data$cuisine_type), selected = "American"),
      sliderInput("prep_time", "Select Food Preparation Time Range:",
                  min = min(food_data$food_preparation_time, na.rm = TRUE),
                  max = max(food_data$food_preparation_time, na.rm = TRUE),
                  value = c(min(food_data$food_preparation_time, na.rm = TRUE), 
                            max(food_data$food_preparation_time, na.rm = TRUE))),
      sliderInput("cost", "Select Order Cost Range:",
                  min = min(food_data$cost_of_the_order, na.rm = TRUE),
                  max = max(food_data$cost_of_the_order, na.rm = TRUE),
                  value = c(min(food_data$cost_of_the_order, na.rm = TRUE), 
                            max(food_data$cost_of_the_order, na.rm = TRUE)))
    ),
    mainPanel(
      plotOutput("deliveryPlot"),
      plotOutput("prepPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    food_data %>%
      filter(cuisine_type == input$cuisine,
             food_preparation_time >= input$prep_time[1],
             food_preparation_time <= input$prep_time[2],
             cost_of_the_order >= input$cost[1],
             cost_of_the_order <= input$cost[2])
  })
  
  output$deliveryPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = delivery_time, y = cost_of_the_order)) +
      geom_point(color = "blue") +
      labs(title = paste("Delivery Time vs Order Cost for", input$cuisine),
           x = "Delivery Time (minutes)", 
           y = "Cost of the Order (USD)") +
      theme_minimal()
  })
  
  output$prepPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = food_preparation_time, y = as.numeric(rating))) +
      geom_point(color = "red") +
      labs(title = "Food Preparation Time vs Rating",
           x = "Food Preparation Time (minutes)",
           y = "Rating") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
