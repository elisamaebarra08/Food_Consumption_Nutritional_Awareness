
library(shiny)
library(dplyr)
library(ggplot2)

food <- read.csv("food_consumption_nutrition.csv")

ui <- fluidPage(
  titlePanel("Food Consumption Patterns and Nutritional Awareness"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "age_group",
        "Filter by age group:",
        choices = c("All", sort(unique(food$AgeGroup))),
        selected = "All"
      )
    ),
    
    mainPanel(
      h3("Summary"),
      textOutput("respondent_count"),
      textOutput("average_fruit_veg"),
      textOutput("average_sugary_drinks"),
      textOutput("average_awareness"),
      
      plotOutput("food_plot"),
      plotOutput("awareness_plot"),
      
      h3("Filtered Sample Data"),
      tableOutput("food_table")
    )
  )
)

server <- function(input, output) {
  
  filtered_food <- reactive({
    if (input$age_group == "All") {
      food
    } else {
      food %>% filter(AgeGroup == input$age_group)
    }
  })
  
  output$respondent_count <- renderText({
    paste("Sample records:", nrow(filtered_food()))
  })
  
  output$average_fruit_veg <- renderText({
    paste(
      "Average fruit and vegetable servings per day:",
      round(mean(filtered_food()$FruitVegServingsPerDay), 2)
    )
  })
  
  output$average_sugary_drinks <- renderText({
    paste(
      "Average sugary drinks per week:",
      round(mean(filtered_food()$SugaryDrinksPerWeek), 2)
    )
  })
  
  output$average_awareness <- renderText({
    paste(
      "Average sample awareness score:",
      round(mean(filtered_food()$NutritionAwarenessScore), 2)
    )
  })
  
  output$food_plot <- renderPlot({
    filtered_food() %>%
      group_by(AgeGroup) %>%
      summarise(
        AverageServings = mean(FruitVegServingsPerDay),
        .groups = "drop"
      ) %>%
      ggplot(aes(x = AgeGroup, y = AverageServings)) +
      geom_col() +
      labs(
        title = "Average Fruit and Vegetable Servings",
        x = "Age Group",
        y = "Average Servings per Day"
      ) +
      theme_minimal()
  })
  
  output$awareness_plot <- renderPlot({
    ggplot(
      filtered_food(),
      aes(
        x = NutritionAwarenessScore,
        y = FruitVegServingsPerDay
      )
    ) +
      geom_point() +
      labs(
        title = "Awareness Score and Fruit/Vegetable Servings",
        x = "Sample Awareness Score",
        y = "Servings per Day"
      ) +
      theme_minimal()
  })
  
  output$food_table <- renderTable({
    filtered_food()
  })
}

shinyApp(ui = ui, server = server)