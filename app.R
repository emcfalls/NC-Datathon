# Load libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(shinyWidgets)
library(DT)

# Sample data (replace this with your actual data)
# Assume you have a data frame named 'cte_data' with columns: 'Year', 'Participation', 'SchoolType'

cte_data <- read_csv("data/school_concentrator_info.csv")

# Define UI
ui <- fluidPage(
  titlePanel("CTE Participation Visualization"),
  sidebarLayout(
    sidebarPanel(
      sliderTextInput("year", "Select Year:",
                      choices = as.character(2018:2022),
                      selected = c("2018", "2022"),
                      width = "300px",
                      animate = TRUE),
      selectInput("title_i", "By Title 1 Status:",
                  c("No", "Yes")),
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overall CTE Participation", 
                 plotOutput("cte_plot"), 
                 DTOutput("cte_table")),
        tabPanel("CTE Cluster Concentrators",
                 plotOutput("cte_plot_career_cluster"),
                 selectInput("cluster", "Select Career Cluster:",
                             c("All", colnames(cte_data[,c(22:36)]))
                 )
        )
      )
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  filtered_data <- reactive({
    data <- cte_data
    
    data <- filter(data, year >= input$year[1] & year <= input$year[2])
    
    if (input$title_i != "No") {
      data <- data %>%
        group_by(year, title_i) %>%
        summarise(pct = mean(pct, na.rm = T)) 
    }
    
    else {
      data <- data %>%
        group_by(year) %>%
        summarise(pct = mean(pct, na.rm = T)) 
    }
    
    data
  })
  
  output$cte_plot <- renderPlot({
    gg <- ggplot(filtered_data(), aes(x = as.factor(year), y = pct)) 
      
      if (input$title_i != "No") {
        gg <- gg +
          geom_line(aes(group = title_i, color = title_i), stat = "identity") +
          geom_point(aes(color = title_i))
      } else {
        gg <- gg +
          geom_line(stat = "identity") +
          geom_point() +
          geom_line() 
      }
      
    gg +
      labs(title = "CTE Participation Over Years",
           x = "Year",
           y = "Participation",
           color = "School Type") +
      theme_minimal()
  })
  
  output$cte_table <- renderDT({
    filtered_data()
  })
  
  # Server logic for the career cluster tab
  filtered_data_career_cluster <- reactive({
    data <- cte_data
    
    if (input$title_i != "No") {
      data <- data %>%
        group_by(year, title_i) %>%
        summarise(across(AAVC:TRAN, ~ mean(.x, na.rm = TRUE)))
      
      data <- data %>%
        select(year, 2:17) %>%
        pivot_longer(!c(year, title_i), names_to = "career_clusters", values_to = "avg_pct")
    }
    
    else {
      data <- data %>%
        group_by(year) %>%
        summarise(across(AAVC:TRAN, ~ mean(.x, na.rm = TRUE)))
      
      data <- data %>%
        select(year, 2:17) %>%
        pivot_longer(!c(year), names_to = "career_clusters", values_to = "avg_pct")
    }
    
    
    data <- filter(data, year >= input$year[1] & year <= input$year[2])

    if (input$cluster != "All") {
      data <- filter(data, career_clusters == input$cluster)
    }
    
    data
  })
  
  output$cte_plot_career_cluster <- renderPlot({
    
    gg2 <- ggplot(filtered_data_career_cluster(), aes(x = year, y = avg_pct, color = career_clusters)) 
    
    if (input$title_i == 'Yes') {
      gg2 <- gg2 +
        facet_wrap(~title_i)
    }
    
    gg2 +
      geom_line(stat = "identity") +
      geom_point() +
      labs(title = "CTE Participation by Career Cluster",
           x = "Year",
           y = "Participation",
           color = "Career Cluster") +
      theme_minimal()
  })
  
  
}

# Run the app
shinyApp(ui, server)