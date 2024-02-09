
library(readr)
library(shiny)
library(ggplot2)
library(dplyr)

# postsecondary <- read_csv("data/postsecondary-wages-race.csv")
# cte_data <-read_csv("data/secondary-wages-race.csv")



# ui <- fluidPage(
#   titlePanel("CTE Enrollment Proportions by Career Cluster and Race"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("cluster1", "Select Cluster 1:", choices = unique(cte_data$`Job Type`)),
#       selectInput("cluster2", "Select Cluster 2:", choices = unique(cte_data$`Job Type`)[-1])
#     ),
#     mainPanel(
#       plotOutput("proportionPlot")
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   
#   observe({
#     available_clusters <- setdiff(unique(cte_data$`Job Type`), input$cluster1)
#     updateSelectInput(session, "cluster2", choices = available_clusters)
#   })
#   
#   output$proportionPlot <- renderPlot({
#     filtered_data <- cte_data %>%
#       filter(`Job Type` %in% c(input$cluster1, input$cluster2)) %>%
#       mutate(Cluster = factor(`Job Type`, levels = c(input$cluster1, input$cluster2))) 
#     
# 
#     ggplot(filtered_data, aes(x = Race, y = Proportion, fill = Cluster)) +
#       geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
#       scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
#       theme_minimal() +
#       labs(title = "CTE Enrollment Proportions Comparison",
#            x = "Race", y = "Proportion") +
#       scale_fill_brewer(palette = "Set2") + 
#       theme(legend.text = element_text(size = 12), 
#             legend.title = element_text(size = 14))
#   })
# }
# 
# shinyApp(ui = ui, server = server)
# 
# 


secondary_data <-read_csv("data/secondary-wages-race.csv")
postsecondary_data <-read_csv("data/postsecondary-wages-race.csv")

# ui <- fluidPage(
#   titlePanel("CTE Enrollment Proportions by Education Level and Cluster"),
#   sidebarLayout(
#     sidebarPanel(
#       radioButtons("educationLevel", "Select Education Level:",
#                    choices = c("Secondary" = "secondary", "Postsecondary" = "postsecondary")),
#       selectInput("cluster1", "Select Cluster 1:", choices = NULL),  # Choices are populated in server.R
#       selectInput("cluster2", "Select Cluster 2:", choices = NULL)   # Choices are populated in server.R
#     ),
#     mainPanel(
#       plotOutput("proportionPlot")
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   selected_dataset <- reactive({
#     if (input$educationLevel == "secondary") {
#       secondary_data
#     } else {
#       postsecondary_data
#     }
#   })
#   
#   observe({
#     updateSelectInput(session, "cluster1",
#                       choices = unique(selected_dataset()$`Job Type`))
#     updateSelectInput(session, "cluster2",
#                       choices = unique(selected_dataset()$`Job Type`)[-1]) 
#   })
#   
#   observe({
#     available_clusters <- setdiff(unique(selected_dataset()$`Job Type`), input$cluster1)
#     updateSelectInput(session, "cluster2", choices = available_clusters)
#   })
#   
#   output$proportionPlot <- renderPlot({
#     filtered_data <- selected_dataset() %>%
#       filter(`Job Type` %in% c(input$cluster1, input$cluster2)) %>%
#       mutate(Cluster = factor(`Job Type`, levels = c(input$cluster1, input$cluster2))) 
#     
#     # Plot
#     ggplot(filtered_data, aes(x = Race, y = Proportion, fill = Cluster)) +
#       geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
#       scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
#       theme_minimal() +
#       labs(title = paste("CTE Enrollment Proportions for", input$educationLevel, "Education"),
#            x = "Race", y = "Proportion") +
#       scale_fill_brewer(palette = "Set2") + 
#       theme(legend.text = element_text(size = 12),  
#             legend.title = element_text(size = 14))
#   })
# }
# 
# shinyApp(ui = ui, server = server)

library(shiny)
library(ggplot2)
library(dplyr)


# UI
ui <- fluidPage(
  titlePanel("2020-2021 North Carolina CTE Enrollment Dashboard"),
  tabsetPanel(
    tabPanel("Participation Rates",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("educationLevel", "Select Education Level:",
                              choices = c("Secondary" = "secondary", "Postsecondary" = "postsecondary")),
                 selectInput("cluster1", "Select Cluster 1:", choices = NULL),  
                 selectInput("cluster2", "Select Cluster 2:", choices = NULL)   
               ),
               mainPanel(
                 plotOutput("proportionPlot")
               )
             )
    ),
    tabPanel("Wages by Demographics",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selectedCluster", "Select Career Cluster:", choices = unique(secondary_data$`Job Type`))
               ),
               mainPanel(
                 plotOutput("wagePlot")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  selected_dataset <- reactive({
    if (input$educationLevel == "secondary") {
      secondary_data
    } else {
      postsecondary_data
    }
  })
  
  observe({
    updateSelectInput(session, "cluster1",
                      choices = unique(selected_dataset()$`Job Type`))
    updateSelectInput(session, "cluster2",
                      choices = unique(selected_dataset()$`Job Type`)[-1])
  })
  
  observe({
    available_clusters <- setdiff(unique(selected_dataset()$`Job Type`), input$cluster1)
    updateSelectInput(session, "cluster2", choices = available_clusters)
  })
  
  output$proportionPlot <- renderPlot({
    filtered_data <- selected_dataset() %>%
      filter(`Job Type` %in% c(input$cluster1, input$cluster2)) %>%
      mutate(Cluster = factor(`Job Type`, levels = c(input$cluster1, input$cluster2)))
    
    # Plot
    ggplot(filtered_data, aes(x = Race, y = Proportion, fill = Cluster)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
      theme_minimal() +
      labs(title = paste("CTE Enrollment Proportions for", input$educationLevel, "Education"),
           x = "Race", y = "Proportion") +
      scale_fill_brewer(palette = "Set2") +
      theme(legend.text = element_text(size = 12),
            legend.title = element_text(size = 14))
  })
  
  # Wage plot
  # output$wagePlot <- renderPlot({
  #   filtered_data <- cte_data %>%
  #     filter(`Job Type` == input$selectedCluster)
  #   
  #   # Plot
  #   ggplot(filtered_data, aes(x = Race, y = `Annual wage; mean`, fill = Race)) +
  #     geom_bar(stat = "identity", position = "dodge") +
  #     theme_minimal() +
  #     labs(title = paste("Average Annual Wages for", input$selectedCluster),
  #          x = "Demographic", y = "Average Annual Wage") +
  #     scale_fill_brewer(palette = "Set1")
  # })
  # 
  # observe({
  # })
}

shinyApp(ui = ui, server = server)




