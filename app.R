# Load libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(shinyWidgets)
library(DT)
library(viridis)
library(RColorBrewer)

# Sample data (replace this with your actual data)
# Assume you have a data frame named 'cte_data' with columns: 'Year', 'Participation', 'SchoolType'

school_information <- read_csv("data/school_information.csv")
clusters_per_school <- read_csv("data/career_cluster_information.csv")
concentrators <- read_csv("data/concentrator_information.csv")
secondary_data <-read_csv("data/secondary-wages-race.csv")
postsecondary_data <-read_csv("data/postsecondary-wages-race.csv")

clusters_per_school <- clusters_per_school %>%
  mutate(csi_lp = ifelse(csi_lp == 'N', 'Not CSI School', 'CSI School'),
         tsi_cu = ifelse(tsi_cu == 'N', 'Not TSI School', 'TSI School'),
         title_i = ifelse(title_i == 'N', 'Not Title I School', 'Title I School'),
         majority_white = ifelse(majority_white, 'Majority white', 'Majority Non-white'))
  

# Define UI
ui <- fluidPage(
  titlePanel("North Carolina CTE Access and Enrollment Dashboard"),
      tabsetPanel(
        tabPanel("Overall NC Participation Rates",
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
        tabPanel("NC CTE Cluster Participation",
                 sliderTextInput("year", "Select Year:",
                                 choices = as.character(2018:2022),
                                 selected = c("2018", "2022"),
                                 width = "300px",
                                 animate = TRUE),
                 selectInput("grouped", "By School Type or LEA Demographic:",
                             c("None",
                               "Title I Status", 
                               "Comprehensive Support and Improvement (CSI)",
                               "Targeted Support and Improvement (TSI)",
                               "Majority White LEA")),
                 plotOutput("cluster_participation"),
                 selectInput("cluster", "Select Career Cluster:",
                             c("All", colnames(concentrators[,c(44:58)])))
                             
                 ),
        tabPanel("NC CTE Access", 
                 selectInput("grouped2", "By School Type or LEA Demographic:",
                             c("None",
                               "Title I Status", 
                               "Comprehensive Support and Improvement (CSI)",
                               "Targeted Support and Improvement (TSI)",
                               "Majority White LEA")),
                 plotOutput("access_plot"),
                 plotOutput("disparity_plot"))
        )
      )
    
  



# Define server logic
server <- function(input, output, session) {
  
  access_data <- reactive({
    data <- clusters_per_school
    
    data <- filter(data, year >= input$year[1] & year <= input$year[2])
    
    if (input$grouped2 != "None") {
      
      if (input$grouped2 == "Title I Status") {
     
        data <- data %>%
          group_by(year, title_i) %>%
          summarise(across(AAVC:TRAN, ~ mean(.x, na.rm = TRUE))) %>%
          pivot_longer(!c(year, title_i), names_to = "career_cluster", values_to = "pct_have") %>%
          filter(!is.na(title_i))
      }
      
      else if (input$grouped2 == "Comprehensive Support and Improvement (CSI)") {
        
        data <- data %>%
          group_by(year, csi_lp) %>%
          summarise(across(AAVC:TRAN, ~ mean(.x, na.rm = TRUE))) %>%
          pivot_longer(!c(year, csi_lp), names_to = "career_cluster", values_to = "pct_have") %>%
          filter(!is.na(csi_lp))
      }
      
      else if (input$grouped2 == "Targeted Support and Improvement (TSI)") {
        
        data <- data %>%
          group_by(year, tsi_cu) %>%
          summarise(across(AAVC:TRAN, ~ mean(.x, na.rm = TRUE))) %>%
          pivot_longer(!c(year, tsi_cu), names_to = "career_cluster", values_to = "pct_have") %>%
          filter(!is.na(tsi_cu))
      }
      
      else {
        
        data <- data %>%
          group_by(year, majority_white) %>%
          summarise(across(AAVC:TRAN, ~ mean(.x, na.rm = TRUE))) %>%
          pivot_longer(!c(year, majority_white), names_to = "career_cluster", values_to = "pct_have") %>%
          filter(!is.na(majority_white))
      }
    }
    
    else {
      data <- data %>%
        group_by(year) %>%
        summarise(across(AAVC:TRAN, ~ mean(.x, na.rm = TRUE))) %>%
        pivot_longer(!c(year), names_to = "career_cluster", values_to = "pct_have")
    }
    
    data
  })
  
  output$access_plot <- renderPlot({
    gg <- ggplot(access_data(), aes(x =  career_cluster, y = pct_have)) 
      
    if (input$grouped2 == "Title I Status") {
        gg <- gg +
          geom_bar(aes(group = title_i, fill = title_i), stat = "identity", position = "dodge", size = 0.3) +
          scale_fill_discrete(labels = c("Not Title I", "Title I")) +
          scale_fill_brewer(palette = "Set2") 
      } 
    
    else if (input$grouped2 == "Comprehensive Support and Improvement (CSI)") {
      gg <- gg +
        geom_bar(aes(group = csi_lp, fill = csi_lp), stat = "identity", position = "dodge", size = 0.3) +
        scale_fill_discrete(labels = c("Not CSI", "CSI")) +
        scale_fill_brewer(palette = "Set2") 
    } 
    
    else if (input$grouped2 == "Targeted Support and Improvement (TSI)") {
      gg <- gg +
        geom_bar(aes(group = tsi_cu, fill = tsi_cu), stat = "identity", position = "dodge", size = 0.3) +
        scale_fill_discrete(labels = c("Not TSI", "TSI")) +
        scale_fill_brewer(palette = "Set2") 
    } 
    
    else if (input$grouped2 == "Majority White LEA") {
      gg <- gg +
        geom_bar(aes(group = majority_white, fill = majority_white), stat = "identity", position = "dodge", size = 0.3) +
        scale_fill_discrete(labels = c("Majority Non-White LEA", "Majority White LEA")) +
        scale_fill_brewer(palette = "Set2") 
    } 
    
    else {
        gg <- gg +
          geom_bar(stat = "identity") +
          scale_fill_brewer(palette = "Set2")
      }
      
    gg +
      labs(title = "CTE Career Cluster Access",
           x = "Career Clusters",
           y = "Proportion of Schools with Cluster",
           fill = "School Type") +
      theme_minimal()
  })
  
 
  disparity_data <- reactive({
    
    data <- clusters_per_school
    
    data <- filter(data, year >= input$year[1] & year <= input$year[2])
    
    if (input$grouped2 != "None") {
      
      if (input$grouped2 == "Title I Status") {
        
        data <- data %>%
          group_by(year, title_i) %>%
          summarise(across(AAVC:TRAN, ~ mean(.x, na.rm = TRUE))) %>%
          pivot_longer(!c(year, title_i), names_to = "career_cluster", values_to = "pct_have") %>%
          filter(!is.na(title_i)) %>%
          arrange(career_cluster) %>%
          ungroup () %>%
          group_by(career_cluster) %>%
          summarise(access_disparity = pct_have[1] - pct_have[2]) 
    
      }
      
      else if (input$grouped2 == "Comprehensive Support and Improvement (CSI)") {
        
        data <- data %>%
          group_by(year, csi_lp) %>%
          summarise(across(AAVC:TRAN, ~ mean(.x, na.rm = TRUE))) %>%
          pivot_longer(!c(year, csi_lp), names_to = "career_cluster", values_to = "pct_have") %>%
          filter(!is.na(csi_lp)) %>%
          ungroup () %>%
          group_by(career_cluster) %>%
          summarise(access_disparity = pct_have[2] - pct_have[1]) 
      }
      
      else if (input$grouped2 == "Targeted Support and Improvement (TSI)") {
        
        data <- data %>%
          group_by(year, tsi_cu) %>%
          summarise(across(AAVC:TRAN, ~ mean(.x, na.rm = TRUE))) %>%
          pivot_longer(!c(year, tsi_cu), names_to = "career_cluster", values_to = "pct_have") %>%
          filter(!is.na(tsi_cu)) %>%
          ungroup () %>%
          group_by(career_cluster) %>%
          summarise(access_disparity = pct_have[1] - pct_have[2]) 
      }
      
      else {
        
        data <- data %>%
          group_by(year, majority_white) %>%
          summarise(across(AAVC:TRAN, ~ mean(.x, na.rm = TRUE))) %>%
          pivot_longer(!c(year, majority_white), names_to = "career_cluster", values_to = "pct_have") %>%
          filter(!is.na(majority_white)) %>%
          ungroup () %>%
          group_by(career_cluster) %>%
          summarise(access_disparity = pct_have[2] - pct_have[1]) 
      }
    }
    
    
    data
    
  })
  
  output$disparity_plot <- renderPlot({
    gg <- ggplot(disparity_data(), aes(x = career_cluster, y = access_disparity))
    
    if (input$grouped2 == "Title I Status") {
      gg <- gg +
        geom_bar(stat = "identity", position = "dodge", size = 0.3) + 
        annotate(geom="text", x = "ARCH" ,y=0.45, label="More prevelant in Non-Title I Schools", color=brewer.pal(n = 1, name = "Dark2")) +
        annotate(geom="text", x = "ARCH", y=-0.3, label="More prevelant in Title I Schools", color=brewer.pal(n = 1, name = "Dark2"))
    } 
    
    else if (input$grouped2 == "Comprehensive Support and Improvement (CSI)") {
      gg <- gg +
        geom_bar(stat = "identity", position = "dodge", size = 0.3)  +
        annotate(geom="text", x = "ARCH" ,y=0.4, label="More prevelant in Non-CSI Schools", color=brewer.pal(n = 1, name = "Dark2")) 
        #annotate(geom="text", x = "ARCH", y=-0., label="More prevelant in CSI Schools", color= brewer.pal(n = 1, name = "Dark2"))
    } 
    
    else if (input$grouped2 == "Targeted Support and Improvement (TSI)") {
      gg <- gg +
        geom_bar(stat = "identity", position = "dodge", size = 0.3) +
        annotate(geom="text", x = "ARCH" ,y=0.5, label="More prevelant in Non-TSI Schools", color= brewer.pal(n = 1, name = "Dark2")) +
        annotate(geom="text", x = "ARCH", y=-0.1, label="More prevelant in TSI Schools", color= brewer.pal(n = 1, name = "Dark2"))
    } 
    
    else if (input$grouped2 == "Majority White LEA") {
      gg <- gg +
        geom_bar(stat = "identity", position = "dodge", size = 0.3) +
        annotate(geom="text", x = "ARCH" ,y=0.2, label="More prevelant in Majority white LEAs",  color=brewer.pal(n = 1, name = "Dark2")) +
        annotate(geom="text", x = "ARCH", y=-0.2, label="More prevelant in Majority Non-white LEAs",  color=brewer.pal(n = 1, name = "Dark2"))
    } 
    
    else {
      gg <- gg +
        geom_bar(stat = "identity", position = "dodge", size = 0.3) 
    }
    
    gg +
      labs(title = "CTE Career Cluster Access Disparity",
           x = "Career Clusters",
           y = "Proportion Difference") +
      theme_minimal()
  })
  
  output$cte_table <- renderDT({
    filtered_data()
  })
  
  # Server logic for the career cluster tab
  cluster_data <- reactive({
    data <- concentrators
    
    if (input$grouped == "Title I Status") {
      data <- data %>%
        group_by(year, title_i) %>%
        summarise(across(AAVC:TRAN, ~ mean(.x, na.rm = TRUE)))  %>%
        pivot_longer(!c(year, title_i), names_to = "career_clusters", values_to = "avg_pct") %>%
        filter(!is.na(title_i))
    }
    
    
    else if (input$grouped == "Comprehensive Support and Improvement (CSI)") {
      data <- data %>%
        group_by(year, csi_lp) %>%
        summarise(across(AAVC:TRAN, ~ mean(.x, na.rm = TRUE)))  %>%
        pivot_longer(!c(year, csi_lp), names_to = "career_clusters", values_to = "avg_pct")%>%
        filter(!is.na(csi_lp))
    }
    
    else if (input$grouped == "Targeted Support and Improvement (TSI)") {
      data <- data %>%
        group_by(year, tsi_cu) %>%
        summarise(across(AAVC:TRAN, ~ mean(.x, na.rm = TRUE)))  %>%
        pivot_longer(!c(year, tsi_cu), names_to = "career_clusters", values_to = "avg_pct")%>%
        filter(!is.na(tsi_cu))
    }
    
    else if (input$grouped ==  "Majority White LEA") {
      data <- data %>%
        group_by(year, majority_white) %>%
        summarise(across(AAVC:TRAN, ~ mean(.x, na.rm = TRUE)))  %>%
        pivot_longer(!c(year, majority_white), names_to = "career_clusters", values_to = "avg_pct")%>%
        filter(!is.na(majority_white))
    }
    
    else {
      data <- data %>%
        group_by(year) %>%
        summarise(across(AAVC:TRAN, ~ mean(.x, na.rm = TRUE)))  %>%
        pivot_longer(!c(year), names_to = "career_clusters", values_to = "avg_pct")
    }
    
    data <- filter(data, year >= input$year[1] & year <= input$year[2])

    if (input$cluster != "All") {
      data <- filter(data, career_clusters == input$cluster)
    }
    
    data
  })
  
  output$cluster_participation <- renderPlot({
    
    gg2 <- ggplot(cluster_data(), aes(x = year, y = avg_pct, color = career_clusters)) 
  
    if (input$grouped == 'Title I Status') {
      # New facet label names for supp variable
      group.labs <- c("Not Title I", "Title I")
      names(group.labs) <- c("N", "Y")
      
      gg2 <- gg2 +
        facet_wrap(~title_i, labeller = labeller(title_i = group.labs))
    }
    
    else if (input$grouped == 'Comprehensive Support and Improvement (CSI)') {
      # New facet label names for supp variable
      group.labs <- c("Not CSI", "CSI")
      names(group.labs) <- c("N", "Y")
      
      gg2 <- gg2 +
        facet_wrap(~csi_lp, labeller = labeller(csi_lp = group.labs))
    }
    
    else if (input$grouped == 'Targeted Support and Improvement (TSI)') {
      # New facet label names for supp variable
      group.labs <- c("Not TSI", "TSI")
      names(group.labs) <- c("N", "Y")
      
      gg2 <- gg2 +
        facet_wrap(~tsi_cu, labeller = labeller(tsi_cu = group.labs))
    }
    
    else if (input$grouped == 'Majority White LEA') {
      # New facet label names for supp variable
      group.labs <- c("Majority White", "Majority Non-White")
      names(group.labs) <- c(TRUE, FALSE)
      
      gg2 <- gg2 +
        facet_wrap(~majority_white, labeller = labeller(majority_white = group.labs))
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
  
    
}

# Run the app
shinyApp(ui, server)