## app.R ##
library(shiny) # ShinyApp
library(readxl) # Read Excel
library(shinydashboard) # Sidebar
library(reshape2)
library(tidyverse)  # data manipulation
library(tidyr)
library(ggplot2)  
library(GGally)
library(cluster)    # clustering algorithms
library(NbClust)
library(factoextra)
library(stringr)
library(bestNormalize)

# Read Excel
readExcel <- function(inFile) {
  if(is.null(inFile))
    return(NULL)
  file.rename(inFile$datapath,
              paste(inFile$datapath, ".xlsx", sep=""))
  df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
}

ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Clustering"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table", tabName = "Table", icon = icon("table")),
      menuItem("Describe", tabName = "Describe", icon = icon("flask")),
      menuItem("Normalize", tabName = "Normalize", icon = icon("wrench")),
      menuItem("Optimum Cluster", tabName = "Optimum", icon = icon("dashboard")),
      menuItem("Visualize", tabName = "Visualize", icon = icon("line-chart"))
    )
  ),
  ## Body content
  dashboardBody(
    tags$script(HTML("$('body').addClass('fixed');")), # fixed top
    tabItems(
      # First tab content
      tabItem(tabName = "Table",
        fluidRow(
          box(
            title = "import Data", status = "danger", solidHeader = TRUE,
            collapsible = TRUE, width = 12,
            fileInput('file1', 'Choose xlsx file',
                      accept = c(".xlsx"))
          ),
          box(
            title = "Output", status = "danger", solidHeader = TRUE,
            collapsible = TRUE, width = 12,
            dataTableOutput('contents')
          )
        )
      ),
      
      # Second tab content
      tabItem(tabName = "Describe",
        fluidRow(
          box(
            title = "Distribution", status = "danger", solidHeader = TRUE,
            collapsible = TRUE, width = 12,
            plotOutput("distribusi")
          ),
          box(
            title = "Box Plot", status = "danger", solidHeader = TRUE,
            collapsible = TRUE, width = 12,
            plotOutput("boxplot")
          )
        )
      ),
      # third tab content
      tabItem(tabName = "Normalize",
        fluidRow(
          box(
            title = "Best Normalize", status = "danger", solidHeader = TRUE,
            collapsible = TRUE, width = 12,
            verbatimTextOutput("bestNormalize")
          ),
          box(
            title = "Distribution", status = "danger", solidHeader = TRUE,
            collapsible = TRUE, width = 12,
            plotOutput("distribusiAfter")
          ),
          box(
            title = "Box Plot", status = "danger", solidHeader = TRUE,
            collapsible = TRUE, width = 12,
            plotOutput("boxplotAfter")
          )
        )
      ),
      # fourth tab content
      tabItem(tabName = "Optimum",
        fluidRow(
          box(
            title = "Method", status = "danger", solidHeader = TRUE,
            collapsible = TRUE, width = 12,
            selectInput("Method", "Choose a Method:",
                        list("silhouette", "wss", "gap_stat"), selected = NULL)
          ),
          box(
            title = "Cluster", status = "danger", solidHeader = TRUE,
            collapsible = TRUE, width = 12,
            plotOutput("methode")
          )
        )
      ),
      # fifth tab content
      tabItem(tabName = "Visualize",
        fluidRow(
          box(
            title = "Cluster", status = "danger", solidHeader = TRUE,
            collapsible = TRUE, width = 12,
            sliderInput("Number", label = ("Number of Cluster"), min = 1, 
                        max = 20, value = 2)
          ),
          box(
            title = "Cluster", status = "danger", solidHeader = TRUE,
            collapsible = TRUE, width = 12,
            plotOutput("Visual")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  output$contents <- renderDataTable({
    readExcel(input$file1)
  })
    
    output$distribusi <- renderPlot({
      inFile <- input$file1
      if(is.null(inFile))
        return(NULL)
      df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
      
      df <- column_to_rownames(df, var = "Kota") # cari otomatis
      df <- as.data.frame(df)
      
      df_long <- df %>%                          # Apply pivot_longer function
        pivot_longer(colnames(df)) %>% 
        as.data.frame()
      
      df_density <- ggplot(df_long, aes(x = value)) +    # Draw each column as density
        geom_density() + 
        facet_wrap(~name, labeller = labeller(name = label_wrap_gen(width = 20)))
      df_density
  })
    
    output$boxplot <- renderPlot({
      inFile <- input$file1
      if(is.null(inFile))
        return(NULL)
      df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
      
      df <- column_to_rownames(df, var = "Kota")
      df <- as.data.frame(df)
      
      df_long <- df %>%                          # Apply pivot_longer function
        pivot_longer(colnames(df)) %>% 
        as.data.frame()
      
      df_boxplot <- ggplot(df_long, aes(x = value)) +    # Draw each column as density
        geom_boxplot() + 
        facet_wrap(~name, labeller = labeller(name = label_wrap_gen(width = 20)))
      df_boxplot
    })
    
    output$bestNormalize <- renderPrint({
      inFile <- input$file1
      if(is.null(inFile))
        return(NULL)
      df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
      
      df <- column_to_rownames(df, var = "Kota")
      df <- as.data.frame(df)
      
      df_long <- df %>%                          # Apply pivot_longer function
        pivot_longer(colnames(df)) %>% 
        as.data.frame()
      
      normalized = df
      for (c in colnames(df)){
        normalized_obj <- orderNorm(df[, c], loo=FALSE)
        normalized[, c] <- normalized_obj$x.t
      }
      normalized_obj
    })
    
    output$distribusiAfter <- renderPlot({
      inFile <- input$file1
      if(is.null(inFile))
        return(NULL)
      df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
      
      df <- column_to_rownames(df, var = "Kota")
      df <- as.data.frame(df)
      
      df_long <- df %>%                          # Apply pivot_longer function
        pivot_longer(colnames(df)) %>% 
        as.data.frame()
      
      normalized = df
      for (c in colnames(df)){
        normalized_obj <- bestNormalize(df[, c], loo=TRUE)
        normalized[, c] <- normalized_obj$x.t
      }
      
      normalized_long <- normalized %>% 
        pivot_longer(colnames(normalized)) %>% 
        as.data.frame()
      
      normalized_density <- ggplot(normalized_long, aes(x = value)) + geom_density() + facet_wrap(~name, labeller = labeller(name = label_wrap_gen(width = 20)))
      normalized_density
    })
    
    output$boxplotAfter <- renderPlot({
      inFile <- input$file1
      if(is.null(inFile))
        return(NULL)
      df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
      
      df <- column_to_rownames(df, var = "Kota")
      df <- as.data.frame(df)
      
      df_long <- df %>%                          # Apply pivot_longer function
        pivot_longer(colnames(df)) %>% 
        as.data.frame()
      
      normalized = df
      for (c in colnames(df)){
        normalized_obj <- bestNormalize(df[, c], loo=TRUE)
        normalized[, c] <- normalized_obj$x.t
      }
      
      normalized_long <- normalized %>% 
        pivot_longer(colnames(normalized)) %>% 
        as.data.frame()
      
      normalized_density <- ggplot(normalized_long, aes(x = value)) + geom_boxplot() + facet_wrap(~name, labeller = labeller(name = label_wrap_gen(width = 20)))
      normalized_density
    })
    
    output$methode <- renderPlot({
      inFile <- input$file1
      if(is.null(inFile))
        return(NULL)
      df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
      
      df <- column_to_rownames(df, var = "Kota")
      df <- as.data.frame(df)
      
      normalized = df
      for (c in colnames(df)){
        normalized_obj <- bestNormalize(df[, c], loo=TRUE)
        normalized[, c] <- normalized_obj$x.t
      }
      
      fviz_nbclust(df, kmeans, method = input$Method, k.max = 20)
    })
    
    output$Visual <- renderPlot({
      inFile <- input$file1
      if(is.null(inFile))
        return(NULL)
      df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
      
      df <- column_to_rownames(df, var = "Kota")
      df <- as.data.frame(df)
      
      normalized = df
      for (c in colnames(df)){
        normalized_obj <- bestNormalize(df[, c], loo=TRUE)
        normalized[, c] <- normalized_obj$x.t
      }
      
      k2 <- kmeans(normalized, centers=input$Number, nstart = 100)
      
      fviz_cluster(k2, data = normalized, repel=TRUE)
    })
}

shinyApp(ui, server)
