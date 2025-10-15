# --- Load libraries ---
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(tidyverse)
library(lubridate)
library(scales)
library(caret)
library(randomForest)

# --- Load dataset ---
data <- read.csv("india_startups2.csv", check.names = FALSE, stringsAsFactors = FALSE)
colnames(data) <- str_replace_all(colnames(data), "[^[:alnum:]_]", "_")
data <- data %>% mutate(across(everything(), ~ na_if(., ""))) %>% drop_na()

# --- Clean Funding Amount ---
funding_col <- grep("Funding", names(data), value = TRUE)[1]
data[[funding_col]] <- gsub("[$,]", "", as.character(data[[funding_col]]))
data[[funding_col]] <- as.numeric(data[[funding_col]])

# --- Convert Date ---
if("Last_Funding_Date" %in% names(data)) {
  data$Last_Funding_Date <- parse_date_time(data$Last_Funding_Date, orders = c("dmy", "ymd", "my", "bY"))
  data$Months_Since_Last_Funding <- as.numeric(floor(interval(data$Last_Funding_Date, Sys.Date()) / months(1)))
  data$Months_Since_Last_Funding[is.na(data$Months_Since_Last_Funding)] <- median(data$Months_Since_Last_Funding, na.rm = TRUE)
}

# --- Load model artifacts ---
rf_model <- readRDS("model.rds")
dummies <- readRDS("dummyvars.rds")
training_meta <- readRDS("training_meta.rds")

# --- UI ---
ui <- dashboardPage(
  dashboardHeader(title = "India Startup Funding Explorer & Predictor"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Explore Data", tabName = "explore", icon = icon("chart-bar")),
      menuItem("Predict Funding", tabName = "predict", icon = icon("bolt")),
      menuItem("Social Media and News", tabName = "social", icon = icon("newspaper")),
      menuItem("View Data Table", tabName = "table", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      # --- Explore Tab ---
      tabItem(tabName = "explore",
              fluidRow(
                box(title = "Top 15 Industries by Startup Count", width = 6, plotlyOutput("industry_chart")),
                box(title = "Funding Type Distribution", width = 6, plotlyOutput("funding_type_chart"))
              ),
              fluidRow(
                box(title = "Funding Amount Distribution", width = 12, plotlyOutput("funding_hist"))
              ),
              fluidRow(
                box(title = "Top 10 Industries by Average Funding", width = 12, plotlyOutput("avg_funding_chart"))
              )
      ),
      
      # --- Predict Tab ---
      tabItem(tabName = "predict",
              fluidRow(
                box(title = "Input Startup Details", width = 4, uiOutput("predict_ui"), 
                    actionButton("go_pred", "Predict", class = "btn-success")),
                box(title = "Predicted Funding (USD)", width = 8, 
                    verbatimTextOutput("prediction"), 
                    plotOutput("importance_plot", height = "300px"))
              )
      ),
      
      # --- Social Media and News Tab ---
      tabItem(tabName = "social",
              fluidRow(
                box(title = "India ranks 3rd globally in fintech startup funding", width = 12, 
                    status = "info", solidHeader = TRUE,
                    HTML("
                      <p><strong>Bengaluru</strong> continued to lead India’s fintech funding landscape, accounting for 55% of total funding, followed by Mumbai at 14%.</p>
                      <p>India’s fintech sector raised $889 million in the first half of 2025, a 5% drop from $936 million in the same period last year and a 26% decline from $1.2 billion in H2 2024.</p>
                      <p>Despite fluctuations, India ranked third globally, trailing only the United States and the United Kingdom.</p>
                      <p>Source: <a href='https://www.fortuneindia.com/business-news/india-ranks-3rd-globally-in-fintech-startup-funding-tracxn/124599' target='_blank'>Fortune India</a></p>
                    ")
                ),
                box(title = "Reflections on the First Decade of Startup India", width = 12, status = "primary", solidHeader = TRUE,
                    HTML("
                      <p>The startup ecosystem in India has seen a remarkable transformation over the past decade, driven by innovation, capital inflow, and government support. The ‘Startup India’ initiative, launched in 2016 by DPIIT, marked a pivotal moment by offering tax benefits, funding opportunities, and regulatory relief.</p>
                      <p>As of January 2025, India hosts over 159,000 startups, making it the third largest startup ecosystem globally. In terms of unicorns—privately-held startups valued at over $1 billion—India also ranks 3rd.</p>
                      <p>Source: <a href='https://www.orfonline.org/research/reflections-on-the-first-decade-of-startup-india' target='_blank'>Observer Research Foundation (ORF)</a></p>
                    ")
                ),
                box(title = "Wealth Management Platform Dezerv Raises Rs 350 Crore", width = 12, status = "warning", solidHeader = TRUE,
                    HTML("
                      <p>Dezerv currently manages over Rs 14,000 crore in assets under management (AUM) across PMS, AIFs, and distribution assets. Its investment app, used by over 5 lakh users, tracks Rs 2 lakh crore in assets.</p>
                      <p>Founded by Sandeep Jethwani, Sahil Contractor, and Vaibhav Porwal, all former IIFL Wealth executives, Dezerv operates across 200 cities through offices in Mumbai, Delhi, Bengaluru, Hyderabad, and Pune.</p>
                      <p>The startup plans to leverage India’s account-aggregator framework and expand its One Model platform to include bonds, REITs, InvITs, and precious metals by year-end.</p>
                      <p>Source: <a href='https://indianstartupnews.com/funding/wealth-management-platform-dezerv-raises-rs-350-crore-led-by-premji-invest-and-accels-global-growth-fund-10557796' target='_blank'>Indian Startup News</a></p>
                    ")
                )
              )
      ),
      
      # --- Data Table Tab ---
      tabItem(tabName = "table",
              DTOutput("data_table"))
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  # --- Visualization Outputs ---
  output$industry_chart <- renderPlotly({
    ind <- data %>% count(Industry, name = "Startups") %>% arrange(desc(Startups)) %>% slice_head(n = 15)
    ggplotly(ggplot(ind, aes(x = reorder(Industry, Startups), y = Startups)) + 
               geom_col(fill = "#2c7fb8") + coord_flip() + theme_minimal())
  })
  
  output$funding_type_chart <- renderPlotly({
    ggplotly(ggplot(data, aes(x = Funding_Type)) + 
               geom_bar(fill = "#ff7f0e") + 
               theme_minimal() + 
               theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  })
  
  output$funding_hist <- renderPlotly({
    df <- data.frame(Funding = data[[funding_col]])
    ggplotly(ggplot(df, aes(Funding)) + 
               geom_histogram(fill = "#1f77b4", bins = 30) + 
               scale_x_continuous(labels = scales::comma) + theme_minimal())
  })
  
  output$avg_funding_chart <- renderPlotly({
    df <- data %>% group_by(Industry) %>% summarise(Average = mean(.data[[funding_col]], na.rm = TRUE)) %>% 
      arrange(desc(Average)) %>% slice_head(n = 10)
    ggplotly(ggplot(df, aes(x = reorder(Industry, Average), y = Average)) + 
               geom_col(fill = "#2ca02c") + coord_flip() + 
               scale_y_continuous(labels = scales::comma) + theme_minimal())
  })
  
  output$data_table <- renderDT({
    datatable(data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # --- Prediction UI ---
  output$predict_ui <- renderUI({
    req(training_meta)
    ui_list <- list()
    for(cat in training_meta$categorical)
      ui_list[[cat]] <- selectInput(paste0("in_", cat), cat, choices = training_meta$levels[[cat]])
    for(num in training_meta$numeric)
      ui_list[[num]] <- numericInput(paste0("in_", num), num, value = median(data[[num]], na.rm = TRUE))
    do.call(tagList, ui_list)
  })
  
  # --- Prediction Logic ---
  observeEvent(input$go_pred, {
    new_obs <- list()
    for(cat in training_meta$categorical)
      new_obs[[cat]] <- input[[paste0("in_", cat)]]
    for(num in training_meta$numeric)
      new_obs[[num]] <- input[[paste0("in_", num)]]
    
    new_df <- as.data.frame(new_obs)
    new_df <- predict(dummies, newdata = new_df) %>% as.data.frame()
    pred_log <- predict(rf_model, newdata = new_df)
    pred_amount <- expm1(pred_log)
    output$prediction <- renderText({
      paste0("$", formatC(round(pred_amount,0), format="d", big.mark=","))
    })
  })
  
  # --- Feature Importance ---
  output$importance_plot <- renderPlot({
    imp_df <- NULL
    
    if ("randomForest" %in% class(rf_model)) {
      imp <- randomForest::importance(rf_model)
      imp_df <- data.frame(Feature = rownames(imp), Importance = imp[,1])
    } else if ("ranger" %in% class(rf_model)) {
      imp <- rf_model$variable.importance
      imp_df <- data.frame(Feature = names(imp), Importance = as.numeric(imp))
    } else if ("train" %in% class(rf_model)) {
      imp <- caret::varImp(rf_model)$importance
      if(ncol(imp) == 1) {
        imp_df <- data.frame(Feature = rownames(imp), Importance = imp[,1])
      } else {
        imp_df <- data.frame(Feature = rownames(imp), Importance = rowMeans(imp))
      }
    }
    
    if (!is.null(imp_df)) {
      imp_df <- imp_df %>% arrange(desc(Importance)) %>% slice_head(n = 10)
      ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
        geom_col(fill="#ff7f0e") + coord_flip() + theme_minimal() + 
        labs(x="", y="Feature Importance")
    } else {
      plot.new(); text(0.5, 0.5, "Feature importance not available", cex = 1.2)
    }
  })
}

# --- Run App ---
shinyApp(ui, server)
setwd("C:/Users/Saket/OneDrive/Desktop/pdsdeploy")
