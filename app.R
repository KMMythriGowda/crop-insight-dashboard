library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(plotly)

# ── CONSTANTS ──────────────────────────────────────────────────────
PRODUCTION_UNIT <- 1e6
UNIT_NAME <- "Million Tonnes"

# ── THEME ──────────────────────────────────────────────────────────
agri_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background = element_rect(fill = "#f0f7ed", color = NA),
      panel.background = element_rect(fill = "#f0f7ed", color = NA),
      panel.grid.major = element_line(color = "#d4e4cf"),
      plot.title = element_text(face = "bold", color = "#1b5e20", size = 14),
      axis.title = element_text(color = "#2e7d32", face = "bold")
    )
}

# ── UI ─────────────────────────────────────────────────────────────
ui <- fluidPage(
  
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
    body { 
      background: linear-gradient(to right, #e8f5e9, #f1f8e9); 
      font-family: 'Segoe UI', sans-serif;
      margin: 0;
      padding: 0;
    }
    
    .welcome-box { 
      text-align: center; 
      padding: 80px 20px;
      animation: fadeIn 0.8s ease-in;
    }
    
    @keyframes fadeIn {
      from { opacity: 0; }
      to { opacity: 1; }
    }
    
    .upload-card {
      background: white; 
      padding: 30px; 
      border-radius: 15px;
      box-shadow: 0 6px 15px rgba(0,0,0,.2);
      max-width: 500px; 
      margin: auto;
      border-left: 5px solid #2e7d32;
    }
    
    .title-bar {
      background: linear-gradient(45deg, #2e7d32, #66bb6a);
      color: white; 
      padding: 20px;
      border-radius: 12px; 
      margin-bottom: 20px;
      text-align: center;
      box-shadow: 0 4px 8px rgba(0,0,0,.15);
    }
    
    .kpi-box {
      background: white; 
      padding: 10px 12px;
      border-radius: 10px;
      box-shadow: 0 3px 10px rgba(0,0,0,.15);
      text-align: center;
      border-top: 3px solid #2e7d32;
      transition: transform 0.3s ease;
    }
    
    .kpi-box:hover {
      transform: translateY(-5px);
      box-shadow: 0 6px 15px rgba(0,0,0,.2);
    }
    
    .kpi-value {
      font-size: 20px;
      font-weight: bold;
      color: #2e7d32;
      margin: 5px 0;
    }
    
    .kpi-label {
      font-size: 10px;
      color: #666;
      text-transform: uppercase;
    }
    
    .kpi-trend {
      font-size: 12px;
      font-weight: bold;
      margin-top: 3px;
    }
    
    .trend-up { color: #4caf50; }
    .trend-down { color: #f44336; }
    
    .filter-section {
      background: white;
      padding: 15px;
      border-radius: 12px;
      margin-bottom: 15px;
      box-shadow: 0 2px 5px rgba(0,0,0,.1);
    }
    
    .shiny-input-container {
      margin-bottom: 10px;
    }
    
    .nav-tabs .nav-link.active {
      background-color: #2e7d32;
      color: white;
      border-bottom: 3px solid #1b5e20;
    }
    
    .nav-tabs .nav-link {
      color: #2e7d32;
      border: none;
      border-bottom: 2px solid #ddd;
    }
    
    .export-buttons {
      margin-bottom: 15px;
      padding: 10px;
      text-align: right;
    }
    
    .btn-download {
      background: linear-gradient(45deg, #2e7d32, #66bb6a);
      color: white;
      border: none;
      border-radius: 8px;
      padding: 8px 16px;
      margin-left: 5px;
      cursor: pointer;
      font-weight: bold;
      transition: background 0.3s ease;
    }
    
    .btn-download:hover {
      background: linear-gradient(45deg, #1b5e20, #2e7d32);
    }
    
    .quality-indicator {
      padding: 8px 12px;
      border-radius: 6px;
      font-weight: bold;
      font-size: 12px;
      display: inline-block;
      margin-top: 10px;
    }
    
    .quality-good { background: #c8e6c9; color: #1b5e20; }
    .quality-fair { background: #ffe0b2; color: #e65100; }
    .quality-poor { background: #ffcdd2; color: #c62828; }
    
    .error-box {
      background: #ffcdd2;
      border-left: 4px solid #c62828;
      padding: 15px;
      border-radius: 8px;
      color: #c62828;
      font-weight: bold;
    }
    
    .success-box {
      background: #c8e6c9;
      border-left: 4px solid #2e7d32;
      padding: 15px;
      border-radius: 8px;
      color: #1b5e20;
      font-weight: bold;
    }
    
    .well {
      background: white !important;
      border-radius: 12px !important;
      box-shadow: 0 2px 8px rgba(0,0,0,.1) !important;
      border: none !important;
      padding: 15px !important;
    }
    "))
  ),
  
  uiOutput("main_ui")
)

# ── SERVER ─────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 1024^3)
  
  user_data <- reactiveVal(NULL)
  upload_message <- reactiveVal("")
  
  # ── MAIN UI ROUTER ──
  output$main_ui <- renderUI({
    
    if (is.null(user_data())) {
      
      # UPLOAD SCREEN
      div(class = "welcome-box",
          h1("🌾 Agricultural Crop Insight Dashboard", 
             style = "color: #2e7d32; margin-bottom: 10px;"),
          p("Upload your crop dataset to begin analysis", 
            style = "font-size: 16px; color: #555;"),
          
          div(class = "upload-card",
              fileInput("file", "📂 Upload CSV File", accept = ".csv"),
              actionButton("load", "Load Dataset", 
                           class = "btn btn-success", 
                           style = "width: 100%; background: #2e7d32; color: white; font-weight: bold;"),
              br(), br(),
              uiOutput("upload_message_ui")
          ),
          
          div(style = "margin-top: 30px; text-align: center; color: #666; font-size: 12px;",
              p("Expected columns: State, Crop, Season, Crop_Year, Production"))
      )
      
    } else {
      
      df <- user_data()
      
      # DASHBOARD SCREEN
      tagList(
        
        div(class = "title-bar", 
            h2("🌾 Agricultural Crop Insight Dashboard"),
            p("Year Range: ", min(df$Crop_Year), " - ", max(df$Crop_Year))),
        
        sidebarLayout(
          
          # ── SIDEBAR ──
          sidebarPanel(
            width = 2,
            
            h5("🔍 Filters", style = "color: #2e7d32; font-weight: bold; margin-top: 0;"),
            hr(style = "margin: 8px 0;"),
            
            selectInput("crop", "🌱 Crop", 
                        c("All", sort(unique(df$Crop)))),
            
            selectInput("season", "🌡 Season", 
                        c("All", sort(unique(df$Season)))),
            
            selectInput("state", "🗺 State", 
                        c("All", sort(unique(df$State)))),
            
            sliderInput("year", "📅 Year",
                        min = min(df$Crop_Year),
                        max = max(df$Crop_Year),
                        value = c(min(df$Crop_Year), max(df$Crop_Year)),
                        sep = ""),
            
            hr(style = "margin: 8px 0;"),
            
            h5("📥 Export", style = "color: #2e7d32; font-weight: bold;"),
            
            downloadButton("download_data", "Export Data",
                           style = "width: 100%; margin-bottom: 8px; background: linear-gradient(45deg, #2e7d32, #66bb6a); color: white; border: none; border-radius: 8px; font-weight: bold;"),
            br(),
            downloadButton("download_plot", "Export Chart",
                           style = "width: 100%; margin-top: 4px; background: linear-gradient(45deg, #2e7d32, #66bb6a); color: white; border: none; border-radius: 8px; font-weight: bold;")
          ),
          
          # ── MAIN PANEL ──
          mainPanel(
            width = 10,
            
            # KPI CARDS
            fluidRow(
              column(3, div(class = "kpi-box",
                            div(class = "kpi-label", "📊 Production"),
                            div(class = "kpi-value", textOutput("kpi_prod")),
                            div(class = "kpi-label", UNIT_NAME),
                            htmlOutput("kpi_prod_trend"))),
              
              column(3, div(class = "kpi-box",
                            div(class = "kpi-label", "📈 Total Records"),
                            div(class = "kpi-value", textOutput("kpi_rows")),
                            htmlOutput("data_quality"))),
              
              column(3, div(class = "kpi-box",
                            div(class = "kpi-label", "🌾 Unique Crops"),
                            div(class = "kpi-value", textOutput("kpi_crops")),
                            div(class = "kpi-label", "counted"))),
              
              column(3, div(class = "kpi-box",
                            div(class = "kpi-label", "🗺 States"),
                            div(class = "kpi-value", textOutput("kpi_states")),
                            div(class = "kpi-label", "covered")))
            ),
            
            br(),
            
            # TABS
            tabsetPanel(
              
              tabPanel("📈 Trends",
                       h3("Production Trends Over Time"),
                       plotlyOutput("linePlot_interactive", height = "400px"),
                       plotlyOutput("regPlot_interactive", height = "400px")),
              
              tabPanel("📊 Distribution",
                       h3("Crop & Production Distribution"),
                       plotOutput("barPlot", height = "400px"),
                       plotOutput("histPlot", height = "400px")),
              
              tabPanel("🗺 Geographic",
                       h3("State-wise Production"),
                       plotlyOutput("statePlot_interactive", height = "500px")),
              
              tabPanel("🔗 Cluster Analysis",
                       h3("Production Clusters by Crop & State"),
                       plotOutput("clusterPlot", height = "500px")),
              
              tabPanel("📉 Forecast",
                       h3("Production Forecast (3-Year)"),
                       plotlyOutput("forecastPlot", height = "500px")),
              
              tabPanel("📋 Summary",
                       h3("Dataset Summary Statistics"),
                       verbatimTextOutput("summary_stats")),
              
              tabPanel("🌱 Farmer Assistant",
                       br(),
                       h3("🌾 Seasonal Crop Recommendations"),
                       fluidRow(
                         column(6, selectInput("assist_season", "Select Season",
                                               choices = c("Kharif", "Rabi", "Summer"))),
                         column(6, actionButton("assist_btn", "Get Advice", 
                                                class = "btn btn-success", 
                                                style = "margin-top: 25px; width: 100%;"))
                       ),
                       br(),
                       div(id = "advice_output", h4("💡 Recommendations:"),
                           textOutput("assist_output")),
                       br(),
                       div(style = "background: #f0f7ed; padding: 15px; border-radius: 8px;",
                           h5("🌍 General Best Practices:"),
                           HTML("
                             <ul>
                               <li>Conduct soil testing before planting</li>
                               <li>Practice crop rotation</li>
                               <li>Use drip irrigation for water efficiency</li>
                               <li>Monitor weather patterns regularly</li>
                               <li>Maintain farm hygiene to prevent pests</li>
                             </ul>
                           "))
              ),
              
              tabPanel("🐛 Pest & Solutions",
                       br(),
                       h3("🐛 Pest Prediction & Management"),
                       fluidRow(
                         column(6, selectInput("pest_crop", "Select Crop",
                                               choices = sort(unique(df$Crop)))),
                         column(6, selectInput("pest_season", "Select Season",
                                               choices = c("Kharif", "Rabi", "Summer")))
                       ),
                       actionButton("predict", "Predict Pest Risk", 
                                    class = "btn btn-danger", style = "width: 100%;"),
                       br(), br(),
                       div(class = "success-box",
                           h4("🐛 Predicted Pest Issue:"), 
                           textOutput("pest_result")),
                       br(),
                       div(class = "success-box",
                           h4("💡 Management Solution:"), 
                           textOutput("pest_solution")),
                       br(),
                       div(style = "background: #e3f2fd; padding: 15px; border-radius: 8px;",
                           h5("📞 Need Expert Help?"),
                           p("Contact your nearest agricultural extension center for personalized assistance."))
              )
            )
          )
        )
      )
    }
  })
  
  # ── UPLOAD MESSAGE UI ──
  output$upload_message_ui <- renderUI({
    msg <- upload_message()
    if (msg == "") {
      return(NULL)
    } else if (grepl("Error|Invalid", msg)) {
      div(class = "error-box", msg)
    } else {
      div(class = "success-box", msg)
    }
  })
  
  # ── LOAD DATA WITH VALIDATION ──
  observeEvent(input$load, {
    
    req(input$file)
    
    df <- tryCatch({
      read.csv(input$file$datapath, stringsAsFactors = FALSE)
    }, error = function(e) {
      NULL
    })
    
    if (is.null(df)) {
      upload_message("❌ Error reading file. Please upload a valid CSV.")
      return()
    }
    
    # Normalize column names
    names(df) <- trimws(names(df))
    
    if ("State_Name" %in% names(df)) names(df)[names(df)=="State_Name"] <- "State"
    if ("state" %in% names(df)) names(df)[names(df)=="state"] <- "State"
    if ("crop_year" %in% names(df)) names(df)[names(df)=="crop_year"] <- "Crop_Year"
    if ("season" %in% names(df)) names(df)[names(df)=="season"] <- "Season"
    if ("crop" %in% names(df)) names(df)[names(df)=="crop"] <- "Crop"
    if ("production" %in% names(df)) names(df)[names(df)=="production"] <- "Production"
    
    # Validate required columns
    required_cols <- c("State", "Crop", "Season", "Crop_Year", "Production")
    
    if (!all(required_cols %in% names(df))) {
      missing <- setdiff(required_cols, names(df))
      upload_message(paste("❌ Invalid Dataset! Missing columns:", paste(missing, collapse = ", ")))
      return()
    }
    
    # Clean and validate data
    df <- df %>%
      mutate(
        State = trimws(State),
        Crop = trimws(Crop),
        Season = trimws(Season),
        Production = as.numeric(Production),
        Crop_Year = as.numeric(Crop_Year)
      ) %>%
      filter(!is.na(Production), !is.na(Crop_Year), Production >= 0)
    
    if (nrow(df) == 0) {
      upload_message("❌ Dataset has no valid data after cleaning.")
      return()
    }
    
    upload_message(paste("✅ Successfully loaded", nrow(df), "records!"))
    user_data(df)
  })
  
  # ── REACTIVE FILTER ──
  filtered <- reactive({
    df <- user_data()
    
    if (input$crop != "All") df <- df[df$Crop == input$crop, ]
    if (input$season != "All") df <- df[df$Season == input$season, ]
    if (input$state != "All") df <- df[df$State == input$state, ]
    
    df <- df[df$Crop_Year >= input$year[1] & df$Crop_Year <= input$year[2], ]
    df
  })
  
  # ── KPI METRICS ──
  output$kpi_prod <- renderText({
    round(sum(filtered()$Production) / PRODUCTION_UNIT, 2)
  })
  
  output$kpi_rows <- renderText({
    nrow(filtered())
  })
  
  output$kpi_crops <- renderText({
    n_distinct(filtered()$Crop)
  })
  
  output$kpi_states <- renderText({
    n_distinct(filtered()$State)
  })
  
  # ── TREND CALCULATION ──
  output$kpi_prod_trend <- renderUI({
    df <- filtered()
    
    if (nrow(df) == 0) {
      return(span(class = "kpi-trend", style = "color: #999;", "N/A"))
    }
    
    current_year <- max(df$Crop_Year)
    previous_year <- current_year - 1
    
    current_prod <- sum(df[df$Crop_Year == current_year, "Production"], na.rm = TRUE)
    previous_prod <- sum(df[df$Crop_Year == previous_year, "Production"], na.rm = TRUE)
    
    if (previous_prod == 0 || previous_year < min(df$Crop_Year)) {
      return(span(class = "kpi-trend", style = "color: #999;", "N/A"))
    }
    
    trend <- ((current_prod - previous_prod) / previous_prod) * 100
    
    if (trend > 0) {
      span(class = "kpi-trend trend-up", paste0("↑ +", round(trend, 1), "%"))
    } else {
      span(class = "kpi-trend trend-down", paste0("↓ ", round(trend, 1), "%"))
    }
  })
  
  # ── DATA QUALITY INDICATOR ──
  output$data_quality <- renderUI({
    df <- filtered()
    
    if (nrow(df) == 0) {
      return(div(class = "quality-indicator quality-poor", "No Data"))
    }
    
    missing <- sum(is.na(df$Production)) + sum(is.na(df$Crop_Year))
    total <- nrow(df) * 2
    quality <- round((1 - missing / total) * 100, 1)
    
    if (quality >= 95) {
      div(class = "quality-indicator quality-good", paste0("✓ Quality: ", quality, "%"))
    } else if (quality >= 80) {
      div(class = "quality-indicator quality-fair", paste0("⚠ Quality: ", quality, "%"))
    } else {
      div(class = "quality-indicator quality-poor", paste0("✗ Quality: ", quality, "%"))
    }
  })
  
  # ── INTERACTIVE PLOTS ──
  output$linePlot_interactive <- renderPlotly({
    df <- filtered() %>%
      group_by(Crop_Year) %>%
      summarise(Total = sum(Production) / PRODUCTION_UNIT, .groups = 'drop')
    
    plot_ly(df, x = ~Crop_Year, y = ~Total, type = 'scatter', 
            mode = 'lines+markers', name = 'Production',
            line = list(color = '#2e7d32', width = 3),
            marker = list(size = 8, color = '#66bb6a')) %>%
      layout(title = "Production Trend Over Years",
             xaxis = list(title = "Year"),
             yaxis = list(title = paste("Total Production", UNIT_NAME)),
             hovermode = 'x unified',
             template = "plotly_white")
  })
  
  output$regPlot_interactive <- renderPlotly({
    df <- filtered() %>%
      group_by(Crop_Year) %>%
      summarise(Total = sum(Production) / PRODUCTION_UNIT, .groups = 'drop')
    
    if (nrow(df) < 3) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "Need at least 3 data points for regression"))
    }
    
    model <- lm(Total ~ Crop_Year, data = df)
    
    plot_ly(df, x = ~Crop_Year, y = ~Total, type = 'scatter', mode = 'markers',
            name = 'Actual', marker = list(color = '#2e7d32', size = 8)) %>%
      add_trace(x = ~Crop_Year, y = fitted(model), mode = 'lines', 
                name = 'Linear Trend', line = list(color = '#66bb6a', dash = 'dash')) %>%
      layout(title = "Production Trend with Linear Regression",
             xaxis = list(title = "Year"),
             yaxis = list(title = paste("Production", UNIT_NAME)),
             template = "plotly_white")
  })
  
  output$statePlot_interactive <- renderPlotly({
    df <- filtered() %>%
      group_by(State) %>%
      summarise(Total = sum(Production) / PRODUCTION_UNIT, .groups = 'drop') %>%
      arrange(desc(Total))
    
    plot_ly(df, x = ~Total, y = ~State, type = 'bar', orientation = 'h',
            marker = list(color = '#2e7d32', line = list(color = '#1b5e20', width = 2))) %>%
      layout(title = "State-wise Production",
             xaxis = list(title = paste("Production", UNIT_NAME)),
             yaxis = list(title = "State"),
             margin = list(l = 150),
             template = "plotly_white")
  })
  
  # ── STATIC PLOTS ──
  output$barPlot <- renderPlot({
    df <- filtered() %>%
      group_by(Crop) %>%
      summarise(Total = sum(Production) / PRODUCTION_UNIT, .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(10)
    
    ggplot(df, aes(reorder(Crop, Total), Total)) +
      geom_col(fill = "#66bb6a", color = "#1b5e20", size = 0.8) +
      coord_flip() +
      labs(title = "Top 10 Crops by Production",
           x = "Crop", y = paste("Production", UNIT_NAME)) +
      agri_theme()
  })
  
  output$histPlot <- renderPlot({
    ggplot(filtered(), aes(Production / PRODUCTION_UNIT)) +
      geom_histogram(fill = "#81c784", color = "#1b5e20", bins = 30) +
      labs(title = "Production Distribution",
           x = paste("Production", UNIT_NAME), y = "Frequency") +
      agri_theme()
  })
  
  output$clusterPlot <- renderPlot({
    
    df <- filtered() %>%
      group_by(State, Crop) %>%
      summarise(Production = sum(Production) / PRODUCTION_UNIT, .groups = 'drop')
    
    if (nrow(df) < 3) {
      plot.new()
      text(0.5, 0.5, "Not enough data for clustering\nTry selecting more crops/states",
           cex = 1.2, col = "red")
      return()
    }
    
    k <- min(3, nrow(df) - 1)
    km <- kmeans(scale(df$Production), centers = k)
    df$Cluster <- factor(km$cluster)
    
    ggplot(df, aes(reorder(Crop, Production), Production, color = Cluster)) +
      geom_point(size = 4, alpha = 0.7) +
      coord_flip() +
      labs(title = "Production Clusters",
           x = "Crop", y = paste("Production", UNIT_NAME)) +
      scale_color_manual(values = c("#2e7d32", "#66bb6a", "#81c784")) +
      agri_theme()
  })
  
  # ── FORECAST PLOT ──
  output$forecastPlot <- renderPlotly({
    
    df <- filtered() %>%
      group_by(Crop_Year) %>%
      summarise(Total = sum(Production) / PRODUCTION_UNIT, .groups = 'drop') %>%
      arrange(Crop_Year)
    
    if (nrow(df) < 3) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "Need at least 3 years of data for forecasting"))
    }
    
    model <- lm(Total ~ Crop_Year, data = df)
    
    future_years <- data.frame(Crop_Year = seq(max(df$Crop_Year) + 1, max(df$Crop_Year) + 3))
    forecast <- predict(model, future_years, se.fit = TRUE)
    
    combined <- rbind(
      data.frame(Year = df$Crop_Year, Value = df$Total, Type = "Historical"),
      data.frame(Year = future_years$Crop_Year, Value = forecast$fit, Type = "Forecast")
    )
    
    plot_ly() %>%
      add_trace(data = df, x = ~Crop_Year, y = ~Total, type = 'scatter',
                mode = 'lines+markers', name = 'Historical',
                line = list(color = '#2e7d32', width = 3)) %>%
      add_trace(data = future_years, x = ~Crop_Year, y = forecast$fit, 
                type = 'scatter', mode = 'lines+markers', name = 'Forecast',
                line = list(color = '#66bb6a', dash = 'dash', width = 2)) %>%
      add_trace(data = future_years, x = ~Crop_Year,
                y = forecast$fit + 1.96 * forecast$se.fit,
                fill = 'tonexty', mode = 'lines',
                line = list(color = 'rgba(0,0,0,0)'), showlegend = FALSE) %>%
      add_trace(data = future_years, x = ~Crop_Year,
                y = forecast$fit - 1.96 * forecast$se.fit,
                fill = 'tonexty', mode = 'lines', name = '95% CI',
                line = list(color = 'rgba(0,0,0,0)'),
                fillcolor = 'rgba(102, 187, 106, 0.2)') %>%
      layout(title = "3-Year Production Forecast",
             xaxis = list(title = "Year"),
             yaxis = list(title = paste("Production", UNIT_NAME)),
             hovermode = 'x unified',
             template = "plotly_white")
  })
  
  # ── SUMMARY STATISTICS ──
  output$summary_stats <- renderText({
    df <- filtered()
    
    if (nrow(df) == 0) {
      return("No data to display. Please adjust filters.")
    }
    
    paste(
      "=== FILTERED DATASET SUMMARY ===\n\n",
      "Total Records:", nrow(df), "\n",
      "Total States:", n_distinct(df$State), "\n",
      "Total Crops:", n_distinct(df$Crop), "\n",
      "Total Seasons:", n_distinct(df$Season), "\n",
      "Year Range:", min(df$Crop_Year), "-", max(df$Crop_Year), "\n\n",
      
      "=== PRODUCTION STATISTICS ===\n",
      "Total Production:", round(sum(df$Production) / PRODUCTION_UNIT, 2), UNIT_NAME, "\n",
      "Average Production:", round(mean(df$Production) / PRODUCTION_UNIT, 4), UNIT_NAME, "\n",
      "Median Production:", round(median(df$Production) / PRODUCTION_UNIT, 4), UNIT_NAME, "\n",
      "Max Production:", round(max(df$Production) / PRODUCTION_UNIT, 4), UNIT_NAME, "\n",
      "Min Production:", round(min(df$Production) / PRODUCTION_UNIT, 4), UNIT_NAME, "\n",
      "Std Dev:", round(sd(df$Production) / PRODUCTION_UNIT, 4), UNIT_NAME, "\n\n",
      
      "=== TOP 5 CROPS BY PRODUCTION ===\n",
      paste(
        df %>%
          group_by(Crop) %>%
          summarise(Total = sum(Production) / PRODUCTION_UNIT, .groups = 'drop') %>%
          arrange(desc(Total)) %>%
          head(5) %>%
          {paste(paste0(1:nrow(.), ". ", .$Crop, ": ", round(.$Total, 2), " ", UNIT_NAME), 
                 collapse = "\n")},
        sep = ""
      )
    )
  })
  
  # ── FARMER ASSISTANT ──
  observeEvent(input$assist_btn, {
    
    season <- input$assist_season
    
    advice <- switch(season,
                     "Kharif" = "🌾 KHARIF SEASON (June-October) RECOMMENDATIONS:\n\n
✓ Main Crops: Rice, Maize, Cotton, Groundnut\n
✓ Rainfall: Expect 60-70% of annual rainfall\n
✓ Soil Prep: Ensure good drainage & organic matter\n
✓ Pest Alert: Monitor for stem borers & leafhoppers\n
✓ Irrigation: Depends on monsoon - maintain backup sources\n
✓ Fertilizer: Use split applications with monsoon rains",
                     
                     "Rabi" = "❄️ RABI SEASON (October-March) RECOMMENDATIONS:\n\n
✓ Main Crops: Wheat, Mustard, Pulse, Barley\n
✓ Temperature: Cool climate ideal for growth\n
✓ Soil Prep: Use residual moisture from monsoon\n
✓ Irrigation: Plan 3-4 irrigation cycles\n
✓ Pest Alert: Watch for aphids & rust disease\n
✓ Fertilizer: Pre-monsoon application recommended",
                     
                     "Summer" = "☀️ SUMMER SEASON (March-May) RECOMMENDATIONS:\n\n
✓ Main Crops: Vegetables, Watermelon, Cucumber\n
✓ Temperature: High heat - use shade nets for sensitive crops\n
✓ Soil Moisture: Critical - drip irrigation essential\n
✓ Mulching: Apply heavily to retain moisture\n
✓ Pest Alert: Whitefly & spider mites active\n
✓ Water Management: Schedule early morning irrigation"
    )
    
    output$assist_output <- renderText(advice)
  })
  
  # ── SMART PEST PREDICTION ──
  observeEvent(input$predict, {
    
    crop <- tolower(trimws(input$pest_crop))
    season <- input$pest_season
    
    pest_db <- data.frame(
      crop = c("rice", "rice", "wheat", "wheat", "cotton", "cotton", 
               "maize", "maize", "sugarcane", "sugarcane"),
      season = c("Kharif", "Rabi", "Rabi", "Kharif", "Kharif", "Rabi",
                 "Kharif", "Rabi", "Kharif", "Rabi"),
      pest = c("Stem Borer", "Blast Disease", "Aphids", "Cutworm", 
               "Bollworm", "Whitefly", "Fall Armyworm", "Shoot Fly",
               "Early Shoot Borer", "Pyrilla"),
      solution = c("Pheromone traps & Bt spray", "Resistant varieties & Fungicide",
                   "Organic neem spray", "Light traps & manual removal",
                   "Bt cotton & field monitoring", "Yellow sticky traps",
                   "Organic pesticide application", "Biopesticides",
                   "Biological control agents", "Community management"),
      risk_level = c("HIGH", "MEDIUM", "MEDIUM", "LOW",
                     "HIGH", "MEDIUM", "HIGH", "MEDIUM",
                     "MEDIUM", "LOW")
    )
    
    match <- pest_db %>%
      filter(tolower(crop) == crop, season == input$pest_season)
    
    if (nrow(match) > 0) {
      row <- match[1, ]
      
      output$pest_result <- renderText({
        paste0(row$pest, " (", row$risk_level, " Risk)")
      })
      output$pest_solution <- renderText(row$solution)
    } else {
      output$pest_result <- renderText("Common Pests (Monitor crop regularly)")
      output$pest_solution <- renderText("Use Integrated Pest Management (IPM) approach:\n• Regular scouting\n• Biological controls\n• Organic pesticides if needed")
    }
  })
  
  # ── DOWNLOAD HANDLERS ──
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("crop_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered(), file, row.names = FALSE)
    }
  )
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("dashboard_chart_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1200, height = 800, res = 100)
      
      df <- filtered() %>%
        group_by(Crop) %>%
        summarise(Total = sum(Production) / PRODUCTION_UNIT, .groups = 'drop') %>%
        arrange(desc(Total)) %>%
        head(10)
      
      print(ggplot(df, aes(reorder(Crop, Total), Total)) +
              geom_col(fill = "#66bb6a", color = "#1b5e20", size = 0.8) +
              coord_flip() +
              labs(title = "Top 10 Crops by Production",
                   x = "Crop", y = paste("Production", UNIT_NAME)) +
              agri_theme())
      
      dev.off()
    }
  )
}

# ── RUN APP ────────────────────────────────────────────────────────
shinyApp(ui, server)