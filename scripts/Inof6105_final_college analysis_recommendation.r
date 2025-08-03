# US Colleges and Universities Analysis and Recommendation System
# Part 1: Comprehensive Data Analysis
# Part 2: Interactive Recommendation System

# Load required packages
library(tidyverse)      # Data manipulation and visualization
library(readxl)         # Reading Excel files
library(plotly)         # Interactive visualizations
library(cluster)        # Clustering algorithms
library(factoextra)     # Visualization of cluster analysis
library(corrplot)       # Correlation visualization
library(viridis)        # Better color palettes
library(car)            # For regression diagnostics
library(leaps)          # For stepwise regression
library(caret)          # For model validation
library(maps)           # For map data
library(shiny)          # For interactive application
library(DT)             # For interactive data tables

#====================================================================
# PART 1: COMPREHENSIVE DATA ANALYSIS
#====================================================================

# 1. DATA IMPORT AND CLEANING -------------------------------------------------

# Read the Excel file
colleges <- read_excel("/data/us_colleges_and_universities.xlsx")

# Convert columns to appropriate types and clean the data
colleges_clean <- colleges %>%
  # Convert enrollment and employee numbers to numeric
  mutate(
    TOT_ENROLL = as.numeric(ifelse(TOT_ENROLL == "-999", NA, TOT_ENROLL)),
    FT_ENROLL = as.numeric(ifelse(FT_ENROLL == "-999", NA, FT_ENROLL)),
    PT_ENROLL = as.numeric(ifelse(PT_ENROLL == "-999", NA, PT_ENROLL)),
    TOT_EMP = as.numeric(ifelse(TOT_EMP == "-999", NA, TOT_EMP)),
    DORM_CAP = as.numeric(ifelse(DORM_CAP == "-999", NA, DORM_CAP)),
    LATITUDE = as.numeric(LATITUDE),
    LONGITUDE = as.numeric(LONGITUDE),
    
    # Create factor variables
    TYPE = factor(TYPE, 
                  levels = c("1", "2", "3", "-3"),
                  labels = c("Public", "Private Non-Profit", 
                             "Private For-Profit", "Other")),
    
    INST_SIZE = factor(INST_SIZE,
                      levels = c("1", "2", "3", "4", "5", "-1", "-2"),
                      labels = c("< 1,000", "1,000-4,999", "5,000-9,999", 
                                 "10,000-19,999", "≥ 20,000", "Not reported", "Not applicable")),
    
    HOUSING = factor(HOUSING,
                    levels = c("1", "2", "-1", "-2"),
                    labels = c("Yes", "No", "Not reported", "Not applicable")),
    
    # Create urbanicity classification
    URBANICITY = case_when(
      LOCALE %in% c("11", "12", "13") ~ "City",
      LOCALE %in% c("21", "22", "23") ~ "Suburb",
      LOCALE %in% c("31", "32", "33") ~ "Town",
      LOCALE %in% c("41", "42", "43") ~ "Rural",
      TRUE ~ "Unknown"
    ),
    
    # Create region variable
    REGION = case_when(
      STATE %in% c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "PA") ~ "Northeast",
      STATE %in% c("OH", "MI", "IN", "IL", "WI", "MN", "IA", "MO", "ND", "SD", "NE", "KS") ~ "Midwest",
      STATE %in% c("DE", "MD", "DC", "VA", "WV", "NC", "SC", "GA", "FL", "KY", "TN", "AL", "MS", "AR", "LA", "OK", "TX") ~ "South",
      STATE %in% c("MT", "ID", "WY", "CO", "NM", "AZ", "UT", "NV", "WA", "OR", "CA", "AK", "HI") ~ "West",
      TRUE ~ "Other"
    ),
    
    # Calculate student-to-employee ratio
    STUDENT_EMP_RATIO = ifelse(!is.na(TOT_ENROLL) & !is.na(TOT_EMP) & TOT_EMP > 0,
                          TOT_ENROLL / TOT_EMP, NA),
    
    # Calculate percentage of dormitory capacity relative to enrollment
    DORM_PERCENT = ifelse(!is.na(DORM_CAP) & !is.na(TOT_ENROLL) & TOT_ENROLL > 0,
                          (DORM_CAP / TOT_ENROLL) * 100, NA),
    
    # Calculate full-time to part-time student ratio
    FT_PT_RATIO = ifelse(!is.na(FT_ENROLL) & !is.na(PT_ENROLL) & PT_ENROLL > 0,
                         FT_ENROLL / PT_ENROLL, NA),
                         
    # Create binary housing variable and log enrollment for later use
    has_housing = ifelse(HOUSING == "Yes", 1, 0),
    log_enrollment = log10(TOT_ENROLL),
    
    # Add size category for recommendation system
    SIZE_CATEGORY = case_when(
      TOT_ENROLL < 5000 ~ "Small (< 5,000)",
      TOT_ENROLL >= 5000 & TOT_ENROLL <= 15000 ~ "Medium (5,000-15,000)",
      TOT_ENROLL > 15000 ~ "Large (> 15,000)",
      TRUE ~ "Unknown"
    ),
    
    # Add support level for recommendation system
    SUPPORT_LEVEL = case_when(
      STUDENT_EMP_RATIO <= 10 ~ "High",
      STUDENT_EMP_RATIO > 10 & STUDENT_EMP_RATIO <= 20 ~ "Medium",
      STUDENT_EMP_RATIO > 20 ~ "Low",
      TRUE ~ "Unknown"
    ),
    
    # Create housing field for recommendation system
    HAS_HOUSING = ifelse(HOUSING == "Yes", "Yes", "No")
  )

# Remove rows with missing values for key variables
colleges_analysis <- colleges_clean %>%
  filter(!is.na(TOT_ENROLL) & !is.na(TOT_EMP) & !is.na(REGION))

# 2. EXPLORATORY ANALYSIS WITH PLOTLY -----------------------------------------

# Institution type distribution - interactive pie chart
type_count <- colleges_analysis %>%
  count(TYPE) %>%
  mutate(percentage = n / sum(n) * 100)

type_pie <- plot_ly(type_count, labels = ~TYPE, values = ~percentage, type = 'pie',
                   textinfo = 'label+percent',
                   insidetextorientation = 'radial',
                   marker = list(colors = viridis(4))) %>%
  layout(title = "Distribution of Institution Types")
print(type_pie)

# Regional distribution - interactive bar chart
region_count <- colleges_analysis %>%
  count(REGION) %>%
  mutate(percentage = n / sum(n) * 100)

region_bar <- plot_ly(region_count, x = ~REGION, y = ~percentage, type = 'bar',
                     marker = list(color = viridis(4))) %>%
  layout(title = "Distribution of Institutions by Region",
         xaxis = list(title = "Region"),
         yaxis = list(title = "Percentage"))
print(region_bar)

# Interactive enrollment by region and type visualization
enrollment_region_plot <- colleges_analysis %>%
  group_by(REGION, TYPE) %>%
  summarize(avg_enrollment = mean(TOT_ENROLL, na.rm = TRUE)) %>%
  ungroup() %>%
  plot_ly(
    x = ~REGION, 
    y = ~avg_enrollment, 
    color = ~TYPE,
    type = "bar"
  ) %>%
  layout(
    title = "Average Enrollment by Region and Institution Type",
    xaxis = list(title = "Region"),
    yaxis = list(title = "Average Enrollment"),
    barmode = "group",
    legend = list(title = list(text = "<b>Institution Type</b>"))
  )
print(enrollment_region_plot)

# Interactive Student-to-Employee Ratio Box Plot
ratio_boxplot <- colleges_analysis %>%
  filter(STUDENT_EMP_RATIO <= 30) %>%  # Filter extreme values for better visualization
  plot_ly(
    y = ~STUDENT_EMP_RATIO, 
    color = ~TYPE, 
    type = "box"
  ) %>%
  layout(
    title = "Student-to-Employee Ratio by Institution Type",
    xaxis = list(title = "Institution Type"),
    yaxis = list(title = "Student-to-Employee Ratio (lower is better)"),
    boxmode = "group"
  )
print(ratio_boxplot)

# Interactive Choropleth Map of Ratio by State
# Create a crosswalk between state abbreviations and names
state_crosswalk <- data.frame(
  STATE = c(state.abb, "DC"),
  state_name = c(tolower(state.name), "district of columbia"),
  stringsAsFactors = FALSE
)

# Calculate average ratio by state
state_ratio_data <- colleges_analysis %>%
  group_by(STATE) %>%
  summarize(
    avg_ratio = mean(STUDENT_EMP_RATIO, na.rm = TRUE),
    count = n()
  ) %>%
  ungroup() %>%
  mutate(hover_text = paste("State:", STATE, "<br>",
                           "Avg Ratio:", round(avg_ratio, 2), "<br>",
                           "# of Institutions:", count))

# Create interactive choropleth map
ratio_map_plotly <- plot_ly(
  data = state_ratio_data,
  type = "choropleth",
  locationmode = "USA-states",
  locations = ~STATE,
  z = ~avg_ratio,
  text = ~hover_text,
  colorscale = "Viridis",
  reversescale = TRUE,  # Reverse so better (lower) ratios are darker
  marker = list(line = list(color = "white", width = 0.5)),
  colorbar = list(title = "Avg Ratio")
) %>%
  layout(
    title = "Average Student-to-Employee Ratio by State",
    geo = list(
      scope = "usa",
      projection = list(type = "albers usa"),
      showlakes = TRUE,
      lakecolor = "rgb(255, 255, 255)"
    )
  )
print(ratio_map_plotly)

# 3. CLUSTER ANALYSIS ---------------------------------------------------------

# Select variables for clustering and filter for complete data
cluster_data <- colleges_analysis %>%
  filter(!is.na(STUDENT_EMP_RATIO) & !is.na(DORM_PERCENT)) %>%
  select(TOT_ENROLL, STUDENT_EMP_RATIO, DORM_PERCENT, NAME, TYPE, REGION, STATE)

# Create a copy with log-transformed enrollment and prepare for clustering
cluster_data_for_scaling <- cluster_data %>%
  mutate(TOT_ENROLL = log10(TOT_ENROLL)) %>%
  select(TOT_ENROLL, STUDENT_EMP_RATIO, DORM_PERCENT)

# Standardize variables for clustering
cluster_data_scaled <- scale(cluster_data_for_scaling)

# Determine the optimal number of clusters
set.seed(123)  # For reproducibility

# Elbow method
wss <- function(k) {
  kmeans(cluster_data_scaled, k, nstart = 25)$tot.withinss
}
k_values <- 1:10
wss_values <- sapply(k_values, wss)

# Plot the elbow plot
elbow_plot <- plot_ly(x = k_values, y = wss_values, type = "scatter", mode = "lines+markers") %>%
  layout(title = "Elbow Method for Optimal K",
         xaxis = list(title = "Number of clusters K"),
         yaxis = list(title = "Total within-clusters sum of squares"))
print(elbow_plot)

# Silhouette method
avg_sil <- function(k) {
  km <- kmeans(cluster_data_scaled, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(cluster_data_scaled))
  mean(ss[, 3])
}
sil_values <- sapply(2:10, avg_sil)

silhouette_plot <- plot_ly(x = 2:10, y = sil_values, type = "scatter", mode = "lines+markers") %>%
  layout(title = "Silhouette Method for Optimal K",
         xaxis = list(title = "Number of clusters K"),
         yaxis = list(title = "Average Silhouette Width"))
print(silhouette_plot)

# Based on plots, select optimal number of clusters (k=4)
k_optimal <- 4

# Perform k-means clustering
set.seed(123)
km_result <- kmeans(cluster_data_scaled, centers = k_optimal, nstart = 25)

# Add cluster assignments to the dataset
cluster_data$cluster <- km_result$cluster

# Examine cluster characteristics
cluster_summary <- cluster_data %>%
  group_by(cluster) %>%
  summarize(
    count = n(),
    avg_enrollment = mean(TOT_ENROLL),
    avg_ratio = mean(STUDENT_EMP_RATIO),
    avg_dorm_pct = mean(DORM_PERCENT)
  )
print("Cluster Summary:")
print(cluster_summary)

# Create interactive scatter plot of clusters
cluster_scatter <- plot_ly(
  data = cluster_data,
  x = ~TOT_ENROLL, 
  y = ~STUDENT_EMP_RATIO, 
  color = ~factor(cluster),
  symbol = ~TYPE,
  type = "scatter", 
  mode = "markers",
  marker = list(size = 10, opacity = 0.7),
  text = ~paste("Name:", NAME, "<br>",
               "Cluster:", cluster, "<br>",
               "Type:", TYPE, "<br>",
               "Enrollment:", round(TOT_ENROLL, 0), "<br>",
               "Student-Employee Ratio:", round(STUDENT_EMP_RATIO, 2), "<br>",
               "Dorm %:", round(DORM_PERCENT, 2))
) %>%
  layout(
    title = "College Clusters",
    xaxis = list(title = "Total Enrollment (log scale)", type = "log"),
    yaxis = list(title = "Student-to-Employee Ratio"),
    legend = list(title = list(text = "<b>Cluster</b>")),
    hovermode = "closest"
  )
print(cluster_scatter)

# Create interactive 3D cluster visualization
cluster_3d <- plot_ly(
  data = cluster_data,
  x = ~TOT_ENROLL, 
  y = ~STUDENT_EMP_RATIO, 
  z = ~DORM_PERCENT,
  color = ~factor(cluster),
  type = "scatter3d", 
  mode = "markers",
  marker = list(size = 3, opacity = 0.8),
  text = ~paste("Name:", NAME, "<br>",
               "Type:", TYPE, "<br>",
               "Enrollment:", round(TOT_ENROLL, 0), "<br>",
               "Student-Employee Ratio:", round(STUDENT_EMP_RATIO, 2), "<br>",
               "Dorm %:", round(DORM_PERCENT, 2))
) %>%
  layout(
    title = "3D Cluster Visualization",
    scene = list(
      xaxis = list(title = "Total Enrollment", type = "log"),
      yaxis = list(title = "Student-to-Employee Ratio"),
      zaxis = list(title = "Dormitory Capacity %")
    ),
    legend = list(title = list(text = "<b>Cluster</b>"))
  )
print(cluster_3d)

# 4. REGRESSION ANALYSIS ------------------------------------------------------

# Prepare data for regression analysis
reg_data <- colleges_analysis %>%
  filter(!is.na(STUDENT_EMP_RATIO) & !is.na(TOT_ENROLL) & !is.na(DORM_PERCENT))

# Correlation analysis of key variables
cor_vars <- reg_data %>%
  select(STUDENT_EMP_RATIO, log_enrollment, DORM_PERCENT, 
         has_housing, FT_PT_RATIO) %>%
  na.omit()

cor_matrix <- cor(cor_vars)
print("Correlation Matrix:")
print(cor_matrix)

# Create interactive correlation heatmap
cor_data <- reshape2::melt(cor_matrix)

cor_heatmap <- plot_ly(
  data = cor_data,
  x = ~Var1,
  y = ~Var2,
  z = ~value,
  type = "heatmap",
  colorscale = "Viridis",
  text = ~paste("Correlation:", round(value, 2))
) %>%
  layout(
    title = "Correlation Matrix",
    xaxis = list(title = ""),
    yaxis = list(title = "")
  )
print(cor_heatmap)

# Run models to predict student-to-employee ratio
# Comprehensive model with all predictors and interactions
full_model <- lm(STUDENT_EMP_RATIO ~ log_enrollment + TYPE * INST_SIZE + 
                   REGION + URBANICITY + has_housing, 
                 data = reg_data)

# Run stepwise regression to find optimal model
step_model <- step(full_model, direction = "both", trace = FALSE)
summary(step_model)

# Cross-validation for the best model
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)
cv_model <- train(STUDENT_EMP_RATIO ~ log_enrollment + TYPE + REGION + 
                    URBANICITY + has_housing, 
                  data = reg_data,
                  method = "lm",
                  trControl = train_control)

print("Cross-Validation Results:")
print(cv_model$results)

# Calculate variable importance
var_imp <- varImp(cv_model, scale = FALSE)
print("Variable Importance:")
print(var_imp)

# Interactive variable importance plot
var_imp_df <- var_imp$importance %>%
  rownames_to_column("Variable") %>%
  arrange(Overall) %>%
  mutate(Variable = factor(Variable, levels = Variable))

var_imp_plot <- plot_ly(
  data = var_imp_df,
  x = ~Overall,
  y = ~Variable,
  type = "bar",
  orientation = "h",
  marker = list(color = viridis(nrow(var_imp_df)))
) %>%
  layout(
    title = "Variable Importance",
    xaxis = list(title = "Importance"),
    yaxis = list(title = "")
  )
print(var_imp_plot)

# Create predicted vs actual visualization
prediction_data <- data.frame(
  Actual = reg_data$STUDENT_EMP_RATIO,
  Predicted = predict(step_model, newdata = reg_data),
  Type = reg_data$TYPE
)

pred_vs_actual <- plot_ly(
  data = prediction_data,
  x = ~Actual,
  y = ~Predicted,
  color = ~Type,
  type = "scatter",
  mode = "markers",
  marker = list(size = 10, opacity = 0.7)
) %>%
  layout(
    title = "Predicted vs Actual Student-to-Employee Ratio",
    xaxis = list(title = "Actual Ratio", range = c(0, 30)),
    yaxis = list(title = "Predicted Ratio", range = c(0, 30)),
    legend = list(title = list(text = "<b>Institution Type</b>")),
    shapes = list(
      list(
        type = "line",
        x0 = 0,
        x1 = 30,
        y0 = 0,
        y1 = 30,
        line = list(dash = "dash", color = "gray")
      )
    )
  )
print(pred_vs_actual)

# 5. INTEGRATED ANALYSIS ------------------------------------------------------
# Add cluster information and support level to the main dataset
colleges_integrated <- colleges_analysis %>%
  mutate(
    # Add support level based on student-to-employee ratio
    support_level = case_when(
      STUDENT_EMP_RATIO <= 10 ~ "High",
      STUDENT_EMP_RATIO <= 20 ~ "Medium",
      STUDENT_EMP_RATIO > 20 ~ "Low",
      TRUE ~ NA_character_
    ),
    support_level = factor(support_level, levels = c("High", "Medium", "Low"))
  )

# Add cluster assignments where available
cluster_assignments <- cluster_data %>%
  select(NAME, STATE, cluster)

colleges_integrated <- colleges_integrated %>%
  left_join(cluster_assignments, by = c("NAME", "STATE"))

# Create interactive scatter plot combining enrollment, ratio, support level and clusters
integrated_plot <- colleges_integrated %>%
  filter(!is.na(STUDENT_EMP_RATIO) & !is.na(support_level)) %>%
  plot_ly(
    x = ~TOT_ENROLL, 
    y = ~STUDENT_EMP_RATIO, 
    color = ~TYPE,
    symbol = ~support_level,
    type = "scatter", 
    mode = "markers",
    marker = list(size = 10, opacity = 0.7),
    text = ~paste("Name:", NAME, "<br>",
                  "State:", STATE, "<br>",
                  "Type:", TYPE, "<br>",
                  "Support Level:", support_level, "<br>",
                  "Cluster:", cluster, "<br>",
                  "Enrollment:", TOT_ENROLL, "<br>",
                  "Student-Employee Ratio:", round(STUDENT_EMP_RATIO, 2))
  ) %>%
  layout(
    title = "Integrated College Analysis",
    xaxis = list(title = "Total Enrollment (log scale)", type = "log"),
    yaxis = list(title = "Student-to-Employee Ratio", range = c(0, 40)),
    legend = list(title = list(text = "<b>Institution Type</b>")),
    hovermode = "closest"
  )
print(integrated_plot)

# Create interactive visualization of support level by institution type
support_by_type <- plot_ly(
  data = colleges_integrated %>% filter(!is.na(support_level)),
  x = ~TYPE,
  color = ~support_level,
  type = "histogram",
  histnorm = "percent"
) %>%
  layout(
    title = "Support Level Distribution by Institution Type",
    xaxis = list(title = "Institution Type"),
    yaxis = list(title = "Percentage"),
    barmode = "stack",
    legend = list(title = list(text = "<b>Support Level</b>"))
  )
print(support_by_type)

# 6. CONCLUSIONS AND KEY FINDINGS ---------------------------------------------
# Summarize key differences by institution type
type_summary <- colleges_analysis %>%
  group_by(TYPE) %>%
  summarize(
    count = n(),
    avg_enrollment = mean(TOT_ENROLL, na.rm = TRUE),
    avg_ratio = mean(STUDENT_EMP_RATIO, na.rm = TRUE),
    pct_with_housing = sum(HOUSING == "Yes", na.rm = TRUE) / n() * 100
  )

print("Key Differences by Institution Type:")
print(type_summary)

# Regional variations
region_summary <- colleges_analysis %>%
  group_by(REGION) %>%
  summarize(
    count = n(),
    avg_enrollment = mean(TOT_ENROLL, na.rm = TRUE),
    avg_ratio = mean(STUDENT_EMP_RATIO, na.rm = TRUE),
    pct_with_housing = sum(HOUSING == "Yes", na.rm = TRUE) / n() * 100
  )

print("Key Differences by Region:")
print(region_summary)

# Print cluster insights
print("Key Cluster Insights:")
print(cluster_summary)

# Print regression model summary
print("Key Regression Analysis Insights:")
print("Significant predictors of student-to-employee ratio:")
print(summary(step_model)$coefficients)

# Key factors identified through analysis
print("Key Factors in College Analysis:")
print("1. Institution Type - Strong predictor of student-to-employee ratio and housing availability")
print("2. Support Level - Varies significantly by institution type and size")
print("3. Size - Larger institutions generally have higher student-to-employee ratios")
print("4. Region/State - Significant variations in institutional characteristics by location")
print("5. Housing Availability - Varies dramatically by institution type")

#====================================================================
# PART 2: RECOMMENDATION SYSTEM
#====================================================================

# Create the recommendation function based on analysis insights
recommend_universities <- function(
  state = NULL,             # State (e.g., "CA", "NY")
  size_category = NULL,     # Size category (Small, Medium, Large)
  has_housing = NULL,       # Housing availability (Yes/No)
  support_level = NULL,     # Support level (High, Medium, Low)
  top_n = 10                # Number of recommendations to return
) {
  
  # Start with the full dataset (using already processed integrated data)
  filtered_colleges <- colleges_integrated
  
  # Apply filters based on parameters
  if (!is.null(state)) {
    filtered_colleges <- filtered_colleges %>%
      filter(STATE %in% state)
  }
  
  if (!is.null(size_category)) {
    filtered_colleges <- filtered_colleges %>%
      filter(SIZE_CATEGORY %in% size_category)
  }
  
  if (!is.null(has_housing)) {
    filtered_colleges <- filtered_colleges %>%
      filter(HAS_HOUSING %in% has_housing)
  }
  
  if (!is.null(support_level)) {
    filtered_colleges <- filtered_colleges %>%
      filter(SUPPORT_LEVEL %in% support_level)
  }
  
  # Return the top N results ordered by enrollment
  recommendations <- filtered_colleges %>%
    arrange(desc(TOT_ENROLL)) %>%
    select(NAME, STATE, TOT_ENROLL, SIZE_CATEGORY, HAS_HOUSING, 
           STUDENT_EMP_RATIO, SUPPORT_LEVEL, REGION, TYPE) %>%
    head(top_n)
  
  return(recommendations)
}

# Example usage:
example_recommendations <- recommend_universities(
  state = "CA",
  size_category = "Large (> 15,000)",
  has_housing = "Yes",
  support_level = "High"
)

print("Example Recommendations:")
print(example_recommendations)

# Shiny app for interactive recommendation
# This app integrates insights from our analysis into the recommendation system
ui <- fluidPage(
  titlePanel("University Recommendation System"),
  
  sidebarLayout(
    sidebarPanel(
      # State selection
      selectizeInput("state", "Location (State):",
                    choices = sort(unique(colleges_integrated$STATE)),
                    multiple = TRUE),
      
      # Size category
      checkboxGroupInput("size", "Enrollment Size:",
                       choices = c("Small (< 5,000)", "Medium (5,000-15,000)", "Large (> 15,000)"),
                       selected = c("Small (< 5,000)", "Medium (5,000-15,000)", "Large (> 15,000)")),
      
      # Housing availability
      radioButtons("housing", "Housing Available:",
                 choices = c("Either" = "both", "Yes", "No"),
                 selected = "both"),
      
      # Support level
      radioButtons("support", "Support Level:",
                 choices = c("Any" = "all", "High", "Medium", "Low"),
                 selected = "all"),
      
      # Region preferences (added from analysis)
      checkboxGroupInput("region", "Region:",
                       choices = c("Northeast", "Midwest", "South", "West"),
                       selected = c("Northeast", "Midwest", "South", "West")),
      
      # Institution type preferences (added from analysis)
      checkboxGroupInput("type", "Institution Type:",
                       choices = c("Public", "Private Non-Profit", "Private For-Profit", "Other"),
                       selected = c("Public", "Private Non-Profit", "Private For-Profit", "Other")),
      
      # Number of recommendations
      numericInput("topN", "Number of Recommendations:", 
                  value = 10, min = 1, max = 50),
      
      # Submit button
      actionButton("recommend", "Find Universities", 
                  class = "btn-primary", style = "width: 100%")
    ),
    
    mainPanel(
      # Display instruction text before recommendations
      conditionalPanel(
        condition = "input.recommend == 0",
        h3("Welcome to the University Finder!"),
        p("Use the filters on the left to find universities that match your preferences:"),
        tags$ul(
          tags$li(strong("Location:"), "Select one or more states where you'd like to study"),
          tags$li(strong("Enrollment Size:"), "Choose your preferred university size"),
          tags$li(strong("Housing:"), "Indicate if on-campus housing is important to you"),
          tags$li(strong("Support Level:"), "Select based on staff-to-student ratio (higher support = lower ratio)"),
          tags$li(strong("Region:"), "Select preferred geographic regions in the US"),
          tags$li(strong("Institution Type:"), "Select types of institutions you're interested in")
        ),
        p("When you're ready, click the 'Find Universities' button to see your recommendations."),
        
        # Show key insights from analysis to guide selection
        h3("Key Insights from Analysis:"),
        tags$ul(
          tags$li("Private Non-Profit institutions tend to have lower student-to-employee ratios (better support)"),
          tags$li("Larger institutions typically have higher student-to-employee ratios"),
          tags$li("Institutions in the Northeast region have higher percentages of housing availability"),
          tags$li("Public institutions have the highest average enrollment")
        )
      ),
      
      # Results tab
      conditionalPanel(
        condition = "input.recommend > 0",
        tabsetPanel(
          tabPanel("Results", 
                  h3("Recommended Universities"),
                  DT::dataTableOutput("recommendationsTable")),
          tabPanel("Statistics",
                  h3("Recommendation Statistics"),
                  plotOutput("sizePlot"),
                  plotOutput("supportPlot"),
                  plotOutput("regionPlot"),
                  plotOutput("typePlot"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Generate recommendations when button is clicked
  recommendations <- eventReactive(input$recommend, {
    # Process housing input
    housing_input <- if(input$housing == "both") NULL else input$housing
    
    # Process support level input
    support_input <- if(input$support == "all") NULL else input$support
    
    # Process region input
    region_input <- if(length(input$region) == 4) NULL else input$region
    
    # Process type input
    type_input <- if(length(input$type) == 4) NULL else input$type
    
    # Generate filtered dataset
    filtered_data <- colleges_integrated
    
    # Apply state filter if selected
    if(length(input$state) > 0) {
      filtered_data <- filtered_data %>% filter(STATE %in% input$state)
    }
    
    # Apply size filter if selected
    if(length(input$size) > 0 && length(input$size) < 3) {
      filtered_data <- filtered_data %>% filter(SIZE_CATEGORY %in% input$size)
    }
    
    # Apply housing filter
    if(housing_input != "both" && !is.null(housing_input)) {
      filtered_data <- filtered_data %>% filter(HAS_HOUSING == housing_input)
    }
    
    # Apply support level filter
    if(support_input != "all" && !is.null(support_input)) {
      filtered_data <- filtered_data %>% filter(SUPPORT_LEVEL == support_input)
    }
    
    # Apply region filter
    if(!is.null(region_input)) {
      filtered_data <- filtered_data %>% filter(REGION %in% region_input)
    }
    
    # Apply type filter
    if(!is.null(type_input)) {
      filtered_data <- filtered_data %>% filter(TYPE %in% type_input)
    }
    
    # Return top results
    filtered_data %>%
      arrange(desc(TOT_ENROLL)) %>%
      select(NAME, STATE, REGION, TYPE, TOT_ENROLL, SIZE_CATEGORY, HAS_HOUSING, 
             STUDENT_EMP_RATIO, SUPPORT_LEVEL, cluster) %>%
      head(input$topN)
  })
  
  # Display the recommendations as a table
  output$recommendationsTable <- DT::renderDataTable({
    recs <- recommendations()
    
    if(nrow(recs) == 0) {
      return(data.frame(Message = "No universities match your criteria. Try adjusting your filters."))
    }
    
    DT::datatable(recs, 
                options = list(pageLength = 10),
                rownames = FALSE) %>%
      formatRound('STUDENT_EMP_RATIO', 1)
  })
}

# Uncomment to run the app
# shinyApp(ui = ui, server = server)

# Uncomment to Save the processed data for future use
#write.csv(colleges_integrated, "processed_colleges_data.csv", row.names = FALSE)
