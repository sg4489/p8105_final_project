library(dplyr)
library(readr)
library(janitor)  
library(plotly)
library(shiny)
library(tidyr)
library(lubridate)
library(flexdashboard)
library(RColorBrewer) 

# Read the datasets and convert to snake_case
population_data <- read.table(file = "./data/Population by States_shiny.txt", header = TRUE, sep = "\t", quote = "\"", fill = TRUE) %>%
  clean_names()  # Convert column names to snake_case

death_data <- read_csv("./data/full_weekly_deaths_by_state_and_causes_shiny.csv") %>%
  clean_names()  # Convert column names to snake_case

# Clean and process population data
population_data_clean <- population_data %>%
  select(residence_state, year, population) %>%
  filter(!is.na(population)) %>%
  mutate(year = as.numeric(gsub(" .*", "", year))) %>%
  rename(state = residence_state)

# Filter death data to remove specific states
death_data_filtered <- death_data %>%
  filter(!state %in% c("United States", "Puerto Rico", "New York City"))  # Remove specific states

# Save the filtered data to a new CSV file
write.csv(death_data_filtered, "./data/filtered_death_data_shiny.csv", row.names = FALSE)

# Continue cleaning and processing the filtered death data
death_data_clean <- death_data_filtered %>%
  select(state, year, all_cause, month) %>%
  group_by(state, year, month) %>%
  summarise(total_deaths = sum(all_cause, na.rm = TRUE), .groups = "drop")


# Merge the datasets
merged_data <- death_data_clean %>%
  inner_join(population_data_clean, by = c("state", "year"))

# Add state abbreviations
state_abbreviations <- data.frame(
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida",
            "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
            "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska",
            "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
            "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
            "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  abbreviation = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS",
                   "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
                   "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
                   "WI", "WY")
)

# Merge abbreviations into the dataset
merged_data <- merged_data %>%
  left_join(state_abbreviations, by = "state")  # Ensure 'abbreviation' is added

# Define state regions
state_regions <- data.frame(
  abbreviation = c("CT", "ME", "MA", "NH", "NJ", "NY", "PA", "RI", "VT", 
                   "IL", "IN", "IA", "KS", "MI", "MN", "MO", "NE", "ND", "OH", "SD", "WI", 
                   "AL", "AR", "DE", "DC", "FL", "GA", "KY", "LA", "MD", "MS", "NC", "OK", "SC", "TN", "TX", "VA", "WV", 
                   "AK", "AZ", "CA", "CO", "HI", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY"),
  region = c(rep("Northeast", 9), 
             rep("Midwest", 12), 
             rep("South", 17), 
             rep("West", 13))
)

# Merge regions into the dataset
merged_data <- merged_data %>%
  left_join(state_regions, by = "abbreviation")  # Join with region data

# Calculate mortality rates
merged_data <- merged_data %>%
  mutate(
    monthly_mortality_rate = (total_deaths / population) * 10000 / 12  # Monthly rate per 10,000
  )

# Save the updated dataset
write.csv(merged_data, "./data/mortality_rate_with_region.csv", row.names = FALSE)

# Load the dataset for the second plot
data1 <- read_csv("./data/filtered_death_data_shiny.csv")
data2 <- read_csv("./data/Mortality_Rate_with_Region.csv")

# Define UI
ui <- fluidPage(
  # Second Plot: Monthly Mortality Rate by Year and Region
  titlePanel("Monthly Mortality Rate by Year and Region"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year2", "Select Year:", choices = unique(data2$year), selected = unique(data2$year)[1]),
      selectInput("region2", "Select Region:", choices = unique(data2$region), selected = unique(data2$region)[1])
    ),
    mainPanel(
      plotlyOutput("mortalityPlot2", height = "600px")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  # First Plot: Death Cause Distribution by State and Year
  filtered_data1 <- reactive({
    req(input$year1, input$state1)
    data1 %>%
      filter(year == input$year1, state == input$state1)
  })
  
  death_cause_totals1 <- reactive({
    req(filtered_data1())
    
    cause_columns <- c(
      "septicemia",
      "malignant_neoplasms",
      "diabetes_mellitus",
      "alzheimer_disease",
      "influenza_and_pneumonia",
      "chronic_lower_respiratory_diseases",
      "other_respiratory_diseases",
      "kidney_disease",
      "other_symptoms",
      "diseases_of_heart",
      "cerebrovascular_diseases",
      "covid_19_multiple",
      "covid_19_underlying"
    )
    
    filtered_data1() %>%
      summarise(across(all_of(cause_columns), sum, na.rm = TRUE)) %>%
      pivot_longer(cols = everything(), names_to = "Cause", values_to = "Count") %>%
      filter(Count > 0)
  })
  
  output$pieChart1 <- renderPlotly({
    req(death_cause_totals1())
    
    plot_ly(
      death_cause_totals1(),
      labels = ~Cause,
      values = ~Count,
      type = 'pie'
    ) %>%
      layout(
        title = list(
          text = paste("Death Cause Distribution in", input$state1, "for year", input$year1),
          x = 0.5
        ),
        legend = list(
          orientation = "h",         # Horizontal layout for the legend
          x = 0.5,                   # Center the legend horizontally
          y = -0.3,                  # Move the legend below the plot
          xanchor = "center",        # Anchor the legend to the center
          yanchor = "top"            # Position the legend above the x-axis
        )
      )
  })
  
  # Second Plot: Monthly Mortality Rate by Year and Region
  filtered_data2 <- reactive({
    req(input$year2, input$region2)
    data2 %>%
      filter(year == input$year2, region == input$region2)
  })
  
  output$mortalityPlot2 <- renderPlotly({
    filtered <- filtered_data2()
    
    if (nrow(filtered) == 0) {
      return(plotly_empty() %>%
               layout(title = "No data available for the selected year and region."))
    }
    
    plot_ly(
      data = filtered,
      x = ~month,
      y = ~monthly_mortality_rate,
      color = ~state,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(width = 2),
      text = ~paste("State:", state, "<br>Month:", month, "<br>Rate:", monthly_mortality_rate)
    ) %>%
      layout(
        title = paste("Monthly Mortality Rate in", input$year2, "for", input$region2, "Region"),
        xaxis = list(
          title = "Month", 
          tickvals = 1:12, 
          ticktext = month.abb  # Use abbreviated month names
        ),
        yaxis = list(title = "Monthly Mortality Rate (per 10,000)"),
        legend = list(
          orientation = "h",         # Horizontal legend layout
          x = 0.5,                   # Center the legend horizontally
          y = -0.2,                  # Position the legend below the plot
          xanchor = "center",        # Anchor the legend to the center
          yanchor = "top"            # Position the legend above the x-axis
        ),
        margin = list(b = 150)       # Add margin to prevent overlap with the legend
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)