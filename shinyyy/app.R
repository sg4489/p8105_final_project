library(flexdashboard)
library(RColorBrewer) 
library(dplyr)
library(readr)
library(janitor)  
library(plotly)
library(shiny)
library(tidyr)
library(lubridate)

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
write.csv(death_data_filtered, "./data/filtered_death_data.csv", row.names = FALSE)

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

# Load the dataset
data <- read_csv("./data/filtered_death_data.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Death Cause Distribution by State and Year"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = unique(data$year)),
      selectInput("state", "Select State:", choices = unique(data$state))
    ),
    
    mainPanel(
      plotlyOutput("pie_chart", height = "600px")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive data filtered by selected year and state
  filtered_data <- reactive({
    req(input$year, input$state)
    data %>%
      filter(year == input$year, state == input$state)
  })
  
  # Reactive data for death cause totals
  death_cause_totals <- reactive({
    req(filtered_data())
    
    # List of specific death causes to include
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
    
    # Summarize counts for each cause
    filtered_data() %>%
      summarise(across(all_of(cause_columns), sum, na.rm = TRUE)) %>%
      pivot_longer(cols = everything(), names_to = "cause", values_to = "count") %>%
      filter(count > 0) # Filter out zero counts for better visualization
  })
  
  # Render the Plotly pie chart
  output$pie_chart <- renderPlotly({
    req(death_cause_totals())
    
    plot_ly(
      death_cause_totals(),
      labels = ~cause,
      values = ~count,
      type = 'pie'
    ) %>%
      layout(
        title = list(
          text = paste("Death Cause Distribution in", input$state, "for Year", input$year),
          x = 0.5
        ),
        legend = list(
          orientation = "v",    # Change to vertical orientation
          x = 1.05,             # Move the legend slightly to the right
          y = 0.5,              # Center the legend vertically
          xanchor = "left",     # Anchor the legend to the left of the chart
          yanchor = "middle"    # Anchor the legend in the middle vertically
        ),
        margin = list(t = 50, b = 100, r = 250) # Increase right margin to avoid overlap
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)