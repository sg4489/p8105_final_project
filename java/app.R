


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
population_data <- read.table(file = "./data/Population by States.txt", header = TRUE, sep = "\t", quote = "\"", fill = TRUE) %>%
  clean_names()  # Convert column names to snake_case

death_data <- read_csv("./data/full_weekly_deaths_by_state_and_causes.csv") %>%
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


# Define UI
ui <- fluidPage(
  titlePanel("Monthly Mortality Rates by Region and State"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = 2020:2023, selected = 2020),
      checkboxInput("single_region", "Single Region Mode", value = FALSE),
      checkboxGroupInput(
        "regions", 
        "Select Regions:", 
        choices = c("Northeast", "Midwest", "South", "West"), 
        selected = c("Northeast")
      )
    ),
    mainPanel(
      plotlyOutput("mortality_plot", height = "800px", width = "90%")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Observer for single region mode
  observeEvent(input$single_region, {
    if (input$single_region) {
      updateCheckboxGroupInput(session, "regions", selected = input$regions[1])
    }
  })
  
  # Reactive data filtered by year and selected regions
  filtered_data <- reactive({
    req(input$regions) # Ensure at least one region is selected
    merged_data %>%
      filter(year == input$year, region %in% input$regions)
  })
  
  # Render the plot
  output$mortality_plot <- renderPlotly({
    data <- filtered_data()
    
    # Dynamically adjust plot height based on the number of selected regions
    plot_height <- 250 + length(unique(data$region)) * 200
    
    # Create individual plots with region name displayed below the plot
    plots <- lapply(seq_along(unique(data$region)), function(i) {
      region_name <- unique(data$region)[i]
      region_data <- data %>% filter(region == region_name)
      
      plot_ly(
        data = region_data,
        x = ~abbreviation, 
        y = ~monthly_mortality_rate,
        type = "violin",
        box = list(visible = TRUE),
        points = "all",
        color = ~abbreviation,
        legendgroup = region_name,
        showlegend = FALSE # Hide the legend for each plot
      ) %>%
        layout(
          annotations = list(
            list(
              text = paste0(region_name, " Region"), # Display region name below the plot
              x = 0.5,
              y = -0.2, # Position below the x-axis
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              font = list(size = 14, color = "black"),
              xanchor = "center",
              yanchor = "top"
            )
          ),
          xaxis = list(
            title = "State",
            tickangle = 45
          ),
          yaxis = list(
            title = "Monthly Mortality Rate (per 10,000)"
          )
        )
    })
    
    # Combine all individual plots into a single subplot
    plot <- subplot(
      plots,
      nrows = ifelse(length(input$regions) > 2, 2, 1), # Dynamically adjust the number of rows
      shareY = TRUE,
      titleX = TRUE,
      titleY = TRUE,
      margin = 0.1
    ) %>%
      layout(
        title = list(
          text = paste("Monthly Mortality Rates in", input$year, "by Selected Region(s)"),
          x = 0.5,
          y = 0.98,
          font = list(size = 24)
        ),
        height = plot_height, # Dynamically adjust the height
        width = 800          # Fixed width
      )
    
    plot
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)