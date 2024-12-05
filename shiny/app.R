library(flexdashboard)
library(dplyr)
library(readr)
library(plotly)
library(shiny)
library(tidyr)
library(lubridate)

# Read the datasets
population_data <- read.table(file = "data/Population by States_shiny.txt", header = TRUE, sep = "\t", quote = "\"", fill = TRUE)
death_data <- read_csv("data/full_weekly_deaths_by_state_and_causes_shiny.csv")

# Clean and process population data
population_data_clean <- population_data %>%
  select(Residence.State, Year, Population) %>%
  filter(!is.na(Population)) %>%
  mutate(Year = as.numeric(gsub(" .*", "", Year))) %>%
  rename(State = Residence.State)

# Clean and process death data
death_data_clean <- death_data %>%
  select(state, year, all_cause, month) %>% 
  group_by(state, year, month) %>%
  summarise(Total_Deaths = sum(all_cause, na.rm = TRUE), .groups = "drop") %>%
  rename(State = state, Year = year, Month = month)

# Merge the datasets
merged_data <- death_data_clean %>%
  inner_join(population_data_clean, by = c("State", "Year"))

# Add state abbreviations
state_abbreviations <- data.frame(
  State = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida",
            "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
            "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska",
            "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
            "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
            "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  Abbreviation = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS",
                   "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
                   "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
                   "WI", "WY")
)

# Merge abbreviations into the dataset
merged_data <- merged_data %>%
  left_join(state_abbreviations, by = "State") # Ensure 'Abbreviation' is added

# Define state regions
state_regions <- data.frame(
  Abbreviation = c("CT", "ME", "MA", "NH", "NJ", "NY", "PA", "RI", "VT", 
                   "IL", "IN", "IA", "KS", "MI", "MN", "MO", "NE", "ND", "OH", "SD", "WI", 
                   "AL", "AR", "DE", "DC", "FL", "GA", "KY", "LA", "MD", "MS", "NC", "OK", "SC", "TN", "TX", "VA", "WV", 
                   "AK", "AZ", "CA", "CO", "HI", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY"),
  Region = c(rep("Northeast", 9), 
             rep("Midwest", 12), 
             rep("South", 17), 
             rep("West", 13))
)

# Merge regions into the dataset
merged_data <- merged_data %>%
  left_join(state_regions, by = "Abbreviation") # Join with region data

# Calculate mortality rates
merged_data <- merged_data %>%
  mutate(
    Annual_Mortality_Rate = (Total_Deaths / Population) * 1000,  # Annual rate per 1,000
    Monthly_Mortality_Rate = (Total_Deaths / Population) * 10000 / 12  # Monthly rate per 10,000
  )

# Save the updated dataset
write.csv(merged_data, "./data/Mortality_Rate_with_Region.csv", row.names = FALSE)

# Define UI
ui <- fluidPage(
  titlePanel("Monthly Mortality Rates by Region and State"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = 2020:2023, selected = 2020),
      checkboxInput("singleRegion", "Single Region Mode", value = FALSE),
      checkboxGroupInput(
        "regions", 
        "Select Regions:", 
        choices = c("Northeast", "Midwest", "South", "West"), 
        selected = c("Northeast")
      )
    ),
    mainPanel(
      plotlyOutput("mortalityPlot", height = "800px", width = "90%") # Ensure enough space for plots
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$singleRegion, {
    if (input$singleRegion) {
      updateCheckboxGroupInput(session, "regions", selected = input$regions[1])
    }
  })
  
  # Reactive data filtered by year and selected regions
  filtered_data <- reactive({
    req(input$regions) # Ensure at least one region is selected
    merged_data %>%
      filter(Year == input$year, Region %in% input$regions)
  })
  
  # Render the plot
  output$mortalityPlot <- renderPlotly({
    data <- filtered_data()
    
    # Dynamically adjust plot height based on the number of selected regions
    plot_height <- 250 + length(unique(data$Region)) * 200
    
    # Create individual plots with region name displayed below the plot
    plots <- lapply(seq_along(unique(data$Region)), function(i) {
      region_name <- unique(data$Region)[i]
      region_data <- data %>% filter(Region == region_name)
      
      plot_ly(
        data = region_data,
        x = ~Abbreviation, 
        y = ~Monthly_Mortality_Rate,
        type = "violin",
        box = list(visible = TRUE),
        points = "all",
        color = ~Abbreviation,
        legendgroup = region_name,
        showlegend = FALSE
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

# Run the application
shinyApp(ui = ui, server = server)
