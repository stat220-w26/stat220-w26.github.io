# Load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(ggthemes)
library(scales)

# Load data --------------------------------------------------------------------

manager_survey <- read_rds("https://github.com/stat220-w26/stat220-w26.github.io/raw/refs/heads/main/slides/23/manager-survey-app/manager-survey-processed.rds") |>
  select(age = how_old_are_you,
         industry = industry_other,
         annual_salary, 
         other_monetary_comp,
         state, 
         overall_years_of_professional_experience,
         years_of_experience_in_field,
         gender,
         race)

# Find all industries ----------------------------------------------------------

industry_choices <- manager_survey |>
  distinct(industry) |>
  arrange(industry) |>
  pull(industry)

# Randomly select 3 industries to start with -----------------------------------


selected_industry_choices <- sample(industry_choices, 3)


# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  titlePanel(title = "Exploring the Ask a Manager data"),
  sidebarLayout(
    
    # Sidebar panel
    sidebarPanel(
      inputPanel(
        selectInput('xcol', label = 'X Variable', choices = colnames(manager_survey), selected = names(manager_survey)[16]),
      ),
      checkboxGroupInput(
        inputId = "industry",
        label = "Select up to 8 industies:",
        choices = industry_choices,
        selected = selected_industry_choices
      ),
    ),
    
    # Main panel
    mainPanel(
      hr(),
      "Showing only results for those with salaries in USD who have provided information on their industry and highest level of education completed.",
      br(), br(),
      textOutput(outputId = "selected_industries"),
      hr(),
      br(),
      plotOutput(outputId = "scatterPlot")
    )
    
  )
)

# Define server function -------------------------------------------------------

server <- function(input, output, session) {
  
  # Plot of jittered data
  output$scatterPlot <- renderPlot({
    manager_survey |>
      filter(industry %in% input$industry) |>
      ggplot( aes_string(x = input$xcol, y = "annual_salary")) +
      geom_jitter(size = 2, alpha = 0.6, aes(color = industry)) +
      theme(legend.position = "top")
  })
  
 
}


# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
