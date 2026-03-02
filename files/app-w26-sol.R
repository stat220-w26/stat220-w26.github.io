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
         race) |>
  filter(annual_salary < 500000)

# Find all industries ----------------------------------------------------------

industry_choices <- manager_survey |>
  distinct(industry) |>
  arrange(industry) |>
  pull(industry)

# Randomly select 3 industries to start with -----------------------------------


selected_industry_choices <- sample(industry_choices, 3)

quant_vars <- manager_survey |>
  select(where(is.numeric)) |>
  colnames()

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  titlePanel(title = "Exploring the Ask a Manager data"),
  sidebarLayout(
    
    # Sidebar panel
    sidebarPanel(
      inputPanel(
        selectInput('xcol', label = 'X Variable', choices = colnames(manager_survey), selected = names(manager_survey)[16]),
        selectInput('color', label = 'Color', choices = colnames(manager_survey), selected = names(manager_survey)[2]),
        selectInput('histvar', label = 'Histogram Variable', choices = quant_vars, selected = quant_vars[1]),
        ),
      sliderInput('salary_filter', label = 'Salary Range', min = 0, max = max(manager_survey$annual_salary), value = c(0, max(manager_survey$annual_salary))),
      checkboxInput('theme', label = 'Check for white background', value = FALSE),
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
      plotOutput(outputId = "scatterPlot"),
      plotOutput(outputId = "histogram")
    )
    
  )
)

# Define server function -------------------------------------------------------

server <- function(input, output, session) {
  
  # Plot of jittered data
  output$scatterPlot <- renderPlot({
    plot <- manager_survey |>
      filter(industry %in% input$industry) |>
      filter(annual_salary < input$salary_filter[2], annual_salary > input$salary_filter[1]) |>
      ggplot(aes_string(x = input$xcol, y = "annual_salary", color = input$color)) +
      geom_jitter(size = 2, alpha = 0.6) +
      theme(legend.position = "top")
    
    if(input$theme){
      plot <- plot + theme_minimal() + theme(legend.position = "top")
    }
    
    plot
  })
  
  output$histogram <- renderPlot({
    plot <- manager_survey |>
      filter(industry %in% input$industry) |>
      filter(annual_salary < input$salary_filter[2], annual_salary > input$salary_filter[1]) |>
      ggplot(aes_string(x = input$histvar)) +
      geom_histogram(bins = 20, col = "white") + 
      theme(legend.position = "top")
    
    if(input$theme){
      plot <- plot + theme_minimal() + theme(legend.position = "top")
    }
    
    plot
  })
  
}


# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)

