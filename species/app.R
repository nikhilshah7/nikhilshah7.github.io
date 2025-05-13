#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(tidyverse)

species <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-10-08/most_visited_nps_species_data.csv')

animals <- species |>
  filter(CategoryName == 'Mammal' | CategoryName == 'Fish' | CategoryName == 'Bird' | CategoryName == 'Amphibian' | CategoryName == 'Reptile')

species <- species |>
  mutate(ParkName = str_replace(ParkName, ' National Park', '')) |>
  mutate(ParkName = fct_infreq(ParkName))


library(shiny)

ui <- fluidPage(
  fluidRow(
    column(6,
           selectInput("family", "Family:", 
                       choices = sort(unique(animals$Family)),
                       selected = "Mustelidae")
    )
  ),
  plotOutput("shinyplot")
)

server <- function(input, output, session) {
  
  output$shinyplot <- renderPlot({
    species |>
      filter(Family == input$family) |>
      ggplot(aes(y = ParkName)) +
      geom_bar() +
      labs(
        y = 'National Park',
        x = '# Species',
        title = paste('Distinct species in family', input$family, 'in most popular national parks')
      )
  })
  
}

shinyApp(ui = ui, server = server)
