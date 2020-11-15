#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(readr)
library(rstanarm)
library(rsample)


Stats <- read_excel("raw_data/NCAA Statistics.xlsx")

ui <- navbarPage(
    "Analysis of NCAA Women's Volleyball's Performance 2016-17",
    tabPanel("Models",
             fluidPage(
                 titlePanel("Aces Rankings"),
                     mainPanel(plotOutput("ppModel")),
                 )
             ),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is a project that analyzes Team USA's performance in Women's Volleyball during the 2016 Olympics.
               Data is taken from the TeamUSA Volleyball website, and was gathered per game. Not all the data has been gathered
               and cleaned up yet, but data used for this milestone was taken from the game between Team USA and Serbia for the Semifinals."),
             h3("About Me"),
             p("My name is Ruby Huang and I study History & Government. You can reach me at rubyhuang@college.harvard.edu.")))


server <- function(input, output) {
    output$ppModel <- renderPlot({
      
      fit_obj <- stan_glm(Aces ~ Cl, 
                          data = Stats, 
                          refresh = 0)
        fit_obj %>% 
          as_tibble() %>% 
          select(-sigma) %>% 
          mutate(Junior = ClJr., Sophomore = ClSo., Senior = ClSr.) %>%
          pivot_longer(cols = Junior:Senior,
                       names_to = "Parameter",
                       values_to = "aces") %>% 
          ggplot(aes(x = aces, color = Parameter)) +
          geom_histogram(aes(y = after_stat(count/sum(count))),
                         alpha = 0.5, 
                         bins = 100, 
                         position = "identity") +
          labs(title = "Posterior Probability Distribution",
               subtitle = "Average aces per set for\nNCAA Women's Volleyball Players in 2016-17",
               x = "Average Aces per Set",
               y = "Probability") +
          scale_y_continuous(labels = scales::percent_format()) +
          theme_classic()
    })
  
}



shinyApp(ui = ui, server = server)