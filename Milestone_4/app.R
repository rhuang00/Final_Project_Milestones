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
library(readr)
library(readxl)


US_Players

ui <- navbarPage(
    "Analysis of Team USA Women's Volleyball's Olympics Performance",
    tabPanel("Models",
             fluidPage(
                 titlePanel("Team USA Players"),
                     mainPanel(tableOutput("table")))
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
    output$table <- renderTable({
        US_Players %>% 
            group_by(Players) %>% 
            summarise(Serve_Success = Serve_Total - Serve_Error)
    })
}



shinyApp(ui = ui, server = server)