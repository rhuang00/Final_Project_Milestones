#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(stringr)
library(janitor)
library(ggplot2)
library(wordcloud)
library(lubridate)
library(ggthemes)
library(broom)
library(tidyverse)
library(DT)
library(gt)
library(shinyWidgets)
library(plotly)


#source("Organization.Rmd")

ui <- navbarPage(
    "Analysis of NCAA Women's Volleyball's Performance \n2011-19",
    tabPanel(
      "About",
     # includeHTML("")
    ),
    tabPanel("By Individual Players",
             tabsetPanel(
               tabPanel(
                 "Across the country",
                 h2("Distribution across the country"),
                 p("Insert explanations"),
                 # heat map visual marking where the most 
                 # top ranked players are
                 # also do one by conference
               ),
               tabPanel(
                 "Performance throughout time",
                 h2("Average performance of ranked players\nevery season"),
                 p("Insert explanations")
                 # line plot mapping avg performance per skillset
                 # across time
               ))),
    tabPanel("By Teams",
             tabsetPanel(
               tabPanel(
                 "Top Teams in the Nation",
                 h2("Top ranking teams across the country"),
                 p("Insert explanations"),
                 # heat map showing distribution of top teams
                 # across the country by region/state
                 # also do one by conferences across the country
               ),
               tabPanel(
                 "Frequency",
                 h2("Frequency for each team to be ranked"),
                 p("Insert explanations")
                 # wordcloud showing frequency of each team to be
                 # ranked, per skill set -> a wordcloud for 
                 # every category
                 # Potentially a measurement of popularity??
                 # do both first
               ))),
    tabPanel("Model", 
             tabsetPanel(
             tabPanel(
               "Individual Players",
               h2("Individual Players"),
             p("Insert explanation about regression")
             # first regression: class year vs performance 
             # per skill set
             # second regression: performance over time per
             # skill set based on region/conference/school
             ),
             tabPanel(
               "Teams",
               h2("Teams"),
               p("Insert explanation about regression")
               # potentially a regression on W-L rates ,
               # likelihood of each school that was ranked to
               # be ranked again (choose top 25 schools?)
               # regression of performance over time per skill
               # set based on regions, potentially divided into
               # within each conference, and then national 
               # rankings
             )
             )),
    tabPanel(
      "Methods",
     # includeHTML("")
    ))



server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)