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
library(ggthemes)
library(tidyverse)
library(DT)
library(gt)
library(shinyWidgets)
library(shinycssloaders)
library(sass)

# Reading in team ranking data.

T_D <- read_rds("raw_data/Team D.rds")
T_Ast <- read_rds("raw_data/Team AST.rds")
T_A <- read_rds("raw_data/Team A.rds")
T_Att <- read_rds("raw_data/Team Att.rds")
T_SA <- read_rds("raw_data/Team SA.rds")
T_K <- read_rds("raw_data/Team K.rds")
T_B <- read_rds("raw_data/Team B.rds")
T_WR <- read_rds("raw_data/Team WR.rds")
T_HP <- read_rds("raw_data/Team Ht Pct.rds")
T_OHP <- read_rds("raw_data/Team Opp Ht Pct.rds")

# Reading in individual player ranking data.

I_D <- read_rds("raw_data/Individual D.rds")
I_B <- read_rds("raw_data/Individual B.rds")
I_A <- read_rds("raw_data/Individual A.rds")
I_Ast <- read_rds("raw_data/Individual AST.rds")
I_Att <- read_rds("raw_data/Individual Att.rds")
I_K <- read_rds("raw_data/Individual K.rds")
I_Pts <- read_rds("raw_data/Individual Pts.rds")
I_SA <- read_rds("raw_data/Individual SA.rds")
I_HP <- read_rds("raw_data/Individual Ht Pct.rds")



  

ui <- navbarPage(
    "NCAA Women's Volleyball Performance",
    theme = shinytheme("cerulean"),
    tabPanel("About",
      includeHTML("About.html"),
      includeCSS("www/main.css",
                 "www/fontawesome-all.min.css"),
      includeScript("www/breakpoints.min.js",
                    "www/browser.min.js",
                    "www/jquery.min.js",
                    "www/jquery.scrollex.min.js",
                    "www/jquery.scrolly.min.js",
                    "www/main.js",
                    "www/util.js")
    ),
    tabPanel("By Individual Players", 
                 h2("Distribution across the country"),
                 p("Insert explanations"),
                 
                 # heat map visual marking where the most 
                 # top ranked players are
                 # also do one by conference
             plotOutput(outputId = "plot1"), 
          
                 sidebarLayout(
                   sidebarPanel(
                selectInput(inputId = "Class",
                                   label = "Select Class:",
                                   choices = c("Sophomore" = "So.", 
                                               "Junior" = "Jr.",
                                               "Senior" = "Sr.")),
                
                # Select which Division(s) to plot
                selectInput(inputId = "Category",
                                   label = "Select Category:",
                                   choices = c("Kills" = I_K, 
                                               "Blocks" = I_B, 
                                               "Digs" = I_D,
                                               "Attacks" = I_Att,
                                               "Assists" = I_Ast,
                                               "Aces" = I_A,
                                               "Service Aces" = I_SA,
                                               "Hitting Percentage" = I_HP,
                                               "Points" = I_Pts),
                                   selected = "Aces")
               ), 
               mainPanel(
                 radioButtons(inputId = "TeamFinder",
                              label = "Display:",
                              choices = c("School Names", "Conference Names"),
                              selected = "School Names")
               ))
               ),
    tabPanel("By Teams", 
                 "Top Teams in the Nation",
                 h2("Top ranking teams in every category"),
                 p("Insert explanations"),
                 # heat map showing distribution of top teams
                 # across the country by region/state
                 # also do one by conferences across the country
               ),
                 
                 # wordcloud showing frequency of each team to be
                 # ranked, per skill set -> a wordcloud for 
                 # every category
                 # Potentially a measurement of popularity??
                 # do both first
                 
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
             )))



server <- function(input, output) {
  output$plot1 <- renderPlot(
    ggplot(I_A, aes(x = Player, y = `Per Set`, fill = input$Class)) +
      geom_point()
  )
}

shinyApp(ui = ui, server = server)