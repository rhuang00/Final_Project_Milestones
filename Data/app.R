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
library(RColorBrewer)
library(wordcloud)

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

# Creating a list of the conferences for future use,
# such as for input selections or the word clouds.

conference_names <-  c("MEAC",
                       "Big Ten",
                       "Southland",
                       "SoCon",
                       "SAC",
                       "WAC",
                       "MVC",
                       "Big South",
                       "Sun Belt",
                       "Patriot",
                       "Big Sky",
                       "Big 12",
                       "WCC",
                       "SWAC",
                       "CAA",
                       "OVC",
                       "Big East",
                       "Big West",
                       "Mountain West",
                       "AAC",
                       "NEC",
                       "MAAC",
                       "C-USA",
                       "Atlantic 10",
                       "SEC",
                       "Pac-12",
                       "Horizon",
                       "America East",
                       "ASUN",
                       "Summit League",
                       "Ivy League",
                       "Independent")



  

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
                 h2("Performance across the country"),
                 p("Here we have graphs displaying the average performance
                   of individually ranked players in every skill category."),
                 p("The skill categories are:
                   Aces: 
                   Assists:
                   Attacks:"),
                 sidebarLayout(position = "left",
                   sidebarPanel(width = 2,
                selectInput(inputId = "Class",
                                   label = "Select Class:",
                                   choices = c("Sophomore" = "So.", 
                                               "Junior" = "Jr.",
                                               "Senior" = "Sr.")),
                selectInput(inputId = "ConferenceFinder",
                             label = "Conference:",
                             choices = c("School Names", 
                                         "Conference Names",
                                         ""),
                             selected = "All")
                
               ), 
               mainPanel(
                 fluidRow(
                 column(4,
                           plotOutput(outputId = "plot1"),
                           plotOutput(outputId = "plot2"), 
                           plotOutput(outputId = "plot3")),
                 column(4,
                           plotOutput(outputId = "plot4"),
                           plotOutput(outputId = "plot5"),
                           plotOutput(outputId = "plot6")),
                 column(4,
                           plotOutput(outputId = "plot7"),
                           plotOutput(outputId = "plot8"),
                           plotOutput(outputId = "plot9"))
                         )
                 )
               
              )),
  
    
    tabPanel("By Teams",
             tabsetPanel(
               tabPanel("Table",
                 "Top 10 Teams per Category", align = "center",
             sidebarLayout(
               sidebarPanel(width = 4,
                            position = "left",
                 selectInput(inputId = "Year",
                             label = "Year:",
                             choices = c(2011:2019)),
             selectInput("Category",
                         "Category:",
                         c("Aces",
                           "Assists",
                           "Attacks",
                           "Blocks",
                           "Digs",
                           "Hitting Percentage",
                           "Kills",
                           "Opponent Hitting Percentage",
                           "Service Aces",
                           "Win Rate",
                           selected = "Aces"
                         )
                         )
                          ),
                mainPanel(position = "right",
                  fluidRow(
                   column(12,
                        tableOutput(outputId = "table")
               )
             )
             )
             )
             ),
             tabPanel("Word Clouds",
                      h3("Here are the most often ranked teams or conferences from 2011-2019"),
                      radioButtons("wordcloud",
                                  label = "Display:",
                                  choices = c("Teams", "Conferences")
                      ),
                      
               
             )
             )
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



# regression models

fit_1 <- lm(Aces ~ Cl + Pos,
            I_A)



server <- function(input, output) {
  output$plot1 <- renderPlot(
    I_A %>% 
      filter(Cl == input$Class) %>% 
    ggplot(aes(x = year, y = `Per Set`)) +
      geom_col(fill = "#FF6666") +
      scale_x_continuous(breaks = c(2011:2019)) +
      theme_bw() +
      labs(title = "Average Aces Per Set by Class Years",
           subtitle = "Years 2011-2019",
           x = "Year",
           y = "Average Aces Per Set")
  )
  output$plot2 <-renderPlot(
    I_Ast %>% 
      filter(Cl == input$Class) %>% 
      ggplot(aes(x = year, y = `Per Set`)) +
      geom_col(fill = "#FF9933") +
      scale_x_continuous(breaks = c(2011:2019)) +
      theme_bw() +
      labs(title = "Average Assists Per Set by Class Years",
           subtitle = "Years 2011-2019",
           x = "Year",
           y = "Average Assists Per Set")
  )
    output$plot3 <-renderPlot(
      I_Att %>% 
        filter(Cl == input$Class) %>% 
        ggplot(aes(x = year, y = `Per Set`)) +
        geom_col(fill = "#FFCC00") +
        scale_x_continuous(breaks = c(2011:2019)) +
        theme_bw() +
        labs(title = "Average Attacks Per Set by Class Years",
             subtitle = "Years 2011-2019",
             x = "Year",
             y = "Average Attacks Per Set")
  )
    output$plot4 <-renderPlot(
      I_B %>% 
        filter(Cl == input$Class) %>% 
        ggplot(aes(x = year, y = `Per Set`)) +
        geom_col(fill = "#33CC66") +
        scale_x_continuous(breaks = c(2011:2019)) +
        theme_bw() +
        labs(title = "Average Blocks Per Set by Class Years",
             subtitle = "Years 2011-2019",
             x = "Year",
             y = "Average Blocks Per Set")
    )
    
    output$plot5 <-renderPlot(
      I_D %>% 
        filter(Cl == input$Class) %>% 
        ggplot(aes(x = year, y = `Per Set`)) +
        geom_col(fill = "#009966") +
        scale_x_continuous(breaks = c(2011:2019)) +
        theme_bw() +
        labs(title = "Average Digs Per Set by Class Years",
             subtitle = "Years 2011-2019",
             x = "Year",
             y = "Average Digs Per Set")
    )
    
    output$plot6 <-renderPlot(
      I_HP %>% 
        filter(Cl == input$Class) %>% 
        ggplot(aes(x = year, y = `Pct.`)) +
        geom_col(fill = "#00CCCC") +
        scale_x_continuous(breaks = c(2011:2019)) +
        theme_bw() +
        labs(title = "Average Hitting Percentage by Class Years",
             subtitle = "Years 2011-2019",
             x = "Year",
             y = "Average Hitting Percentage")
    )

    output$plot7 <-renderPlot(
      I_K %>% 
        filter(Cl == input$Class) %>% 
        ggplot(aes(x = year, y = `Per Set`)) +
        geom_col(fill = "#0066CC") +
        scale_x_continuous(breaks = c(2011:2019)) +
        theme_bw() +
        labs(title = "Average Kills Per Set by Class Years",
             subtitle = "Years 2011-2019",
             x = "Year",
             y = "Average Kills Per Set")
    )
    
    output$plot8 <-renderPlot(
      I_Pts %>% 
        filter(Cl == input$Class) %>% 
        ggplot(aes(x = year, y = `Per Set`)) +
        geom_col(fill = "#003399") +
        scale_x_continuous(breaks = c(2011:2019)) +
        theme_bw() +
        labs(title = "Average Pointss Per Set by Class Years",
             subtitle = "Years 2011-2019",
             x = "Year",
             y = "Average Points Per Set")
    )
    
    output$plot9 <-renderPlot(
      I_SA %>% 
        filter(Cl == input$Class) %>% 
        ggplot(aes(x = year, y = `Per Set`)) +
        geom_col(fill = "#9966FF",
                 width = 0.4) +
        scale_x_continuous(breaks = c(2011:2019)) +
        theme_bw() +
        labs(title = "Average Service Aces Per Set by Class Years",
             subtitle = "Years 2011-2019",
             x = "Year",
             y = "Average Service Aces Per Set")
    )
    
# Output for the table in the "Table" subpanel of the "By Teams" panel.
# Rather than having all the tables for each category be displayed
# like for the "By Individual Players" panel, I decided that
# it would be better for there to be a single table showing every 
# category's top 10 teams for every available year.
    
    
      output$table <- renderTable(
        
# I am embedding if() else() statements to render a table
# depending on the selected category using the dropdown
# options.
        
      if(input$Category == "Aces")
      {T_A %>% 
        filter(year == input$Year) %>%
        mutate(`Aces per Set` = `Per Set`) %>% 
        summarize(Team, `Aces per Set`) %>% 
        slice(1:10)
      }
      
     else(if(input$Category == "Assists"){
      T_Ast %>% 
        filter(year == input$Year) %>%
        mutate(`Assists per Set` = `Per Set`) %>% 
        summarize(Team, `Assists per Set`) %>% 
        slice(1:10)
     }
      
     else(if(input$Category == "Attacks"){
       T_Att %>% 
         filter(year == input$Year) %>%
         mutate(`Attacks per Set` = `Per Set`) %>% 
         summarize(Team, `Attacks per Set`) %>% 
         slice(1:10)
     }
     
     else(if(input$Category == "Blocks"){
       T_B %>% 
         filter(year == input$Year) %>%
         mutate(`Blocks per Set` = `Per Set`) %>% 
         summarize(Team, `Blocks per Set`) %>% 
         slice(1:10)
     }
     
     else(if(input$Category == "Digs"){
       T_D %>% 
         filter(year == input$Year) %>%
         mutate(`Digs per Set` = `Per Set`) %>% 
         summarize(Team, `Digs per Set`) %>% 
         slice(1:10)
     }
     
     else(if(input$Category == "Hitting Percentage"){
       T_HP %>% 
         filter(year == input$Year) %>%
         
# Here I am converting the Pct. column to an actual percentage
# and rename it to "Hitting Percentage" for ease to understand
# when the column name is displayed in the table.
         
         mutate(`Hitting Percentage` = pct(`Pct.` * 100)) %>% 
         summarize(Team, `Hitting Percentage`) %>% 
         slice(1:10)
     }
     
     else(if(input$Category == "Kills"){
       T_K %>% 
         filter(year == input$Year) %>%
         mutate(`Kills per Set` = `Per Set`) %>% 
         summarize(Team, `Kills per Set`) %>% 
         slice(1:10)
     }
     
     else(if(input$Category == "Service Aces"){
       T_SA %>% 
         mutate(`Per Set` == round(Aces/S, 2)) %>% 
         filter(year == input$Year) %>%
         mutate(`Service Aces per Set` = `Per Set`) %>% 
         summarize(Team, `Service Aces per Set`) %>% 
         slice(1:10)
     }
     
     else(if(input$Category == "Win Rate"){
       T_WR %>% 
         filter(year == input$Year) %>%
         
# Here I am doing the same thing I did for "Hitting Percentage"
# since Win Rate is essentially the percentage of wins each team
# has per year(season).
         
         mutate(`Win Rate` = pct(`Pct.` * 100)) %>% 
         summarize(Team, `Win Rate`) %>% 
         slice(1:10)
     }
     
      else(
        
# Last else statement that leads me to the Opponent Hitting Percentage, 
# which measures how good the ranked team's opponents were at 
# scoring on average. The higher the opp hp,
# the tougher the opponents, and the higher ranked were the teams.
      
        
        if(input$Category == "Opponent Hitting Percentage"){
        T_OHP %>%  
        filter(year == input$Year) %>%
        rename(`Percentage` = pct(`Opp Pct` * 100)) %>% 
        summarize(Team, Percentage) %>% 
        slice(1:10)
        }
      
     ))))))))))
    
      
}

shinyApp(ui = ui, server = server)