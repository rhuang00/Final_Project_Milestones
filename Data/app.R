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
# library(RColorBrewer)
# library(wordcloud)
# library(wordcloud2)
library(htmltools)

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

# read in win rate for the word cloud

# Digs <- read_rds("raw_data/Digs.rds")

# Assists <- read_rds("raw_data/Ast.rds")

# Attacks <- read_rds("raw_data/Att.rds")

# Aces <- read_rds("raw_data/Aces.rds")

# Kills <- read_rds("raw_data/Kills.rds")

# Blocks <- read_rds("raw_data/Blocks.rds")

# "Hitting Percentage" <- read_rds("raw_data/HP.rds")

# "Opponent Hitting Percentage" <- read_rds( "raw_data/OHP.rds")

# "Win Rate" <- read_rds("raw_data/WR.rds")

# "Service Aces" <- read_rds("raw_data/SA.rds")


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


only_text <- function(file){
  
  file %>% 
    
    # Here I am separating the team names from the conference names.
    
    mutate(conference = strsplit(Team, "[()]")) %>% 
    
    # It took me quite a long time to figure out how to
    # only keep the conference names/team names since
    # the each row was a list, but after googling examples I figured it out!
    
    mutate(conference = lapply(conference,'[[', 2)) %>% 
    mutate(Team = strsplit(Team, "[()]")) %>% 
    mutate(Team = lapply(Team, '[[', 1))
}
  

ui <- navbarPage(#tags$head(
     #tags$style(
     # HTML('* {font-family: Garamond};'))),
    "NCAA Women's Volleyball Performance",
    theme = shinytheme("cerulean"),
    tabPanel("About",
             
# Loading in separately-written html page.


      includeHTML("About.html"),

# Loading css and javascript. I originally has sass too, but
# the code did not work out, so I had to take them out.
# The images in html won't load properly on shinyapp, so I removed
# the images as well. I will try to add them back in later, but
# the images for the About page are not a top priority at the moment.

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

# First panel with graphics, used for data sets containing rankings of 
# individual players. Unfortunately a lot of data was missing (per season),
# and although I tried to make up for that by calculating some stats using 
# the guidebook's formulas, I could not make up for the lack of data for an
# entire year/season.

    tabPanel("By Individual Players", 
             
                 h2("Performance across the country", align = "center"),
             
                 p("Here we have graphs displaying the average performance
                   of individually ranked players in every skill category."),
             
                 sidebarLayout(position = "left",
                               
                   sidebarPanel(width = 2,
                                
# Interactive feature: choose the class year for each graphics to see the average 
# performance of ranked players in that class year for each year/season.

                selectInput(inputId = "Class",
                                   label = "Select Class:",
                                   choices = c("Sophomore" = "So.", 
                                               "Junior" = "Jr.",
                                               "Senior" = "Sr."))#,
                #selectInput(inputId = "ConferenceFinder",
                            # label = "Conference:",
                            # choices = c("School Names", 
                                         #"Conference Names",
                                         #""),
                            # selected = "All")
                
               ), 
               mainPanel(
                 fluidRow(

# I played around with the layout and this was the most comfortable visually
# for now.
                   
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
  

# Second panel with graphics featuring data from the Team ranking data sets.

    tabPanel("By Teams",
             
             tabsetPanel(
               tabPanel("Top Teams", 
             sidebarLayout(
               
# Instead of class year, the interactive feature is to choose the year/season.
               
               sidebarPanel(width = 2,
                 selectInput(inputId = "Year",
                             label = "Year:",
                             choices = c(2011:2019))#,
            # selectInput("Category",
                        # "Category:",
                        # c("Aces",
                        #   "Assists",
                         #  "Attacks",
                         #  "Blocks",
                          # "Digs",
                         #  "Hitting Percentage",
                         #  "Kills",
                         #  "Opponent Hitting Percentage",
                         #  "Service Aces",
                          # "Win Rate",
                         #  selected = "Aces"
                         #)
                         #)
                          ),
                mainPanel(h4("Top 10 Teams per Category", align = "center"),
                          fluidRow(
                            
# There is an odd number of tables, so i ended up creating 4 columns and having
# the last table be displayed on the first row. 
                            
                            column(4,
                                   tableOutput(outputId = "table1"),
                                   tableOutput(outputId = "table2"), 
                                   tableOutput(outputId = "table3")),
                            column(4,
                                   tableOutput(outputId = "table4"),
                                   tableOutput(outputId = "table5"),
                                   tableOutput(outputId = "table6")),
                            column(4,
                                   tableOutput(outputId = "table7"),
                                   tableOutput(outputId = "table8"),
                                   tableOutput(outputId = "table9")),
                            column(4,
                                   tableOutput(outputId = "table10"))
                          )
                )
               )
             ),

# Using the same data sets, but filtering for the conference names rather than
# team names (Which contain the conference they compete in).

             tabPanel("Top Conferences",
                      sidebarLayout(
                      sidebarPanel(width = 2,
                        selectInput(inputId = "Year2",
                                    label = "Year:",
                                    choices = c(2011:2019))#,
                      #selectInput("Category2",
                                 # "Category:",
                                 # c("Aces",
                                 #   "Assists",
                                 #   "Attacks",
                                 #   "Blocks",
                                  #  "Digs",
                                  #  "Hitting Percentage",
                                  #  "Kills",
                                  #  "Opponent Hitting Percentage",
                                  #  "Service Aces",
                                  #  "Win Rate",
                                  #  selected = "Aces"
                                  #)
                      #)
                      ),
                      mainPanel(h4("Top 10 Conferences per Category", align = "center"),
                                fluidRow(
                                  column(4,
                                         tableOutput(outputId = "table11"),
                                         tableOutput(outputId = "table12"), 
                                         tableOutput(outputId = "table13")),
                                  column(4,
                                         tableOutput(outputId = "table14"),
                                         tableOutput(outputId = "table15"),
                                         tableOutput(outputId = "table16")),
                                  column(4,
                                         tableOutput(outputId = "table17"),
                                         tableOutput(outputId = "table18"),
                                         tableOutput(outputId = "table19")),
                                  column(4,
                                         tableOutput(outputId = "table20"))
                                )
                      )
             )
             )
             ))
             ,
                 
                 # wordcloud showing frequency of each team to be
                 # ranked, per skill set -> a wordcloud for 
                 # every category
                 # Potentially a measurement of popularity??
                 # do both first
                 
    tabPanel("Model", 
             tabsetPanel(
             tabPanel(
               "Individual Players",
               h2("Regression Model 1", align = "center"),
              plotOutput("regression1"),
              p("This model is a linear regression between the total number of points 
                scored by top players and the total number of kills they scored."),
              p("Based on the regression line, we can assume that there is a 
                positive relationship between kills and overall points scored."),
             # first regression: class year vs performance 
             # per skill set
             # second regression: performance over time per
             # skill set based on region/conference/school
             h2("Regression Model 2", align = "center"),
             plotOutput("regression2")
             ),
             tabPanel(
               "Teams",
               h2("Regression Model 1", align = "center"),
               plotOutput("regression"),
              # p("")
               h2("Regression Model 2", align = "center"),
               plotOutput("regression3")
               # potentially a regression on W-L rates ,
               # likelihood of each school that was ranked to
               # be ranked again (choose top 25 schools?)
               # regression of performance over time per skill
               # set based on regions, potentially divided into
               # within each conference, and then national 
               # rankings
             )
             ))
    )



# regression models

#fit_1 <- lm(Aces ~ Cl + Pos,
            #I_A)

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

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
    
    
      output$table1 <- renderTable({
        T_A %>% 
        filter(year == input$Year) %>%
        mutate(`Aces per Set` = `Per Set`) %>% 
        summarize(Team, `Aces per Set`) %>% 
        slice(1:10)
      }
      )
      
     output$table2 <- renderTable({
      T_Ast %>% 
        filter(year == input$Year) %>%
        mutate(`Assists per Set` = `Per Set`) %>% 
        summarize(Team, `Assists per Set`) %>% 
        slice(1:10)
     }
     )
      
     output$table3 <- renderTable({
       T_Att %>% 
         filter(year == input$Year) %>%
         mutate(`Attacks per Set` = `Per Set`) %>% 
         summarize(Team, `Attacks per Set`) %>% 
         slice(1:10)
     }
     )
     
     output$table4 <- renderTable({
       T_B %>% 
         filter(year == input$Year) %>%
         mutate(`Blocks per Set` = `Per Set`) %>% 
         summarize(Team, `Blocks per Set`) %>% 
         slice(1:10)
     }
     )
     
     output$table5 <- renderTable({
       T_D %>% 
         filter(year == input$Year) %>%
         mutate(`Digs per Set` = `Per Set`) %>% 
         summarize(Team, `Digs per Set`) %>% 
         slice(1:10)
     }
     )
     
     output$table6 <- renderTable({
       T_HP %>% 
         filter(year == input$Year) %>%
         
# Here I am converting the Pct. column to an actual percentage
# and rename it to "Hitting Percentage" for ease to understand
# when the column name is displayed in the table.
         
         mutate(`Hitting Percentage` = pct(`Pct.` * 100)) %>% 
         summarize(Team, `Hitting Percentage`) %>% 
         slice(1:10)
     }
)
     
      output$table7 <- renderTable({
       T_K %>% 
         filter(year == input$Year) %>%
         mutate(`Kills per Set` = `Per Set`) %>% 
         summarize(Team, `Kills per Set`) %>% 
         slice(1:10)
      }
      )
     
      output$table8 <- renderTable({
       T_SA %>% 
         mutate(`Service Aces per Set` = round(Aces/S, 2)) %>% 
         filter(year == input$Year) %>%
         summarize(Team, `Service Aces per Set`) %>% 
         slice(1:10)
     }
      )
      
     output$table9 <- renderTable({
       T_WR %>% 
         filter(year == input$Year) %>%
         
# Here I am doing the same thing I did for "Hitting Percentage"
# since Win Rate is essentially the percentage of wins each team
# has per year(season).
         
         mutate(`Win Rate` = pct(`Pct.` * 100)) %>% 
         summarize(Team, `Win Rate`) %>% 
         slice(1:10)
     }
     )
        
# Last else statement that leads me to the Opponent Hitting Percentage, 
# which measures how good the ranked team's opponents were at 
# scoring on average. The higher the opp hp,
# the tougher the opponents, and the higher ranked were the teams.
      
        
        output$table10 <- renderTable({
        T_OHP %>%  
        filter(year == input$Year) %>%
        mutate(`Opp Hitting Percentage` = pct(`Opp Pct` * 100)) %>% 
        summarize(Team, `Opp Hitting Percentage`) %>% 
        slice(1:10)
        }
     )
    
     output$table11 <- renderTable({
       only_text(T_A) %>% 
             filter(year == input$Year2) %>%
             group_by(conference) %>% 
             mutate(`Aces per Set` = `Per Set`) %>% 
             summarize(mean(`Aces per Set`), .groups = "drop") %>% 
             slice(1:10)
         }
     )
         output$table12 <- renderTable({
           only_text(T_Ast) %>% 
             filter(year == input$Year2) %>%
             mutate(`Assists per Set` = `Per Set`) %>% 
             group_by(conference) %>% 
             summarize(mean(`Assists per Set`), .groups = "drop") %>% 
             slice(1:10)
         }
         )
         
        output$table13 <- renderTable({
           only_text(T_Att) %>% 
             filter(year == input$Year2) %>%
             mutate(`Attacks per Set` = `Per Set`) %>% 
             group_by(conference) %>% 
             summarize(mean(`Attacks per Set`), .groups = "drop") %>% 
             slice(1:10)
         }
        )
        
         output$table14 <- renderTable({
           only_text(T_B) %>% 
             filter(year == input$Year2) %>%
             mutate(`Blocks per Set` = `Per Set`) %>% 
             group_by(conference) %>% 
             summarize(mean(`Blocks per Set`), .groups = "drop") %>% 
             slice(1:10)
         }
         )
         
         output$table15 <- renderTable({
           only_text(T_D) %>% 
             filter(year == input$Year2) %>%
             mutate(`Digs per Set` = `Per Set`) %>%
             group_by(conference) %>% 
             summarize(mean(`Digs per Set`), .groups = "drop") %>% 
             slice(1:10)
         }
         )
         
         output$table16 <- renderTable({
           only_text(T_HP) %>% 
             filter(year == input$Year2) %>% 
             group_by(conference) %>% 
             summarize(mean(`Pct.`), .groups = "drop") %>% 
             slice(1:10)
         }
         )
         
         output$table17 <- renderTable({
           only_text(T_K)%>% 
             filter(year == input$Year2) %>%
             mutate(`Kills per Set` = `Per Set`) %>% 
             group_by(conference) %>% 
             summarize(mean(`Kills per Set`), .groups = "drop") %>% 
             slice(1:10)
         }
         )
         
         output$table18 <- renderTable({
           only_text(T_SA) %>% 
             mutate(`Service Aces per Set` = round(Aces/S, 2)) %>%
             filter(year == input$Year2) %>%
             group_by(conference) %>% 
             summarize(mean(`Service Aces per Set`), .groups = "drop") %>% 
             slice(1:10)
         }
         )
         
         output$table19 <- renderTable({
           only_text(T_WR) %>% 
             filter(year == input$Year2) %>%
             group_by(conference) %>%  
             summarize(mean(`Pct.`), .groups = "drop")%>% 
             slice(10)
         }
         )
         
        
           
           
           output$table20 <- renderTable({
             only_text(T_OHP) %>%  
               filter(year == input$Year2) %>%
               mutate(`Opp Hitting Percentage` = pct(`Opp Pct` * 100)) %>% 
               group_by(conference) %>% 
               summarize(mean(`Opp Hitting Percentage`), .groups = "drop")%>% 
               slice(1:10)
           })
 

     output$regression1 <- renderPlot(
       {
        ggplotRegression(lm(Pts ~ Kills, I_Pts) )
       }
     )
     output$regression2 <- renderPlot(
       {
         ggplotRegression(lm(Pts ~ Pos, I_Pts))
       }
     )
     
     output$regression <- renderPlot(
       {
        
         ggplotRegression(lm(`Pct.` ~ Kills, T_HP))
       }
     )
     
     output$regression3 <- renderPlot(
       {
         ggplotRegression(lm(`Pct.` ~ Errors, T_HP))
       }
     )
     }

shinyApp(ui = ui, server = server)