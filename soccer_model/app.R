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

#source(file = "soccer_model/fit4.R")
gather_RDS <-readRDS("gather.RDS")

fit_1_RDS <-readRDS("fit_1.RDS")
fit_2_RDS <-readRDS("fit_2.RDS")
fit_3_RDS <-readRDS("fit_3.RDS")
fit_4_RDS <-readRDS("fit_4.RDS")

# Define UI for application that draws a barplot
ui <- navbarPage(
    "Soccer Data Science Final Project",
    tabPanel("Discussion",
             titlePanel("Data Sources Discussion"),
             h3("Data Source 1: StatsBomb"),
             p("This is the data source I will be using that has all events 
               data. It has data about competitions, lineups, events (passes,
               shots, tackles, dribbles) and any in match event data. It is yet
               to be determined whether I will need to pay or find some other 
               way to  access the full dataset by obtaining an API."),
             h3("Data Source 2: Metrica Sports"),
             p("This is the data source I will be using that contains position 
               and tracking data of players. The goal currently in my mind 
               for my final project is to emphasize and categorize players into 
               different 'role players'. For that goal, tracking data becomes 
               increasingly important as soccers' players movement and starting 
               position with respect and with or without the ball becomes as 
               paramount as the actions they complete with it that will be 
               recorded in the Statsbomb events dataset. Wyscout also has some 
               interesting data that I could leverage if I'm going to end up 
               getting an API.")),
    tabPanel("Manipulation", 
             fluidPage(
                 titlePanel("Game Events Bar Plot"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Pick Team",
                             # here we have A = a, where A is title and a is the
                             # option that is also specified in the server
                             c("Home" = "Home", "Away" = "Away")
                         )),
                     mainPanel(plotOutput("events_plot")))),
             p("The plot above shows the number of each type of event recorded 
             for a soccer game between a Home and an Away Team."),
    ),
    tabPanel("Model", 
             fluidPage(
                 titlePanel("Fitting A Model to Data"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "model_type",
                             "Pick Model",
                             c("Shots" = "Shots Only", "xG & Shots" = "xG + Shots")
                         )),
                     mainPanel(plotOutput("model_plot")))),
             p("The model here is one for number of goals scored based on two 
             possible inputs: xG and shots taken. This data was taken from all 
             38 games Barcelona Men's team played in La Liga in the 2014/15 
             season."),
    ),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project. I have not completely
             nailed down what I will be doing for my project. However, I would 
             like to use event and tracking data to allow an outsider to be able
             to search the game and visualize what happened. For example, if
             somebody is interested in goals scored. I would make it such that
             you can filter for such an event and specify the time before the 
             goal was scored and visualize the player and ball movements according
             to the tracking data available."),
             h3("About Me"),
             p("My name is Fahad Alkhaja and I study Mechanical Engineering. 
             You can reach me at falkhaja@college.harvard.edu."),
             h3("Repo URL"),
             p("https://github.com/Falkhaja/milestone4_finalproj_Gov_1005.git")))

# Define server logic
server <- function(input, output) {
    output$events_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
        
        ifelse(
            input$plot_type == "Home",
            
            # If input$plot_type is "Home", plot bar graph of home team events
            
            x   <- gather_RDS %>%
                filter(Team == "Home") %>%
                select(Type) %>% 
                count(Type),
            
            # If input$plot_type is "Away", plot bar graph of away team events
            
            x   <- gather_RDS %>%
                filter(Team == "Away") %>%
                select(Type) %>% 
                count(Type)
        )
        
        # Draw the barplot with the specified number of bins
        
        barplot(height = pull(x), names.arg = x$Type, horiz = FALSE,
                col = 'darkgray',
                border = 'black',
                main = "Team Event Data", cex.names = .75)
    })
    
    output$model_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
        
        ifelse(
            input$model_type == "Shots Only",
            
            # If input$plot_type is "Shots Only", we want fit_2
            fit <- fit_2_RDS %>%
                pull(`1`),
            
            # If input$plot_type is "xG + Shots", we want fit_1
            fit <- fit_1_RDS %>%
                pull(`1`)
            
        )
        
        # Draw the histogram for the specified model
        
        hist(fit, col = 'darkgray', border = 'white',
             freq = FALSE , breaks = 100,
             main = "Posterior Distribution for Goals scored in a Soccer Game",
             xlab = "Number of Goals", ylab = "Probability")
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
