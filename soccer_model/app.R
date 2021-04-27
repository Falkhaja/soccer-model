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
    "Managing Expectations: xSoccer Data",
    tabPanel("Player Tracking and Events", 
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
    tabPanel("E(xG)pected Goals Case Study: Barcelona", 
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
             p("The regression model here, shown as a histogram, outputs the
             expected number of goals scored per game by the Barcelona team in
             the La Liga competition in the 2014/15 season."),
             p("The data here was collected and compiled from all 38 games,
             19 home and 19 away, that Barcelona played in during the 2014/15 
             season. This is merely an example of the type of model that can be
             made for other teams."),
    ),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("I have long been infatutaed with soccer and watching and playing
               the game. In recent times, especially after the movie Moneyball,
               data analytics has been making great advances and strides in the
               soccer world. As an aspiring statistician and data scientist and
               more importantly an avid fan, soccer analytics has thus become 
               central to my enjoying and understanding of the different nuances
               of the game."),
             h3("Data Sources"),
             h4("StatsBomb"),
             p("Whether via the StatsBomb package API directly or the FBref
               website, which summarizes and organizes data in detailed and easy
               to export tables."),
             h4("Metrica Sports"),
             p("Metrica Sports recently released proprietary player events and
               corresponding tracking data. These events, in a simple format,
               filtered by Home and Away Team, are shown here to showcase the
               number for each of the different events."),
             h3("About Me"),
             p("My name is Fahad Alkhaja and I am a graduating senior from
             Harvard College with a BS in Mechanical Engineering 2021. I will be
             pursuing a MA in Statistics at Columbia's GSAS.
             You can reach me at fhd.alkhaja@gmail.com."),
             h3("Repo URL"),
             p("https://github.com/Falkhaja/soccer-model.git")))

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
        
        if (input$model_type == "Shots Only") {
            fit <- fit_2_RDS %>%
                pull(`1`)
        } else if (input$model_type == "xG + Shots") {
            fit <- fit_1_RDS %>%
                pull(`1`)
        }
        # } else if (test_expression3) {
        #     statement3
        # } else {
        #     statement4
        # }
        # ifelse(
        #     input$model_type == "Shots Only",
        #     
        #     # If input$plot_type is "Shots Only", we want fit_2
        #     fit <- fit_2_RDS %>%
        #         pull(`1`),
        #     
        #     # If input$plot_type is "xG + Shots", we want fit_1
        #     fit <- fit_1_RDS %>%
        #         pull(`1`)
        #     
        # )
        
        # Draw the histogram for the specified model
        
        hist(fit, col = 'darkgray', border = 'white',
             freq = FALSE , breaks = 100,
             main = "Posterior Distribution for Goals scored in a Soccer Game",
             xlab = "Number of Goals", ylab = "Probability")
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
