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
library(shinythemes)
library(gt)
library(gtsummary)


#source(file = "soccer_model/fit4.R")
gather_RDS <-readRDS("gather.RDS")

fit_1_RDS <-readRDS("fit_1.RDS")
fit_2_RDS <-readRDS("fit_2.RDS")
fit_3_RDS <-readRDS("fit_3.RDS")
fit_4_RDS <-readRDS("fit_4.RDS")

fit_1_gt <-readRDS("fit_1_Barcelona_gt.RDS")
fit_2_gt <-readRDS("fit_2_Barcelona_gt.RDS")
fit_3_gt <-readRDS("fit_3_Barcelona_gt.RDS")
fit_4_gt <-readRDS("fit_4_Barcelona_gt.RDS")

fit_model_PLtable <-readRDS("fit_model_PLtable.RDS")

final_gt_modeltable <-readRDS("final_gt_model_table.RDS")

# Define UI for application that draws a barplot
ui <- navbarPage(
    "Managing Expectations: xSoccer Data",
    tabPanel("E(x)pected Points Model Study: Premier League 19/20", 
             fluidPage(
                 titlePanel("Points Model Estimation"),
                 gt_output("tablePL")),
             h3("Table Interpretation"),
             p("The above table shows two alternative realities, or different 
               modelling outcomes. The first three columns (Pts Table, Pts, PPG)
               where PPG is points per game and Points(Pts) is based off of the 
               end of year outcomes for points gathered in the English Premier 
               League. The second set of three columns (xPts Table, xPts, xPPG)
               are based off of a metric called xPts. The third set of three 
               columns (Model Table, Model Pts, Model PPG) are based off of a 
               model generated output."),
             h3("xPts Discussion"),
             p("The xPts outcome is based on comparing the
               collected xG in a certain game and determining a winner based off
               of which team had a higher registered xG. The xG winner of a game
               receives 3 points, while the loser gets none. If the xG values
               are equal for both teams then they both get an xPts of 1.
               This process was repeated for all 38 games for each team
               throughout the season, and the accumulated results were 
               summarized in xPts columns above. It is obvious there is a 
               discrepancy between what actually happened (the actual table) and
               this xPts generated table. In general xG and xPts are not a perfect
               model. However, over time these metrics give an accurate 
               representation for how sustainable the performances of teams are,
               in terms of xG and thus in terms of actual Pts and xPts. For
               example, a team that generates a much higher xPts than their
               actual Pts can serve as a prediction for teams that are expected
               to do better in the future (perform closer to their xPts ) and
               vice versa if a team is underperforming their xG and thus their
               xPts they are bound to bounce back."),
             
             h2("Model Table and Analysis"),
             gt_output("tablePLmodel"),
             
             
             p("This regression table shows the Model-xPts based on 3 parameters:
               Team, Location game is being played (Home/Away), and xPts/game of
               each team. Here Arsenal for the Teams parameter and Away for the
               Home/Away parameter are the default values embedded in intercept,
               hence why they have blank values. The rest of the paramter values
               are based off of it. The model output here that I generated using
               the aforementioned parameters more closely resembles the actual
               end of league table compared to that of the xPts generated table.
               "),
    ),
    tabPanel("E(xG)pected Goals Model Study: Barcelona", 
             fluidPage(
                 titlePanel("Fitting A Model to Data"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "model_type",
                             "Pick Model",
                             c("Shots" = "Shots Only",
                               "xG" = "xG Only",
                               "xG & Shots" = "xG + Shots",
                               "xG & Shots Interaction" = "xG * Shots")
                         )),
                     mainPanel(plotOutput("model_plot")))),
             h2("Understanding xG"),
             p("xG is a metric used in the soccer world. It is a probabilistic
               measure for the likelihood of a goal being scored based on a shot
               taken and characteristics about that shot (location from goal,
               proximity of other players, etc.). These values are collected by
               people at Statsbomb such that every shot action has an associated
               xG or likelihood of scoring. As it is a probabilistic measure,
               there will be some discrepancy between the xG of a team (added up
               throughout a game) and their actual number of goals scored.
               The histogram here attempts to estimate the number of goals
               actually scored based on shots taken and the xG garnered by the
               Barcelona 14/15 team."),
             
             h2("Model Table and Analysis"),
             gt_output("table1"),
             
             p("The regression model here, shown as a histogram, outputs the
             expected number of goals scored per game by the Barcelona team in
             the La Liga competition in the 2014/15 season."),
             p("The data here was collected and compiled from all 38 games,
             19 home and 19 away, that Barcelona played in during the 2014/15 
             season. This is an example of the type of model that can be
             made for other teams and estimates the likelihood of number of
             goals scored given the xG and number of shots in a game."),
             p("All the histograms above (for all options) show similar results
             in estimating between 2.5 and 3 goals scored per game irrespective
             of the parameters selected. This is a specific case of Barcelona in
             this season that set averages of 2.4 xG and 16.4 shots per game."),
    ),
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
             for a soccer game between a Home and an Away Team. This data has
             the potential to be very informative as it can give an in depth
             overview of the events and actions taking place in a soccer game.
             This in turn allows analysts to contextualize and understand better
             the performance of their team."),
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
               to export tables, the data provided by Statsbomb is very detailed
               and wholesome, specifically their xG values, with them being
               recognized as having the most advanced and accurate model in the
               industry.."),
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
        ggplot(data = x, aes(x = Type, y = n, fill = Type)) +
            geom_col() +
            geom_label(aes(label = n)) +
            coord_flip() +
            theme_classic() +
            labs(title = "Team Event Data ",
                 subtitle = "This shows the number of each of these events recorded per team during a single game",
                 x = "Event",
                 y = "Frequency",
                 caption = "Source: Metrica Tracking") +
            theme(legend.position = "none")
        
    })
    
    output$model_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
        
        if (input$model_type == "Shots Only") {
            fit <- fit_2_RDS 
        } else if (input$model_type == "xG + Shots") {
            fit <- fit_1_RDS 
        } else if (input$model_type == "xG Only") {
            fit <- fit_3_RDS 
        } else {
            fit <- fit_4_RDS 
        }

        # Draw the histogram for the specified model
        
        ggplot(data = fit, aes(x = `1`)) +
            geom_histogram(bins = 100,
                           aes(y = after_stat(count/sum(count)))) +
            scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
            scale_x_continuous(labels = scales::number_format(accuracy = .001)) +
            theme_classic() +
            labs(title = "Posterior for Estimated Goals Scored in a Soccer Game",
                 subtitle = "This distribution is based on average values of xG and Number of Shots per game for the season.",
                 x = "Number of Goals Scored ",
                 y = "Probability",
                 caption = "Source: StatsBomb")

    })
    
    output$table1 <- render_gt({
        if (input$model_type == "Shots Only") {
            gt_table <- fit_2_gt 
        } else if (input$model_type == "xG + Shots") {
            gt_table <- fit_1_gt 
        } else if (input$model_type == "xG Only") {
            gt_table <- fit_3_gt 
        } else {
            gt_table <- fit_4_gt 
        }
        
        gt_table
    })
    
    output$tablePL <- render_gt({
        
        final_gt_modeltable
        
    })
    
    output$tablePLmodel <- render_gt({
        fit_model_PLtable
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
