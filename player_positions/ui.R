#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Position Density"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("plyrA",
                        label = "Player Name:",
                        choices = c("Armon","Brent","Connor","Jimi",
                                    "Landon","Quinn R", "Quinlan", 
                                    "Shane", "Truman", "Zack"),
                        selected=NULL,
                        multiple = FALSE),
            textInput("g", label = "Grep for Games (i.e., g)",
                      value = c("t"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("ggSoccerPlot")
        )
    )
))
