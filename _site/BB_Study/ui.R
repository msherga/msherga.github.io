#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# Load the libraries in R 
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(rmarkdown)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Beers & Breweries Case Study"),

    # Sidebar with a slider input for number of bins
    fluidRow(sidebarLayout(
        sidebarPanel(
            fileInput("file1", label = h3("Upload file \"brews.csv\" linked on msherga.github.io"), accept = c("text/csv",
                                                                    "text/comma-separated-values,text/plain",
                                                                    ".csv")),
            
         
            hr(),
            
            
            
            selectInput("plot",
                        "Choose a plot:",
                        c("Qty of Breweries", 
                          "ABV Histogram",
                                    "ABV Boxplot",
                                    "IBU Histogram",
                                    "IBU Boxplot",
                                    "ScatterPlot of % ABV vs IBU"
                          )
                        ),
            hr(),
            conditionalPanel(
                condition = "input.plot == 'ScatterPlot of % ABV vs IBU'",
                radioButtons("regline", "Include regression line?",
                            list("Yes", "No"), selected = "Yes"),
                hr()),
            
            
            selectInput("States",
                        "Change which states you want included by editing the box below (click and delete, or backspace just like text):", multiple = TRUE, selected = c( "AK",
                                                                                                 "AL",
                                                                                                 "AR",
                                                                                                 "AZ",
                                                                                                 "CA",
                                                                                                 "CO",
                                                                                                 "CT",
                                                                                                 "DC",
                                                                                                 "DE",
                                                                                                 "FL",
                                                                                                 "GA",
                                                                                                 "HI",
                                                                                                 "IA",
                                                                                                 "ID",
                                                                                                 "IL",
                                                                                                 "IN",
                                                                                                 "KS",
                                                                                                 "KY",
                                                                                                 "LA",
                                                                                                 "MA",
                                                                                                 "MD",
                                                                                                 "ME",
                                                                                                 "MI",
                                                                                                 "MN",
                                                                                                 "MO",
                                                                                                 "MS",
                                                                                                 "MT",
                                                                                                 "NC",
                                                                                                 "ND",
                                                                                                 "NE",
                                                                                                 "NH",
                                                                                                 "NJ",
                                                                                                 "NM",
                                                                                                 "NV",
                                                                                                 "NY",
                                                                                                 "OH",
                                                                                                 "OK",
                                                                                                 "OR",
                                                                                                 "PA",
                                                                                                 "RI",
                                                                                                 "SC",
                                                                                                 "SD",
                                                                                                 "TN",
                                                                                                 "TX",
                                                                                                 "UT",
                                                                                                 "VA",
                                                                                                 "VT",
                                                                                                 "WA",
                                                                                                 "WI",
                                                                                                 "WV",
                                                                                                 "WY"
                        ),
                        c( "AK",
                           "AL",
                           "AR",
                           "AZ",
                           "CA",
                           "CO",
                           "CT",
                           "DC",
                           "DE",
                           "FL",
                           "GA",
                           "HI",
                           "IA",
                           "ID",
                           "IL",
                           "IN",
                           "KS",
                           "KY",
                           "LA",
                           "MA",
                           "MD",
                           "ME",
                           "MI",
                           "MN",
                           "MO",
                           "MS",
                           "MT",
                           "NC",
                           "ND",
                           "NE",
                           "NH",
                           "NJ",
                           "NM",
                           "NV",
                           "NY",
                           "OH",
                           "OK",
                           "OR",
                           "PA",
                           "RI",
                           "SC",
                           "SD",
                           "TN",
                           "TX",
                           "UT",
                           "VA",
                           "VT",
                           "WA",
                           "WI",
                           "WV",
                           "WY"
                        )
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Plot", plotOutput("distPlot")),
                tabPanel("Uploaded Dataset", tableOutput("contents"))
                
            ))
            
        ))
    )
)


