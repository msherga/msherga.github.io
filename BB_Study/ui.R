library(shiny)

states <- sort(c(state.abb, "DC"))

shinyUI(fluidPage(
    titlePanel("Beers & Breweries Case Study"),

    fluidRow(
        sidebarLayout(
            sidebarPanel(
                helpText(
                    "The built-in brews.csv dataset loads automatically from GitHub."
                ),
                fileInput(
                    "file1",
                    label = "Optional: upload a compatible CSV for this session",
                    accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv"
                    )
                ),

                hr(),

                selectInput(
                    "plot",
                    "Choose a plot:",
                    c(
                        "Qty of Breweries",
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
                    radioButtons(
                        "regline",
                        "Include regression line?",
                        c("Yes", "No"),
                        selected = "Yes"
                    ),
                    hr()
                ),

                selectInput(
                    "States",
                    paste(
                        "Choose which states to include.",
                        "Click a state and press Backspace or Delete to remove it."
                    ),
                    choices = states,
                    selected = states,
                    multiple = TRUE
                )
            ),

            mainPanel(
                tabsetPanel(
                    tabPanel("Plot", plotOutput("distPlot")),
                    tabPanel("Dataset", tableOutput("contents"))
                )
            )
        )
    )
))
