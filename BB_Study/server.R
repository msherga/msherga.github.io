library(shiny)
library(dplyr)
library(ggplot2)

# GitHub is the source of truth for the built-in datasets.
default_data_url <- paste0(
    "https://raw.githubusercontent.com/msherga/msherga.github.io/",
    "refs/heads/sherga-redesign/brews.csv"
)

default_breweries_url <- paste0(
    "https://raw.githubusercontent.com/msherga/msherga.github.io/",
    "refs/heads/sherga-redesign/Breweries.csv"
)

required_columns <- c("State", "Percent_ABV", "IBU")

read_brews <- function(source) {
    df <- tryCatch(
        read.csv(
            file = source,
            header = TRUE,
            sep = ",",
            stringsAsFactors = FALSE,
            check.names = FALSE
        ),
        error = function(error) NULL
    )

    validate(
        need(!is.null(df), "The dataset could not be loaded."),
        need(
            all(required_columns %in% names(df)),
            paste(
                "The dataset must contain these columns:",
                paste(required_columns, collapse = ", ")
            )
        )
    )

    df$State <- trimws(df$State)
    df$Percent_ABV <- suppressWarnings(as.numeric(df$Percent_ABV))
    df$IBU <- suppressWarnings(as.numeric(df$IBU))
    df
}

read_original_breweries <- function() {
    breweries <- tryCatch(
        read.csv(
            file = default_breweries_url,
            header = TRUE,
            sep = ",",
            stringsAsFactors = FALSE,
            check.names = FALSE
        ),
        error = function(error) NULL
    )

    validate(
        need(!is.null(breweries), "The original brewery dataset could not be loaded."),
        need("State" %in% names(breweries), "The brewery dataset must contain State.")
    )

    breweries$State <- trimws(breweries$State)
    breweries
}

summary_values <- function(x) {
    values <- summary(x)
    data.frame(
        stats = c("Min", "1st Q", "Med", "Mean", "3rd Q", "Max"),
        value = round(as.numeric(values[1:6]), 2),
        stringsAsFactors = FALSE
    )
}

shinyServer(function(input, output, session) {
    dataset <- reactive({
        source <- if (is.null(input$file1)) {
            default_data_url
        } else {
            input$file1$datapath
        }

        read_brews(source)
    })

    output$contents <- renderTable({
        dataset()
    })

    output$distPlot <- renderPlot({
        validate(
            need(length(input$States) > 0, "Select at least one state.")
        )

        df <- dataset()
        df <- df[df$State %in% input$States, , drop = FALSE]

        validate(
            need(nrow(df) > 0, "No rows match the selected states.")
        )

        if (input$plot == "Qty of Breweries") {
            # Match the original case study exactly. The original Brewery.csv
            # contains one row per brewery; brews.csv contains many beers per brewery.
            if (is.null(input$file1)) {
                breweries <- read_original_breweries()
                breweries <- breweries[
                    breweries$State %in% input$States,
                    ,
                    drop = FALSE
                ]

                brewbystate <- breweries %>%
                    count(State, name = "n") %>%
                    arrange(desc(n))
            } else if ("Brewery_id" %in% names(df)) {
                brewbystate <- df %>%
                    filter(!is.na(Brewery_id)) %>%
                    distinct(State, Brewery_id) %>%
                    count(State, name = "n") %>%
                    arrange(desc(n))
            } else if ("Brewery" %in% names(df)) {
                brewbystate <- df %>%
                    filter(!is.na(Brewery), trimws(Brewery) != "") %>%
                    distinct(State, Brewery) %>%
                    count(State, name = "n") %>%
                    arrange(desc(n))
            } else {
                validate(
                    need(
                        FALSE,
                        "An uploaded dataset must contain Brewery_id or Brewery to count breweries."
                    )
                )
            }

            validate(
                need(nrow(brewbystate) > 0, "No breweries match the selected states.")
            )

            return(
                ggplot(
                    brewbystate,
                    aes(x = reorder(as.factor(State), -n), y = n)
                ) +
                    labs(y = "# of Breweries", x = "State") +
                    geom_col(fill = "goldenrod3") +
                    geom_label(aes(label = n), size = 3, vjust = 1) +
                    ggtitle("Breweries by State") +
                    theme(plot.title = element_text(hjust = 0.5))
            )
        }

        if (input$plot == "ScatterPlot of % ABV vs IBU") {
            plot <- ggplot(
                df,
                aes(x = IBU, y = Percent_ABV, color = Percent_ABV)
            ) +
                geom_point() +
                ylim(2.5, 13) +
                theme_dark() +
                labs(title = "% ABV vs IBU") +
                theme(plot.title = element_text(hjust = 0.5)) +
                scale_color_gradient(low = "gold2", high = "white")

            if (identical(input$regline, "Yes")) {
                plot <- plot + stat_smooth(method = "lm", color = "red")
            }

            return(plot)
        }

        if (input$plot %in% c("ABV Histogram", "ABV Boxplot")) {
            values <- df$Percent_ABV[!is.na(df$Percent_ABV)]
            validate(need(length(values) > 0, "No ABV values are available for this selection."))
            stats <- summary_values(values)

            if (input$plot == "ABV Histogram") {
                return(
                    ggplot(data.frame(Percent_ABV = values), aes(x = Percent_ABV)) +
                        geom_density(aes(color = "red", linewidth = 1)) +
                        geom_histogram(aes(y = after_stat(density), alpha = 0.2)) +
                        labs(
                            x = "% ABV",
                            title = "Distribution of ABV Percentages",
                            y = "Density"
                        ) +
                        theme(
                            plot.title = element_text(hjust = 0.5),
                            legend.position = "none"
                        ) +
                        annotate(
                            "text",
                            x = stats$value,
                            y = 0.6,
                            label = stats$value
                        ) +
                        annotate(
                            "text",
                            x = stats$value,
                            y = 0.63,
                            label = stats$stats
                        ) +
                        ylim(0, 0.63) +
                        xlim(min(stats$value), max(stats$value))
                )
            }

            return(
                ggplot(data.frame(Percent_ABV = values), aes(x = Percent_ABV)) +
                    geom_boxplot() +
                    labs(x = "% ABV", title = "Distribution of ABV Percentages") +
                    theme(
                        plot.title = element_text(hjust = 0.5),
                        legend.position = "none",
                        axis.text.y = element_blank(),
                        axis.title.y = element_blank(),
                        axis.ticks.y = element_blank()
                    ) +
                    geom_point(aes(x = stats$value[4], y = 0), colour = "red")
            )
        }

        if (input$plot %in% c("IBU Histogram", "IBU Boxplot")) {
            values <- df$IBU[!is.na(df$IBU)]
            validate(need(length(values) > 0, "No IBU values are available for this selection."))
            stats <- summary_values(values)

            if (input$plot == "IBU Histogram") {
                return(
                    ggplot(data.frame(IBU = values), aes(x = IBU)) +
                        geom_density(aes(color = "red", linewidth = 1)) +
                        geom_histogram(aes(y = after_stat(density), alpha = 0.2)) +
                        labs(x = "IBU", title = "Distribution of IBU", y = "Density") +
                        theme(
                            plot.title = element_text(hjust = 0.5),
                            legend.position = "none"
                        ) +
                        annotate(
                            "text",
                            x = stats$value,
                            y = 0.2,
                            label = stats$value
                        ) +
                        annotate(
                            "text",
                            x = stats$value,
                            y = 0.21,
                            label = stats$stats
                        ) +
                        ylim(0, 0.23) +
                        xlim(min(stats$value), max(stats$value))
                )
            }

            return(
                ggplot(data.frame(IBU = values), aes(x = IBU)) +
                    geom_boxplot() +
                    labs(x = "IBU", title = "Distribution of IBU") +
                    theme(
                        plot.title = element_text(hjust = 0.5),
                        legend.position = "none",
                        axis.text.y = element_blank(),
                        axis.title.y = element_blank(),
                        axis.ticks.y = element_blank()
                    ) +
                    geom_point(aes(x = stats$value[4], y = 0), colour = "red")
            )
        }
    })
})
