library(shiny)
library(dplyr)
library(ggplot2)

# GitHub is the source of truth for the built-in dataset.
default_data_url <- paste0(
    "https://raw.githubusercontent.com/msherga/msherga.github.io/",
    "refs/heads/sherga-redesign/brews.csv"
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
        df <- dataset()

        validate(
            need(length(input$States) > 0, "Select at least one state.")
        )

        df <- df[df$State %in% input$States, , drop = FALSE]

        validate(
            need(nrow(df) > 0, "No rows match the selected states.")
        )

        # Count unique breweries, not beer rows. The merged dataset contains
        # multiple beers for each brewery.
        if ("Brewery_id" %in% names(df)) {
            brewbystate <- df %>%
                distinct(State, Brewery_id) %>%
                count(State, name = "n") %>%
                arrange(desc(n))
        } else if ("Brewery" %in% names(df)) {
            brewbystate <- df %>%
                distinct(State, Brewery) %>%
                count(State, name = "n") %>%
                arrange(desc(n))
        } else {
            validate(
                need(
                    FALSE,
                    "The dataset must contain Brewery_id or Brewery to count breweries."
                )
            )
        }

        brewbystate <- data.frame(brewbystate)

        brewbystateplot <- brewbystate %>%
            ggplot(aes(x = reorder(as.factor(State), -n), y = n)) +
            labs(y = "# of Breweries", x = "State") +
            geom_bar(stat = "identity", position = "dodge", fill = "goldenrod3") +
            geom_label(size = 3, label = brewbystate$n, vjust = 1) +
            ggtitle("Breweries by State") +
            theme(plot.title = element_text(hjust = 0.5))

        # Summary statistics and distribution of ABV
        abvsum <- summary(df$Percent_ABV)
        abvsum <- data.frame(table(abvsum))
        abvsum$stats[1] <- "Min"
        abvsum$stats[2] <- "1st Q"
        abvsum$stats[3] <- "Med"
        abvsum$stats[4] <- "Mean"
        abvsum$stats[5] <- "3rd Q"
        abvsum$stats[6] <- "Max"
        abvsum <- subset(abvsum, select = -c(Freq))
        abvsum$abvsum <- round(
            as.numeric(as.character(abvsum$abvsum)),
            digits = 2
        )
        abvsumstatsstr <- as.numeric(
            unlist(strsplit(toString(abvsum[1:6, 1]), ", "))
        )

        scatterlm <- df %>%
            ggplot(aes(x = IBU, y = Percent_ABV, color = Percent_ABV)) +
            geom_point() +
            ylim(2.5, 13) +
            theme_dark() +
            stat_smooth(method = "lm", color = "red") +
            labs(title = "% ABV vs IBU") +
            theme(plot.title = element_text(hjust = 0.5)) +
            scale_color_gradient(low = "gold2", high = "white")

        scatternolm <- df %>%
            ggplot(aes(x = IBU, y = Percent_ABV, color = Percent_ABV)) +
            geom_point() +
            ylim(2.5, 13) +
            theme_dark() +
            labs(title = "% ABV vs IBU") +
            theme(plot.title = element_text(hjust = 0.5)) +
            scale_color_gradient(low = "gold2", high = "white")

        abvplotdens <- df %>%
            ggplot(aes(x = Percent_ABV)) +
            geom_density(aes(color = "red", size = 1)) +
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
                x = c(
                    abvsumstatsstr[1],
                    abvsumstatsstr[2],
                    abvsumstatsstr[3] - 0.2,
                    abvsumstatsstr[4] + 0.2,
                    abvsumstatsstr[5] + 0.2,
                    abvsumstatsstr[6]
                ),
                y = 0.6,
                label = abvsumstatsstr
            ) +
            annotate(
                "text",
                x = c(
                    abvsumstatsstr[1],
                    abvsumstatsstr[2],
                    abvsumstatsstr[3] - 0.2,
                    abvsumstatsstr[4] + 0.2,
                    abvsumstatsstr[5] + 0.2,
                    abvsumstatsstr[6]
                ),
                y = 0.63,
                label = abvsum$stats
            ) +
            ylim(0, 0.63) +
            xlim(abvsumstatsstr[1], abvsumstatsstr[6])

        abvplotbox <- df %>%
            ggplot(aes(x = Percent_ABV)) +
            geom_boxplot() +
            labs(x = "% ABV", title = "Distribution of ABV Percentages") +
            theme(
                plot.title = element_text(hjust = 0.5),
                legend.position = "none",
                axis.text.y = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks.y = element_blank()
            ) +
            annotate(
                "text",
                x = c(
                    abvsumstatsstr[1],
                    abvsumstatsstr[2],
                    abvsumstatsstr[3] - 0.2,
                    abvsumstatsstr[4] + 0.2,
                    abvsumstatsstr[5] + 0.2,
                    abvsumstatsstr[6]
                ),
                y = 0.6,
                label = abvsumstatsstr
            ) +
            annotate(
                "text",
                x = c(
                    abvsumstatsstr[1],
                    abvsumstatsstr[2],
                    abvsumstatsstr[3] - 0.2,
                    abvsumstatsstr[4] + 0.2,
                    abvsumstatsstr[5] + 0.2,
                    abvsumstatsstr[6]
                ),
                y = 0.63,
                label = abvsum$stats
            ) +
            xlim(abvsumstatsstr[1], abvsumstatsstr[6]) +
            geom_point(aes(x = abvsum$abvsum[4], y = 0), colour = "red")

        # Summary statistics and distribution of IBU with NA values removed
        ibudf <- df[!is.na(df$IBU), , drop = FALSE]

        validate(
            need(nrow(ibudf) > 0, "No IBU values are available for this selection.")
        )

        ibusum <- summary(ibudf$IBU)
        ibusum <- data.frame(table(ibusum))
        ibusum$stats[1] <- "Min"
        ibusum$stats[2] <- "1st Q"
        ibusum$stats[3] <- "Med"
        ibusum$stats[4] <- "Mean"
        ibusum$stats[5] <- "3rd Q"
        ibusum$stats[6] <- "Max"
        ibusum <- subset(ibusum, select = -c(Freq))
        ibusum$ibusum <- round(
            as.numeric(as.character(ibusum$ibusum)),
            digits = 2
        )
        ibusumstatsstr <- as.numeric(
            unlist(strsplit(toString(ibusum[1:6, 1]), ", "))
        )

        ibuplotdens <- ibudf %>%
            ggplot(aes(x = IBU)) +
            geom_density(aes(color = "red", size = 1)) +
            geom_histogram(aes(y = after_stat(density), alpha = 0.2)) +
            labs(x = "IBU", title = "Distribution of IBU", y = "Density") +
            theme(
                plot.title = element_text(hjust = 0.5),
                legend.position = "none"
            ) +
            annotate(
                "text",
                x = c(
                    ibusumstatsstr[1],
                    ibusumstatsstr[2],
                    ibusumstatsstr[3] - 0.2,
                    ibusumstatsstr[4] + 0.2,
                    ibusumstatsstr[5] + 0.2,
                    ibusumstatsstr[6]
                ),
                y = 0.2,
                label = ibusumstatsstr
            ) +
            annotate(
                "text",
                x = c(
                    ibusumstatsstr[1],
                    ibusumstatsstr[2],
                    ibusumstatsstr[3] - 0.2,
                    ibusumstatsstr[4] + 0.2,
                    ibusumstatsstr[5] + 0.2,
                    ibusumstatsstr[6]
                ),
                y = 0.21,
                label = ibusum$stats
            ) +
            ylim(0, 0.23) +
            xlim(ibusumstatsstr[1], ibusumstatsstr[6])

        ibuplotbox <- ibudf %>%
            ggplot(aes(x = IBU)) +
            geom_boxplot() +
            labs(x = "IBU", title = "Distribution of IBU") +
            theme(
                plot.title = element_text(hjust = 0.5),
                legend.position = "none",
                axis.text.y = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks.y = element_blank()
            ) +
            annotate(
                "text",
                x = c(
                    ibusumstatsstr[1],
                    ibusumstatsstr[2],
                    ibusumstatsstr[3] - 0.2,
                    ibusumstatsstr[4] + 0.2,
                    ibusumstatsstr[5] + 0.2,
                    ibusumstatsstr[6]
                ),
                y = 0.6,
                label = ibusumstatsstr
            ) +
            annotate(
                "text",
                x = c(
                    ibusumstatsstr[1],
                    ibusumstatsstr[2],
                    ibusumstatsstr[3] - 0.2,
                    ibusumstatsstr[4] + 0.2,
                    ibusumstatsstr[5] + 0.2,
                    ibusumstatsstr[6]
                ),
                y = 0.63,
                label = ibusum$stats
            ) +
            xlim(ibusumstatsstr[1], ibusumstatsstr[6]) +
            geom_point(aes(x = ibusum$ibusum[4], y = 0), colour = "red")

        if (input$plot == "ABV Histogram") {
            abvplotdens
        } else if (input$plot == "ABV Boxplot") {
            abvplotbox
        } else if (input$plot == "IBU Histogram") {
            ibuplotdens
        } else if (input$plot == "IBU Boxplot") {
            ibuplotbox
        } else if (
            input$plot == "ScatterPlot of % ABV vs IBU" &&
                input$regline == "Yes"
        ) {
            scatterlm
        } else if (
            input$plot == "ScatterPlot of % ABV vs IBU" &&
                input$regline == "No"
        ) {
            scatternolm
        } else {
            brewbystateplot
        }
    })
})
