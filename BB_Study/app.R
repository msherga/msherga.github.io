library(shiny)

# Reuse the existing UI and server, then replace only the dataset table.
ui <- source("ui.R", local = TRUE)$value
base_server <- source("server.R", local = TRUE)$value

# The original beer data contains a few legacy Windows-1252 characters.
# Plots ignore most text fields, but the HTML table must render every string.
normalize_text <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  normalized <- iconv(x, from = "UTF-8", to = "UTF-8", sub = NA)
  invalid <- is.na(normalized) & !is.na(x)

  if (any(invalid)) {
    normalized[invalid] <- iconv(
      x[invalid],
      from = "windows-1252",
      to = "UTF-8",
      sub = ""
    )
  }

  normalized[is.na(x)] <- NA_character_
  normalized
}

server <- function(input, output, session) {
  base_server(input, output, session)

  output$contents <- renderTable({
    source <- if (is.null(input$file1)) {
      default_data_url
    } else {
      input$file1$datapath
    }

    df <- read_brews(source)
    df[] <- lapply(df, normalize_text)

    head(df, 100)
  })
}

shinyApp(ui = ui, server = server)
