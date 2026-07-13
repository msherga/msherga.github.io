library(shiny)

# Reuse the existing UI and server, then replace only the large dataset table.
ui <- source("ui.R", local = TRUE)$value
base_server <- source("server.R", local = TRUE)$value

server <- function(input, output, session) {
  base_server(input, output, session)

  output$contents <- renderTable({
    source <- if (is.null(input$file1)) {
      default_data_url
    } else {
      input$file1$datapath
    }

    head(read_brews(source), 100)
  })
}

shinyApp(ui = ui, server = server)
