library(shiny)

ui <- source("ui.R", local = TRUE)$value
base_server <- source("server.R", local = TRUE)$value

server <- function(input, output, session) {
  base_server(input, output, session)

  output$contents <- renderUI({
    source_path <- if (is.null(input$file1)) default_data_url else input$file1$datapath
    df <- head(read_brews(source_path), 100)

    make_cell <- function(value) {
      if (length(value) == 0 || is.na(value)) "" else as.character(value)
    }

    header <- tags$tr(lapply(names(df), tags$th))
    rows <- lapply(seq_len(nrow(df)), function(i) {
      tags$tr(lapply(seq_along(df), function(j) {
        tags$td(make_cell(df[[j]][i]))
      }))
    })

    tags$div(
      tags$p(sprintf("Showing the first %d rows.", nrow(df))),
      tags$div(
        style = "overflow-x:auto; max-height:70vh;",
        tags$table(
          class = "table table-striped table-hover table-condensed",
          tags$thead(header),
          tags$tbody(rows)
        )
      )
    )
  })
}

shinyApp(ui = ui, server = server)
