#' PCA Diagnostics Dashboard
#'
#' Launch an interactive Shiny dashboard for exploring PCA diagnostic results.
#'
#' @param diagnotic_return A list containing diagnostic output from \code{process_pca_data()}
#'   generated with \code{diagnostic = TRUE}. Expected to contain flowsheet data
#'   and PCA results as elements.
#'
#' @return Invisibly returns NULL. Launches an interactive Shiny application for
#'   browsing diagnostic plots by admission CSN.
#'
#' @details
#' The dashboard displays flowsheet timeseries and cumulative values for each
#' admission case. Navigate between cases using the Previous/Next buttons.
#'
#' Requires interactive mode. Will exit with an error if run non-interactively
#' or if input is not a properly formatted list.
#'
#' @examples
#' \dontrun{
#' results <- process_pca_data(data, diagnostic = TRUE)
#' pca_diagnostics(results)
#' }
#'
#' @export
#' @import shiny ggplot2
#' @importFrom rlang .data
pca_diagnostics <- function(diagnotic_return) {

  if (!interactive()) {
    cli::cli_alert_danger("Not running interactive mode, unable to start diganotic dashboard.")
    return(invisible())
  } else if (typeof(diagnotic_return) != "list"){
    cli::cli_alert_danger("You have not supplied a list, unable to start diganotic dashboard.
                          Ensure that you use {.arg diagnostic = TRUE} in the {.fn proccess_pca_data} function and pass the output directly to this function.")
    return(invisible())
  }


  flowsheet_df <- diagnotic_return[[1]]
  results <- diagnotic_return[[2]]

  library(shiny)
  library(ggplot2)
  library(dplyr)
  library(patchwork)

  csn_list <- unique(flowsheet_df$admission_csn)

  ui <- fluidPage(
    titlePanel("PCA Diagnostics"),
    fluidRow(
      column(2, actionButton("dec", "<- Prev")),
      column(2, actionButton("inc", "Next ->")),
      column(2, actionButton("exit", "Exit", class = "btn-danger")),
      column(6, h4(textOutput("csn_label")))
    ),
    plotOutput("plots", height = "800px")
  )

  server <- function(input, output, session) {

    i <- reactiveVal(1)

    observeEvent(input$inc, {
      i(min(i() + 1, length(csn_list)))
    })

    observeEvent(input$dec, {
      i(max(i() - 1, 1))
    })

    observeEvent(input$exit, {
      stopApp()
    })

    output$csn_label <- renderText({
      paste0("CSN ", i(), " of ", length(csn_list), ": ", csn_list[i()])
    })

    output$plots <- renderPlot({

      current_csn <- csn_list[i()]

      plot_1 <- flowsheet_df %>%
        filter(admission_csn == current_csn) %>%
        ggplot(aes(x = recorded_date, y = value, colour = flowsheet_doc_type, group = flowsheet_type)) +
        geom_line() +
        geom_line(aes(y = cumulative_value), color = "black") +
        geom_point(shape = 4, colour = "darkgray") +
        geom_point(data = . %>% filter(reset == TRUE), colour = "orange") +
        geom_point(data = . %>% filter(reset_to_zero == TRUE), colour = "red") +
        geom_point(data = . %>% filter(reset_true == FALSE), colour = "purple", shape = 3) +
        facet_grid(flowsheet_type ~ ., scales = "free")

      plot_2 <- flowsheet_df %>%
        filter(admission_csn == current_csn) %>%
        ggplot(aes(x = recorded_date, y = cumulative_value)) +
        geom_line(aes(colour = flowsheet_type, group = flowsheet_type)) +
        geom_line(data = results %>% filter(admission_csn == current_csn), aes(y = median_value), linetype = 2)

      plot_1 / plot_2
    })
  }

  runApp(shinyApp(ui, server))

  return(invisible())
}
