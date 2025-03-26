#' Coselectr Interactive Column Selector for DataFrames
#'
#' Quickly select columns of interest from large dataframes
#' 
#' Deselect by unchecking columns. Columns can be previewed in the table, 
#' and list at the top can be copied for pasting into console or R script for 
#' use in selecting the chosen columns
#'
#' @param df A data frame to explore
#' @return Selected columns from the input data frame
#' @examples
#' coselectr(mtcars)
#' selected_data <- coselectr(mtcars)
#' @import shiny
#' @import dplyr
#' @export
coselectr <- function(df) {
  # Ensure required packages are available
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("shiny package is required. Please install it with install.packages('shiny')")
  }
  require(tidyverse)
  require(shiny)
  
  # UI Definition
  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
        body { font-family: Arial, sans-serif; }
        .checkbox { margin-bottom: 10px; }
        #column_list { 
          background-color: #f0f0f0; 
          padding: 10px; 
          margin-bottom: 15px; 
          border-radius: 5px;
        }
      "))
    ),
    titlePanel("Column Selector"),
    div(
      id = "column_list",
      h4("Selected Columns:"),
      textOutput("selected_columns_text")
    ),
    sidebarLayout(
      sidebarPanel(
        # Dynamic checkbox group for columns
        uiOutput("column_checkboxes"),
        actionButton("select_all", "Select All"),
        actionButton("deselect_all", "Deselect All")
      ),
      mainPanel(
        # Data preview
        dataTableOutput("selected_data_preview")
      )
    )
  )
  
  # Server Logic
  server <- function(input, output, session) {
    # Generate checkboxes dynamically
    output$column_checkboxes <- renderUI({
      checkboxGroupInput(
        "selected_cols", 
        "Choose Columns:", 
        choices = names(df),
        selected = names(df),  # All selected by default
        inline = FALSE
      )
    })
    
    # Display selected columns as comma-separated list
    output$selected_columns_text <- renderText({
      req(input$selected_cols)
      paste(input$selected_cols, collapse = ", ")
    })
    
    # Select All button
    observeEvent(input$select_all, {
      updateCheckboxGroupInput(
        session, 
        "selected_cols", 
        selected = names(df)
      )
    })
    
    # Deselect All button
    observeEvent(input$deselect_all, {
      updateCheckboxGroupInput(
        session, 
        "selected_cols", 
        selected = character(0)
      )
    })
    
    # Preview selected data
    output$selected_data_preview <- DT::renderDT({
      req(input$selected_cols)
      df %>% select(all_of(input$selected_cols))
    }, options = list(pageLength = 10))
    
    # Store selected columns for return
    return_columns <- reactiveVal(names(df))
    
    # Update return columns when selection changes
    observe({
      return_columns(input$selected_cols)
    })
    
    # When app closes, return the selected columns  FIX THIS
 #   session$onSessionEnded(function() {
  #    stopApp(return_columns())
  #  })
  }
  
  # Run the Shiny app and capture the result
  selected_cols <- runApp(shinyApp(ui, server), display.mode = "normal")
  
  # Print the comma-separated list of selected columns to console
  cat("Selected Columns:", paste(selected_cols, collapse = ", "), "\n")
  
  # Return the selected columns from the original dataframe
 # return(df %>% dplyr::select(all_of(selected_cols)))
}


