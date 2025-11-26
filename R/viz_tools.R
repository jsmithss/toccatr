#' Tubemap colour list
#' 
#' A List of colours inspired by the London Underground Map
#'
#' This vector contains a list of distinct 23 colours: after 16 it becomes greyscale.
#' Note that for optimal visual distinction, it's recommended to use no more than 16 colors.
#'
#' @format A character vector with 23 elements.
#' @examples
#' tubemap
#' pie(rep(1,23), col=tubemap, labels=tubemap)
#' @export
tubemap <- c("#E32017", "#FFD300", "#00782A", "#F3A9BB", "#868F98", "#9B0056", 
             "#000000", "#003688", "#0098D4", "#95CDBA", "#00A4A7", "#EE7C0E", 
             "#84B817", "#E21836", "#7156A5", "#B36305", "grey33", "grey44", 
             "grey55", "grey66", "grey77", "grey88", "grey92")

#' Color-blind friendly palette
#' 
#' A list of color-blind friendly distinct colors
#'
#' @format A character vector with 8 elements.
#' @examples
#' cbfriendly
#' pie(rep(1,8), col=cbfriendly, labels=cbfriendly)
#' @export
cbfriendly <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7", "#999999")

#' James Theme
#' 
#' A minimal ggplot theme
#'
#' This is superseded by theme_tt but kept for continuity with old scripts
#'
#' @examples
#' ggplot(data = mtcars, aes(x = wt, y = mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   ggtitle("JS theme") +
#'   theme_js()
#' @importFrom ggplot2 theme_minimal theme element_line element_text element_rect margin
#' @export
theme_js <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.line = ggplot2::element_line(colour = "grey50"),
      axis.ticks = ggplot2::element_line(colour = "grey70", linewidth = 0.2),
      panel.grid = ggplot2::element_blank(),
      text = ggplot2::element_text(family = "serif"),
      plot.title = ggplot2::element_text(
        hjust = 0.5, size = 16, margin = ggplot2::margin(b = 30)
      ),
      plot.background = ggplot2::element_rect(fill = "#ffffff", colour = "#ffffff"),
      legend.margin = ggplot2::margin(t = 0)
    )
}

#' Talisman Theme
#'
#' A minimal ggplot theme.
#' 
#' A minimal theme for ggplot with legend at the bottom and no gridlines.  
#' 
#' @examples
#' ggplot(data = mtcars, aes(x = wt, y = mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   ggtitle("Talisman theme") +
#'   theme_tt()
#' @importFrom ggplot2 theme_minimal theme element_line element_text element_rect margin
#' @export
theme_tt <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.line = ggplot2::element_line(colour = "grey50"),
      axis.ticks = ggplot2::element_line(colour = "grey70", linewidth = 0.2),
      panel.grid = ggplot2::element_blank(),
      text = ggplot2::element_text(family = "Calibri"),
      plot.title = ggplot2::element_text(
        hjust = 0.5, size = 16, margin = ggplot2::margin(b = 30)
      ),
      plot.background = ggplot2::element_rect(fill = "#ffffff", colour = "#ffffff"),
      legend.margin = ggplot2::margin(t = 0)
    )
}

#' Talisman Theme 2
#'
#' A minimal ggplot theme with Fira Sans font which must be installed on your system, via google fonts.
#' 
#' This requires the following in the header of your Qmd file: 
#' knitr:
#'   opts_chunk:
#'     dev: ragg_png
#' 
#' A minimal theme for ggplot with legend at the bottom and no gridlines.  
#' 
#'
#' @examples
#' ggplot(data = mtcars, aes(x = wt, y = mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   ggtitle("Talisman theme") +
#'   theme_tt2()
#' @importFrom ggplot2 theme_minimal theme element_line element_text element_rect margin
#' @import ragg
#' @import systemfonts
#' @export
theme_tt2 = function(){
  theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.line = element_line(colour = "grey50"),
      axis.ticks = element_line(colour = "grey70", linewidth = 0.2),
      panel.grid = element_blank(),
      text = element_text(family = "Fira sans"),
      plot.title = element_text(
        hjust = 0.5, size = 16, margin = margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      plot.background = element_rect(fill = "#ffffff", colour = "#ffffff"),
      legend.margin = margin(t = 0)
    )
}




#' Talisman Theme for Facets
#'
#' A minimal ggplot theme for faceted charts.
#' 
#'        Rstudio Global Options...General...Graphics...Backend... 
#'        should be set to Cairo PNG (or anything except "windows").
#' @examples
#' ggplot(data = mtcars, aes(x = wt, y = mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   ggtitle("Talisman theme") +
#'   facet_wrap(~cyl) +
#'   theme_ttf()
#' @importFrom ggplot2 theme element_rect element_text unit
#' @export
theme_ttf <- function() { 
  theme_tt() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "black", fill = NA), #add back for facet
      panel.spacing.x = ggplot2::unit(0.5, "lines"),
      strip.background = ggplot2::element_rect(colour = "grey20", fill = "grey20"),
      strip.text = ggplot2::element_text(colour = "white")
    )
}


#' Talisman Theme for Facets 2
#'
#' A minimal ggplot theme for faceted charts, using Fira Sans.
#' 
#' @examples
#' ggplot(data = mtcars, aes(x = wt, y = mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   ggtitle("Talisman theme") +
#'   facet_wrap(~cyl) +
#'   theme_tt2f()
#' @importFrom ggplot2 theme element_rect element_text unit
#' @export
theme_tt2f = function(){
  theme_tt2() +
    theme(
      panel.border = element_rect(colour = "black", fill = NA), #add back for facet
      panel.spacing.x = unit(0.5,"lines"),
      strip.background = element_rect(colour = "grey20", fill = "grey20"),
      strip.text = element_text(colour = "white"),
    )
}



#' Majsty interactive scatter plots
#'
#' Select and colour points in a scatterplot
#' 
#' A shiny application to create scatter plots with selectable x and y axes for EDA
#' 
#' Points can be highlighted in different colours, and importantly these colours persist
#' when the axes are changed, for example to visualise where outliers in one metric
#' appear in other metrics. Starting X and Y variables can be preselected
#' 
#' @param data Dataframe to plot
#' @param x_axis_variable Data to plot on x axis (optional, must be in quotes)
#' @param y_axis_variable Data to plot on y axis (optional, must be in quotes)
#' @param color_scheme Vector of color codes to use for point colors
#' @return Interactive scatterplot application
#' @examples
#' majsty(df)
#' majsty(user_df, "no_of_cells", "stain_intensity")
#' majsty(df, color_scheme = cbfriendly) # Use color blind friendly palette
#' @importFrom shiny fluidPage fluidRow column selectInput plotOutput checkboxInput
#'   tableOutput brushedPoints reactive reactiveValues observeEvent renderTable
#'   renderPlot stopApp shinyApp
#' @importFrom ggplot2 ggplot aes_string geom_point
#' @export
majsty <- function(data, x_axis_variable = NULL, y_axis_variable = NULL, 
                   color_scheme = NULL) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame")
  }
  if (nrow(data) == 0) {
    stop("Input data frame is empty")
  }
  
  column_names <- colnames(data)
  
  if (!is.null(x_axis_variable) && !(x_axis_variable %in% column_names)) {
    stop("x_axis_variable '", x_axis_variable, "' not found in data")
  }
  if (!is.null(y_axis_variable) && !(y_axis_variable %in% column_names)) {
    stop("y_axis_variable '", y_axis_variable, "' not found in data")
  }
  
  # Color configuration
  colors <- c("red", "amber", "green", "cyan", "lilac", "none")
  
  if (is.null(color_scheme)) {
    color_codes <- c(
      red = "#D2222D",
      amber = "#FFBF00",
      green = "#238823",
      cyan = "#08B2E3",
      lilac = "#DC74DC",
      none = "grey70"
    )
  } else {
    if (length(color_scheme) < 6) {
      warning("color_scheme should have at least 6 colors, using default colors")
      color_codes <- c(
        red = "#D2222D",
        amber = "#FFBF00",
        green = "#238823",
        cyan = "#08B2E3",
        lilac = "#DC74DC",
        none = "grey70"
      )
    } else {
      color_codes <- c(
        red = color_scheme[1],
        amber = color_scheme[2],
        green = color_scheme[3],
        cyan = color_scheme[4],
        lilac = color_scheme[5],
        none = color_scheme[6]
      )
    }
  }
  
  draw_main_plot <- function(data, x_axis_variable, y_axis_variable, colourcodes) {
    # Filter data for each color category
    none_data <- data[colourcodes == "none", ]
    red_data <- data[colourcodes == "red", ]
    amber_data <- data[colourcodes == "amber", ]
    green_data <- data[colourcodes == "green", ]
    cyan_data <- data[colourcodes == "cyan", ]
    lilac_data <- data[colourcodes == "lilac", ]
    
    # Create plot layer by layer
    p <- ggplot2::ggplot(data = none_data, 
                         ggplot2::aes_string(x = x_axis_variable, y = y_axis_variable)) + 
      ggplot2::geom_point(colour = color_codes["none"], size = 3, alpha = 0.6)
    
    # Add points for each color only if they exist
    if (nrow(red_data) > 0) {
      p <- p + ggplot2::geom_point(data = red_data, colour = color_codes["red"], 
                                   size = 3, alpha = 0.6)
    }
    if (nrow(amber_data) > 0) {
      p <- p + ggplot2::geom_point(data = amber_data, colour = color_codes["amber"], 
                                   size = 3, alpha = 0.6)
    }
    if (nrow(green_data) > 0) {
      p <- p + ggplot2::geom_point(data = green_data, colour = color_codes["green"], 
                                   size = 3, alpha = 0.6)
    }
    if (nrow(cyan_data) > 0) {
      p <- p + ggplot2::geom_point(data = cyan_data, colour = color_codes["cyan"], 
                                   size = 3, alpha = 0.6)
    }
    if (nrow(lilac_data) > 0) {
      p <- p + ggplot2::geom_point(data = lilac_data, colour = color_codes["lilac"], 
                                   size = 3, alpha = 0.6)
    }
    
    p + theme_js()
  }
  
  ui <- shiny::fluidPage(
    title = "Data Explorer",
    shiny::fluidRow(
      shiny::column(4, shiny::selectInput("colour", "Point Colour", colors)),
      shiny::column(4, shiny::selectInput("x_axis_variable", "X Axis Variable", column_names, 
                                          selected = ifelse(is.null(x_axis_variable), 
                                                            column_names[1], x_axis_variable))),
      shiny::column(4, shiny::selectInput("y_axis_variable", "Y Axis Variable", column_names, 
                                          selected = ifelse(is.null(y_axis_variable), 
                                                            column_names[2], y_axis_variable)))
    ),
    shiny::fluidRow(
      shiny::column(12, shiny::plotOutput("main_plot", brush = "main_plot_brush"))
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::checkboxInput('show_table', 'Show Selected Points', value = FALSE)),
      shiny::column(6, shiny::downloadButton('download_data', 'Download Selected Points'))
    ),
    shiny::fluidRow(
      shiny::column(12, shiny::tableOutput("info"))
    )
  )    
  
  server <- function(input, output, session) {
    # Store color codes for each point
    results <- shiny::reactiveValues(
      colourcodes = rep("none", nrow(data))
    )
    
    # Store currently selected points
    selected_points <- shiny::reactiveVal(data.frame())
    
    # When there is a brush event, update the colourcodes accordingly
    shiny::observeEvent(input$main_plot_brush, {
      brushed <- shiny::brushedPoints(data, input$main_plot_brush, allRows = TRUE)
      results$colourcodes[brushed$selected_] <- input$colour
      
      # Update selected points
      selected_points(shiny::brushedPoints(data, input$main_plot_brush))
    })
    
    # Display brushed points as a table
    output$info <- shiny::renderTable({
      if (input$show_table) {
        selected_points()
      }
    })
    
    # Download handler for selected points
    output$download_data <- shiny::downloadHandler(
      filename = function() {
        paste("selected-points-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(selected_points(), file, row.names = FALSE)
      }
    )
    
    # The plot updates every time the variables or the colourcode changes
    main_plot <- shiny::reactive({
      draw_main_plot(
        data, input$x_axis_variable, input$y_axis_variable, results$colourcodes)
    })
    
    output$main_plot <- shiny::renderPlot(main_plot())
    
    # Reset brush after plot changes
    shiny::observeEvent(main_plot(), {
      session$resetBrush("main_plot_brush")
    })
    
    # Handle cancel and done
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })
    
    shiny::observeEvent(input$done, {
      shiny::stopApp(results$colourcodes)
    })
  }
  
  shiny::shinyApp(ui, server)
}

#' Quick summary barcharts
#'
#' Plots mean with SD error bars in ggplot
#' 
#' A quick function to plot summary data barcharts with error bars using calculated means.
#' 
#' A time-saver since ggplot doesn't do this by default, but customisation options are limited.  
#' Default colours are based on London underground map. Recommended maximum number of bars is 16.
#' 
#' @param dfname Dataframe to plot
#' @param varname Variable to plot on barchart
#' @param groupnames Variable which lists groups which will become bar labels
#' @param color_palette Color palette to use (defaults to tubemap)
#' @param error_bar Logical. If TRUE, show error bars (default)
#' @return ggplot object
#' @examples
#' barchartr(df, number_of_cells, compound)
#' bc <- barchartr(df, GFP_pos_cells, treatment)
#' bc <- barchartr(df, GFP_pos_cells, treatment, color_palette = cbfriendly)
#' @importFrom dplyr group_by summarise ungroup rename
#' @importFrom ggplot2 ggplot aes geom_col geom_errorbar scale_fill_manual theme
#' @export
barchartr <- function(dfname, varname, groupnames, 
                      color_palette = tubemap, error_bar = TRUE) {
  # Input validation
  if (!is.data.frame(dfname)) {
    stop("First argument must be a data frame")
  }
  
  varname_expr <- rlang::enquo(varname)
  groupnames_expr <- rlang::enquo(groupnames)
  xvarname <- rlang::as_name(varname_expr)
  
  # Check if columns exist
  if (!rlang::as_name(groupnames_expr) %in% colnames(dfname)) {
    stop(paste("Column", rlang::as_name(groupnames_expr), "not found in data frame"))
  }
  if (!xvarname %in% colnames(dfname)) {
    stop(paste("Column", xvarname, "not found in data frame"))
  }
  
  # Calculate summary statistics
  data_sum <- dfname %>% 
    dplyr::group_by({{ groupnames }}) %>% 
    dplyr::summarise(
      mean = mean({{ varname }}, na.rm = TRUE), 
      sd = sd({{ varname }}, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    dplyr::ungroup()
  
  data_sum <- dplyr::rename(data_sum, {{ xvarname }} := mean)
  
  # Check if there are too many groups
  group_count <- length(unique(dplyr::pull(data_sum, {{ groupnames }})))
  if (group_count > 16 && group_count <= length(color_palette)) {
    warning("More than 16 groups detected. Colors after 16 may be less visually distinct.")
  } else if (group_count > length(color_palette)) {
    warning("More groups than available colors. Colors will be recycled.")
  }
  
  # Create plot
  meanbarchart <- ggplot2::ggplot(data_sum) +
    ggplot2::aes(
      x = {{ groupnames }},
      y = {{ varname }},
      fill = {{ groupnames }}
    ) +
    ggplot2::geom_col()
  
  # Add error bars if requested
  if (error_bar) {
    meanbarchart <- meanbarchart +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = {{ varname }} - sd, ymax = {{ varname }} + sd), 
        width = 0.1
      )
  }
  
  # Complete the plot with styling
  meanbarchart <- meanbarchart +
    ggplot2::scale_fill_manual(values = color_palette) +
    theme_js() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = -270, hjust = 1, vjust = 0.2),
      legend.position = "none"
    )
  
  return(meanbarchart)
}

#' Ploxy interactive scatter plots
#'
#' Shiny and plotly combine for scatterplot interaction
#' 
#' A shiny application to create scatter plots with selectable x and y axes for EDA.
#' X and Y variables can be interactively changed, along with
#' Plotly actions such as zoom, pan and save.
#' 
#' @param data Dataframe to plot
#' @return Interactive scatterplot
#' @examples
#' ploxy(df)
#' ploxy(mtcars)
#' @importFrom shiny fluidPage titlePanel fluidRow column selectInput req downloadButton downloadHandler
#' @importFrom plotly plot_ly layout hide_colorbar plotlyOutput renderPlotly 
#' @export
ploxy <- function(data) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame")
  }
  if (nrow(data) == 0) {
    stop("Input data frame is empty")
  }
  
  # Get column names for use in axis selection
  column_names <- colnames(data)
  
  ui <- shiny::fluidPage(
    # Application title
    shiny::titlePanel("Ploxy: plots xy"),
    
    # Page layout
    shiny::fluidRow(shiny::column(12, plotly::plotlyOutput("myPlot"))),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::selectInput("xvariable", "Select the X variable", column_names)
      ), 
      shiny::column(
        6,
        shiny::selectInput("yvariable", "Select the Y variable", column_names)
      )
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::selectInput("textvariable", "Select the annotation", column_names)
      ), 
      shiny::column(
        6,
        shiny::selectInput("colourvariable", "Colour points by", column_names)
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::downloadButton("download_data", "Download Displayed Data")
      )
    )
  )
  
  server <- function(input, output, session) {
    output$myPlot <- plotly::renderPlotly({
      shiny::req(input$yvariable)
      plotly::plot_ly(
        data = data,
        x = ~ get(input$xvariable),
        y = ~ get(input$yvariable),
        color = ~ get(input$colourvariable),
        text = ~ get(input$textvariable),
        type = "scatter",
        mode = "markers",
        alpha = 0.6,
        hoverinfo = "text"
      ) %>%
        plotly::hide_colorbar() %>%
        plotly::layout(
          xaxis = list(title = input$xvariable),
          yaxis = list(title = input$yvariable),
          legend = list(title = list(text = "Legend"))
        )
    })
    
    # Download handler for the data
    output$download_data <- shiny::downloadHandler(
      filename = function() {
        paste("ploxy-data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data, file, row.names = FALSE)
      }
    )
  }
  
  shiny::shinyApp(ui, server)
}

#' Ploxytime interactive scatter plots with animation
#'
#' Shiny and plotly combine for timecourse interaction
#' 
#' A shiny application to create scatter plots with selectable x and y axes for EDA.
#' X and Y variables can be interactively changed, along with
#' Plotly actions such as zoom, pan and save.
#' 
#' @param data Dataframe to plot
#' @param timevariable Name of the column to use for animation frames (default: "timepoint")
#' @return Interactive scatterplot with animation
#' @examples
#' ploxytime(df)
#' ploxytime(df, "date")
#' @importFrom shiny fluidPage titlePanel fluidRow column selectInput req downloadButton downloadHandler
#' @importFrom plotly plot_ly layout hide_colorbar plotlyOutput renderPlotly animation_opts animation_slider
#' @export
ploxytime <- function(data, timevariable = "timepoint") {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame")
  }
  if (nrow(data) == 0) {
    stop("Input data frame is empty")
  }
  if (!(timevariable %in% colnames(data))) {
    stop(paste("Time variable '", timevariable, "' not found in data frame"))
  }
  
  column_names <- colnames(data)
  
  ui <- shiny::fluidPage(
    # Application title
    shiny::titlePanel("Ploxytime: plots xy time"),
    
    # Plot output
    shiny::fluidRow(
      shiny::column(12, plotly::plotlyOutput("myPlot"))
    ),
    # Variable selection
    shiny::fluidRow(
      shiny::column(6, shiny::selectInput("xvariable", "Select the X variable", column_names)),
      shiny::column(6, shiny::selectInput("yvariable", "Select the Y variable", column_names))
    ),
    shiny::fluidRow(
      shiny::column(4, shiny::selectInput("textvariable", "Select the annotation", column_names)),
      shiny::column(4, shiny::selectInput("colourvariable", "Colour points by", column_names)),
      shiny::column(4, shiny::selectInput("timevariable", "Animate by", column_names, selected = timevariable))
    ),
    shiny::fluidRow(
      shiny::column(12, shiny::downloadButton("download_data", "Download Data"))
    )
  )
  
  server <- function(input, output, session) {
    output$myPlot <- plotly::renderPlotly({
      shiny::req(input$yvariable)
      plotly::plot_ly(
        data = data,
        x = ~ get(input$xvariable), 
        y = ~ get(input$yvariable), 
        color = ~ get(input$colourvariable), 
        text = ~ get(input$textvariable), 
        type = "scatter", 
        mode = "markers", 
        alpha = 0.6, 
        frame = ~ get(input$timevariable), 
        hoverinfo = "text"
      ) %>%
        plotly::animation_opts(transition = 0) %>%  
        plotly::hide_colorbar() %>%
        plotly::layout(
          xaxis = list(title = input$xvariable), 
          yaxis = list(title = input$yvariable), 
          legend = list(title = list(text = "Legend"))
        ) %>% 
        plotly::animation_slider(currentvalue = list(prefix = ""))
    })
    
    # Download handler for the data
    output$download_data <- shiny::downloadHandler(
      filename = function() {
        paste("ploxytime-data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data, file, row.names = FALSE)
      }
    )
  }
  
  shiny::shinyApp(ui, server)
}
