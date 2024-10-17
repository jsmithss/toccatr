
#' Tubemap colour list
#' 
#' A List of colours inspired by the London Underground Map
#'
#' This vector contains a list of distinct 23 colours: after 16 it becomes greyscale.
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
#' @export
theme_js <- function() { 
  theme_classic() + 
    theme(
      text = element_text(family = "serif"),
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      axis.ticks = element_line(colour = "grey70", linewidth = 0.2),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey95", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(colour = "grey20", fill = "grey20"),
      strip.text = element_text(colour = "white"),
      #  panel.border = element_rect(colour = "black", fill = NA), #add back for facet
      panel.spacing.x = unit(1,"lines"),
      strip.placement = "outside",
      legend.title = element_text(face="italic"),
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0, "pt"),
      legend.box.margin = margin(0, 0, 0, 0, "pt"),
      legend.spacing = margin(0, 0, 0, 0, "pt"),
      legend.key = element_rect(colour = NA)
    )
}


#' Talisman Theme
#'
#' A minimal ggplot theme.
#' 
#' Talisman colours not yet included.  
#' 
#' Showtext package needs to be installed and Rstudio Global Options...General...Graphics...Backend... 
#' should be set to Cairo PNG (or AGG, basically anything except "windows").  
#' 
#' This package requires font "Fira Sans" to be installed on your system - download from google fonts and restart R if not displaying correctly.  
#'
#' @examples
#' ggplot(data = mtcars, aes(x = wt, y = mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   ggtitle("Talisman theme") +
#'   theme_tt()
#' @export
theme_tt = function(){
  require(showtext)
  font_add_google("Fira Sans", "Fira Sans")
  theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.line = element_line(colour = "grey50"),
      axis.ticks = element_line(colour = "grey70", linewidth = 0.2),
      panel.grid = element_blank(),
      text = element_text(family = "Fira sans"),
      plot.title = element_text(
        hjust = 0.5, size = 16, margin = margin(b = 30)
      ),
      plot.background = element_rect(fill = "#ffffff", colour = "#ffffff"),
      legend.margin = margin(t = 0)
    )
}


#' Talisman Theme for Facets
#'
#' A minimal ggplot theme for faceted charts.
#' 
#' Talisman colours not yet included.
#' Showtext package needs to be installed and Rstudio Global Options...General...Graphics...Backend... 
#' should be set to Cairo PNG (or anything except "windows".
#'
#' @examples
#' ggplot(data = mtcars, aes(x = wt, y = mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   ggtitle("Talisman theme") +
#'   facet_wrap(~cyl) +
#'   theme_ttf()
#' @export
theme_ttf = function(){
  theme_tt() +
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
#' appear in other metrics.  
#' Starting X and Y variables can be preselected
#' 
#' @param data Dataframe to plot
#' @param x_axis_variable Data to plot on x axis (optional, must be in quotes)
#' @param y_axis_variable Data to plot on y axis (optional, must be in quotes)
#' @return Interactive scatterplot
#' @examples
#' majsty(df)
#' majsty(user_df, "no_of_cells", "stain_intensity")
#' @export
majsty <- function(data, x_axis_variable = NULL, y_axis_variable = NULL){
  require(shiny)
  require(ggplot2)
  require(tidyverse)  
  
  column_names <- colnames(data)
  colours <- c("red", "amber", "green", "cyan", "lilac", "none")
  
  red_colourcode <- "#D2222D"
  amber_colourcode <- "#FFBF00"
  green_colourcode <- "#238823"
  cyan_colourcode <- "#08B2E3"
  lilac_colourcode <- "#DC74DC"
  none_colourcode <- "grey70"
  
  draw_main_plot <- function(data, x_axis_variable, y_axis_variable, colourcodes){
    ggplot(data = data[colourcodes == "none",], 
           aes_string(x = x_axis_variable, y = y_axis_variable)) + 
      geom_point(colour = none_colourcode, size = 3, alpha = 0.6) + 
      geom_point(data = data[colourcodes == "red",], colour = red_colourcode, size = 3, alpha = 0.6) + 
      geom_point(data = data[colourcodes == "amber",], colour = amber_colourcode, size = 3, alpha = 0.6) + 
      geom_point(data = data[colourcodes == "green",], colour = green_colourcode, size = 3, alpha = 0.6) +
      geom_point(data = data[colourcodes == "cyan",], colour = cyan_colourcode, size = 3, alpha = 0.6) +
      geom_point(data = data[colourcodes == "lilac",], colour = lilac_colourcode, size = 3, alpha = 0.6) +
      theme_js()
  }
  
  ui <- fluidPage(
    title = "Data Explorer",
    fluidRow(
      column(4, selectInput("colour", "Point Colour", colours)),
      column(4, selectInput("x_axis_variable", "X Axis Variable", column_names, 
                            selected = ifelse(is.null(x_axis_variable), 
                                              column_names[1], x_axis_variable))),
      column(4, selectInput("y_axis_variable", "Y Axis Variable", column_names, 
                            selected = ifelse(is.null(y_axis_variable), 
                                              column_names[2], y_axis_variable))),
      column(12, plotOutput("main_plot", brush = "main_plot_brush")),
      #         column(12,checkboxInput('smooth', 'Smooth')),
      column(12, tableOutput("info"))
    )
  )    
  
  
  
  
  server <- function(input, output, session){
    
    results <- reactiveValues(
      colourcodes = rep("none", nrow(data)))
    
    # when there is a brush event, update the colourcodes accordingly
    # also clears the brush, otherwise it just lingers there
    observeEvent(input$main_plot_brush,{
      results$colourcodes[
        brushedPoints(data, input$main_plot_brush, allRows = TRUE)$selected_] <- 
        input$colour
    })
    
    # this section displays the brushed points as a table however they disappear as soon as the points are selected.    
    output$info <- renderTable({
      brushedPoints(as.data.frame(data), input$main_plot_brush)
    })   
    
    
    # the plot updates every time the variables or the colourcode changes
    main_plot <- reactive(
      draw_main_plot(
        data, input$x_axis_variable, input$y_axis_variable, results$colourcodes))
    output$main_plot <- renderPlot(main_plot())
    
    observeEvent(main_plot(), {
      session$resetBrush("main_plot_brush")  
    })
    
    # handle cancel and done
    observeEvent(input$cancel, {
      stopApp(NULL)
    })
    observeEvent(input$done, {
      stopApp(results$colourcodes)
    })
    
    
  }
  
  shinyApp(ui, server)
  
}






#' Quick summary barcharts
#'
#' Plots mean with SD error bars in GGplot
#' 
#' A quick function to plot summary data barcharts with error bars calculated means.
#' 
#' A time-saver since GGplot doesn't do this by default, but customisation options are limited.  
#' Default colours are based on London underground map. Recommended maximum number of bars is 16, absolute limit is 23.
#' 
#' @param df Dataframe to plot
#' @param varname Variable to plot on barchart
#' @param groupnames Variable which lists groups which will become bar labels
#' @return ggplot object
#' @examples
#' barchartr(df, number_of_cells, compound)
#' bc <- barchartr(df, GFP_pos_cells, treatment)
#' @export
barchartr <- function(dfname, varname, groupnames){
  require(tidyverse)
  xvarname <- deparse(substitute(varname))
  
  data_sum <- dfname %>% group_by({{groupnames}}) %>% summarise(mean = mean({{varname}}, na.rm=TRUE), sd = sd({{varname}}, na.rm=TRUE)) %>% ungroup()
  
  data_sum <- rename(data_sum, {{xvarname}} := mean)
  tubemap <- c("#E32017", "#FFD300", "#00782A", "#F3A9BB", "#868F98", "#9B0056", "#000000", "#003688", "#0098D4", "#95CDBA", "#00A4A7", "#EE7C0E", "#84B817", "#E21836", "#7156A5", "#B36305", "grey33", "grey44", "grey55", "grey66", "grey77", "grey88", "grey92" )
  
  meanbarchart <- ggplot(data_sum) +
    aes(
      x = {{groupnames}},
      y = {{varname}},
      fill = {{groupnames}}
    ) +
    geom_col() +
    geom_errorbar(aes(ymin = {{varname}} - sd, ymax = {{varname}} + sd), width = 0.1) +
    scale_fill_manual(values = tubemap) +
    theme_js() +
    theme(axis.text.x=element_text(angle =- 270, hjust = 1, vjust = 0.2)) +
    theme(legend.position = "none")
  return(meanbarchart)
  
}



#' Ploxy interactive scatter plots
#'
#' Shiny and plotly combine for scatterplot interaction
#' 
#' A shiny application to create scatter plots with selectable x and y axes for EDA
#' X and Y variables can be interactively changed, along with
#' Plotly actions such as zoom, pan and save
#' 
#' @param data Dataframe to plot
#' @return Interactive scatterplot
#' @examples
#' ploxy(df)
#' ploxy(mtcars)
#' @export
ploxy <- function(data) {
  require(shiny)
  require(plotly)
  require(tidyverse)
  
  # get column names for use in axis selection
  column_names <- (colnames(data))
  
  
  ui <- fluidPage(
    # Application title
    titlePanel("Ploxy: plots xy"),
    
    # Page layout
    fluidRow(column(12, plotlyOutput("myPlot"))),
    fluidRow(column(
      6,
      selectInput("xvariable", "Select the X variable", column_names)
    ), column(
      6,
      selectInput("yvariable", "Select the Y variable", column_names)
    )),
    fluidRow(column(
      6,
      selectInput("textvariable", "Select the annotation", column_names)
    ), column(
      6,
      selectInput("colourvariable", "Colour points by", column_names)
    ))
  )
  
  # Make plotly scatter plot
  server <- function(input, output, session) {
    output$myPlot <- renderPlotly({
      req(input$yvariable)
      plot_ly(
        data = data,
        x = ~ get(input$xvariable),
        y = ~ get(input$yvariable),
        color =  ~ get(input$colourvariable),
        text = ~ get(input$textvariable),
        type = "scatter",
        mode = "markers",
        alpha = 0.6,
        hoverinfo = "text"
      ) %>%
        layout(
          xaxis = list(title = " "),
          yaxis = list(title = " "),
          legend = list(title = list(text = " "))
        )
    })
  }
  
  shinyApp(ui, server)
  
}



#' Ploxytime interactive scatter plots with animation
#'
#' Shiny and plotly combine for timecourse interaction
#' 
#' A shiny application to create scatter plots with selectable x and y axes for EDA
#' Data must have a variable called timepoint
#' X and Y variables can be interactively changed, along with
#' Plotly actions such as zoom, pan and save
#' 
#' @param data Dataframe to plot
#' @return Interactive scatterplot
#' @examples
#' ploxy(df)
#' @export
ploxytime <- function(data, timevariable = "timepoint"){
  require(shiny)
  require(plotly)
  require(tidyverse)  
  
  
  column_names <- (colnames(data))
  
  ui <- fluidPage(
    # Application title
    titlePanel("Ploxytime: plots xy time"),
    
    
    fluidRow(
      column(12, plotlyOutput("myPlot"))
    ),
    fluidRow(
      column(6, selectInput("xvariable", "Select the X variable", column_names)),
      column(6, selectInput("yvariable", "Select the Y variable", column_names))
    ),
    fluidRow(
      column(4, selectInput("textvariable", "Select the annotation", column_names)),
      column(4, selectInput("colourvariable", "Colour points by", column_names)),
      column(4, selectInput("timevariable", "Animate by", column_names, selected = "timepoint"))
    )
  )
  
  
  server <- function(input, output, session) {
    output$myPlot <- renderPlotly({
      req(input$yvariable)
      plot_ly(data = data, x = ~get(input$xvariable), y = ~get(input$yvariable), color =~get(input$colourvariable), text = ~get(input$textvariable), type = "scatter", mode = "markers", alpha = 0.6, frame = ~get(input$timevariable), hoverinfo = "text") %>%
        animation_opts(transition = 0) %>%  layout(xaxis = list(title = " "), yaxis = list(title = " "), legend=list(title= list(text = " "))) %>% animation_slider(currentvalue = list(prefix = ""))
    })
  }
  
  shinyApp(ui, server) 
  
  
}
