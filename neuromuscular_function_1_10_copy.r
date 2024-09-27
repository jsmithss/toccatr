# Neuromuscular  function v1.7
# James Smith, Talisman Therapeutics
# Change notes: 
# v1.10 - edited hotplate to force full y axis A- H
# v1.9 - added Talisman themes
# v1.8 - added clip2csv function
# v1.7 - increased clipboard buffer size for clip2xl, added clipfromxl
# v1.6 - fixed incorrect evaluation numbers in harmread_ob when multiple sets are in same folder


# replace underscore with space
# Example usage: user_df$new <- space(user_df$rowname)
space <- function(x, y = "_") gsub(y, " ", x)

#replace space with underscore
# Example: mtcars$newname <- underscore(rownames(mtcars))
underscore <- function(x, y = " ") gsub(y, "_", x)

# hadley zscores?
# mtcars$zmpg <- rescale_z(mtcars$mpg)
rescale_z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# hadley scaling allows defining floor and ceiling?
# mtcars$limits <- squish(mtcars$hp, 100, 220)
squish <- function(x, min, max) {
  case_when(
    x < min ~ min,
    x > max ~ max,
    TRUE ~ x
  )
}


# custom function for "Not In" 
# aliens <- starwars %>% filter(species %nin% c("Human", "Droid"))
'%nin%' <- Negate('%in%')


#a function which collects a dataframe  as clipboard contents. Do not use for massive dataframes!!
clip2xl <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,file = "clipboard-1024",sep="\t",row.names=row.names,col.names=col.names,...)
}

#a function which collects a dataframe  as clipboard contents. Do not use for massive dataframes!!
clip2csv <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,file = "clipboard-1024",sep=",",row.names=row.names,col.names=col.names,...)
}


#a function to paste from excel
clipfromxl <- function(){
  require(tidyverse)
  require(janitor)
    cp <- read_tsv(file = clipboard())
    cp <- clean_names(cp, replace=janitor:::mu_to_u)
    return(cp)
}


#tubemap colour list
tubemap <- c("#E32017", "#FFD300", "#00782A", "#F3A9BB", "#868F98", "#9B0056", 
  "#000000", "#003688", "#0098D4", "#95CDBA", "#00A4A7", "#EE7C0E", 
  "#84B817", "#E21836", "#7156A5", "#B36305", "grey33", "grey44", 
  "grey55", "grey66", "grey77", "grey88", "grey92")


#### JIGGLE ####
jiggle <- function(dfname) {
  # arguments are dataframe
  # reorders dataframe so that chacter cols come first then numeric
  
  require(tidyverse)
dfjigged <- dfname %>% relocate(where(is.numeric), .after = where(is.character))
return(dfjigged)
}

#### JAMES THEME ####

theme_js <- function() { 
  theme_classic() + 
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
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

theme_ttf = function(){
  theme_tt() +
    theme(
      panel.border = element_rect(colour = "black", fill = NA), #add back for facet
      panel.spacing.x = unit(0.5,"lines"),
      strip.background = element_rect(colour = "grey20", fill = "grey20"),
      strip.text = element_text(colour = "white"),
    )
}


#### HEATMAP ####

hotplate <- function(dfname, metricname) {
# arguments are dataframe, variable_to_plot
# makes a heatmap of the plate
# needs numerical row and column info
# only works for 96 well plate at present

  require(tidyverse)
  dfname <- select(dfname, !matches("rowlet"))
  convrow <- data.frame(rowlet = rep(LETTERS[1:8], 1), row = rep(1:8, 1))
  hotpldf <- merge(convrow,dfname,by= "row")
  hotpldf$rowlf <- fct_rev(hotpldf$rowlet)

  ttitle <- deparse(substitute(metricname))

  require(ggplot2)

  ggplot(hotpldf, aes(column, rowlf, fill={{metricname}})) + 
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colours=c("#440154FF", "#443A83FF", "#31688EFF", "#21908CFF", "#35B779FF", "#8FD744FF", "#FDE725FF")) +
    xlab("Column") +
    ylab("Row") +
    scale_x_continuous(breaks=seq(1,12,1)) +
    scale_y_discrete(limits = c("H", "G", "F", "E", "D", "C", "B", "A")) +
    geom_vline(xintercept = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5), color="white", linewidth=0.25) + 
    geom_hline(yintercept = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5), color="white", linewidth=0.25) +
    theme_classic() +
    ggtitle(ttitle)

}


#### PLATEMAP ####


platemap <- function(dfname, metricname = compound) {
  # arguments are dataframe, metricname
  # makes a plate layout map
  # needs numerical row and column info
  # only works for 96 well plate at present
  
  require(tidyverse)
  dfname <- select(dfname, !matches("rowlet"))
  convrow <- data.frame(rowlet = rep(LETTERS[1:8], 1), row = rep(1:8, 1))
  plmapdf <- left_join(convrow,dfname,by= "row")
  plmapdf$rowlf <- fct_rev(plmapdf$rowlet)
  
  ttitle <- deparse(substitute(metricname))
  
  require(ggplot2)
  #require(RColorBrewer)
  my_pal <- c("#179266","#b16e36","#e0cb17","#79e654","#4f4126","#26eaed","#8bcfb9","#8e6fa2","#990caf","#46a509","#11a8a2","#7c9688","#db91c2","#0bd58d","#b4098f","#3aae20","#d50a6a","#010e3b","#6749c4","#c32470","#7bbcee","#c206db","#1ff229","#2d153f","#dfd699","#5a1ec7","#e2d301","#bb90e0","#9a974d","#a5b0b0", "#324091", "#801155", "#c96e36", "#dffd12", "#df1112", "#4ee0aa")
  ggplot(plmapdf, aes(column, rowlf, fill={{metricname}})) + 
    geom_tile(show.legend = TRUE) +
    scale_fill_manual(values = my_pal) +
    xlab("Column") +
    ylab("Row") +
    scale_x_continuous(breaks=seq(1,12,1)) +
    geom_vline(xintercept = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5), color="white", linewidth=0.25) + 
    geom_hline(yintercept = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5), color="white", linewidth=0.25) +
    theme_classic() +
    theme(axis.line = element_blank()) +
    ggtitle(ttitle)
  
}



#### READ HARMONY PLATERESULTS ####

harmread_pl <- function(pthname) {
  # arguments are pathname
  # imports all PlateResults.txt files from any subdirectory into harmony
  # uses Harmony platename 
  # usage: my_data <- harmread_pl("//10.0.3.251/Projects/Tigers shared/ELB011_Electronic_Lab_Book/ELB011-30 - All data")
  
  require(tidyverse)
  require(janitor)


 hinput <- data.frame()
  bartrack <- ""
  
  i<-1
  
  for (i in 1:length(list.files(path = pthname, full.names = TRUE, all.files = TRUE, recursive = TRUE, pattern="PlateResults.txt")))
    
  {bc <- read_tsv(list.files(path = pthname, full.names = TRUE, all.files = TRUE, recursive = TRUE, pattern="PlateResults.txt")[i], skip = 3, n_max = 1, col_names = FALSE, name_repair = "unique", show_col_types = FALSE) #gets platename from metadata
   ev <- read_tsv(list.files(path = pthname, full.names = TRUE, all.files = TRUE, recursive = TRUE, pattern="PlateResults.txt")[i], skip = 5, n_max = 1, col_names = FALSE, name_repair = "unique", show_col_types = FALSE) #gets evalname  
   temp <- withr::with_options(list(rlib_name_repair_verbosity = "quiet"), 
     read_tsv(list.files(path = pthname, full.names = TRUE, all.files = TRUE, recursive = TRUE, pattern="PlateResults.txt")[i], skip = 8, col_names = TRUE, name_repair = "unique", show_col_types = FALSE) #reads plate results
   )
    temp$platename <- bc <- bc[[1,2]] #pastes platename into plate result
    temp$eval <- ev <- ev[[1,2]] #pastes evaluation name into plate result
    temp <- dplyr::select(temp, !starts_with("..")) # removes empty columns with autongenerated name
    hinput<-bind_rows(hinput,temp) #add data from latest file to hinput
    bartrack <- append(bartrack, bc) #adds platename to tracker sheet
  }
  

  bartrack <- as.data.frame(bartrack)
  dups <- bartrack %>% 
    group_by(bartrack) %>% 
    summarize(n = n()) %>% 
    rename(platename=bartrack) %>% 
    filter(n>1) %>% 
    ungroup()
  
  hinput<- clean_names(hinput, replace = c("µ" = "u")) #use replace=janitor:::mu_to_u to fix micro symbol if using janitor 2.2.0 or later. 
  hinput[] <- lapply(hinput, function(x) (gsub("NaN", NA, x))) #replacing harmony NaN with R NA
  hinput <- type.convert(hinput, as.is = TRUE) #finds integer or string columns and re-applies
  
  rownum2text <- data.frame(row=c(1:8), rowlet=c("A","B","C","D","E","F","G","H"), stringsAsFactors = FALSE)
  colnum2text <- data.frame(column=c(1:12), colstring=c("01","02","03","04","05","06","07","08", "09", "10", "11", "12"), stringsAsFactors = FALSE)
  hinput <- hinput %>% left_join(rownum2text, by = "row") %>% left_join(colnum2text, by = "column")
  
  hinput$metadata_well<-paste(hinput$rowlet, hinput$colstring, sep="")
  hinput$platename_well<-paste(hinput$platename,hinput$metadata_well,sep="_")
  hinput <- hinput %>% mutate(eval = str_replace(eval, "Evaluation", ""))
  #rm(temp, colnum2text, rownum2text)
  print.data.frame(dups)
  return(hinput)
}



#### READ HARMONY OBJECTRESULTS ####

harmread_ob <- function(pthname, flename) {
  # arguments are pathname
  # imports all Object results matching given name files from any subdirectory into harmony
  # uses Harmony platename 
  # usage: my_data <- harmread_ob("//10.0.3.251/Projects/Tigers shared/ELB011_Electronic_Lab_Book/ELB011-30 - All data", "Objects_Population - Whole Image.txt")
  
  require(tidyverse)
  require(janitor)
  
  
  hinput <- data.frame()
  bartrack <- ""
  
  i<-1
  
  for (i in 1:length(list.files(path = pthname, full.names = TRUE, all.files = TRUE, recursive = TRUE, pattern = flename)))
    
  {bc <- read_tsv (list.files(path = pthname, full.names = TRUE, all.files = TRUE, recursive = TRUE, pattern = flename)[i], skip = 3, n_max = 1, col_names = FALSE, name_repair = "unique", show_col_types = FALSE) #gets platename from metadata
  ev <- read_tsv (list.files(path = pthname, full.names = TRUE, all.files = TRUE, recursive = TRUE, pattern=flename)[i], skip = 5, n_max = 1, col_names = FALSE, name_repair = "unique", show_col_types = FALSE) #gets evalname  
  temp<-read_tsv (list.files(path = pthname, full.names = TRUE, all.files = TRUE, recursive = TRUE, pattern = flename)[i], skip = 9, col_names = TRUE, name_repair = "unique", show_col_types = FALSE) #reads plate results
  temp$platename <- bc <- bc[[1,2]] #pastes platename into plate result
  temp$eval <- ev <- ev[[1,2]] #pastes evaluation name into plate result
  temp <- dplyr::select(temp, !starts_with("..")) # removes empty columns with autongenerated name
  hinput<-bind_rows(hinput,temp) #add data from latest file to hinput
  bartrack <- append(bartrack, bc) #adds platename to tracker sheet
  }
  
  
  bartrack <- as.data.frame(bartrack)
  dups <- bartrack %>% 
    group_by(bartrack) %>% 
    summarize(n = n()) %>% 
    rename(platename=bartrack) %>% 
    filter(n>1) %>% 
    ungroup()
  
  hinput<- clean_names(hinput, replace = c("µ" = "u"))
  hinput[] <- lapply(hinput, function(x) (gsub("NaN", NA, x))) #replacing harmony NaN with R NA
  hinput <- type.convert(hinput, as.is = TRUE) #finds integer or string columns and re-applies
  
  rownum2text <- data.frame(row=c(1:8), rowlet=c("A","B","C","D","E","F","G","H"), stringsAsFactors = FALSE)
  colnum2text <- data.frame(column=c(1:12), colstring=c("01","02","03","04","05","06","07","08", "09", "10", "11", "12"), stringsAsFactors = FALSE)
  hinput <- hinput %>% left_join(rownum2text, by = "row") %>% left_join(colnum2text, by = "column")
  
  hinput$metadata_well<-paste(hinput$rowlet, hinput$colstring, sep="")
  hinput$platename_well<-paste(hinput$platename,hinput$metadata_well,sep="_")
  hinput <- hinput %>% mutate(eval = str_replace(eval, "Evaluation", ""))
  #rm(temp, colnum2text, rownum2text)
  print.data.frame(dups)
  return(hinput)
}






#### MAJSTY SCATTER EXPLORER ####

# usage majsty(user_df, "no_of_cells", "stain_intensity")
# column names are optional
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

#### BARCHARTR ####

barchartr <- function(dfname, varname, groupnames){
  # Function to calculate the mean and the standard deviation for each group and plot barchart
  # Usage: bc <- barchartr(df, number_of_cells, compound)
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


#### PLOXY PLOTS XY ####
# usage ploxy(user_df)

ploxy <- function(data){
  require(shiny)
  require(plotly)
  require(tidyverse)  
  

column_names <- (colnames(data))


ui <- fluidPage(
  # Application title
  titlePanel("Ploxy: plots xy"),
  
  
  fluidRow(
    column(12, plotlyOutput("myPlot"))
           ),
  fluidRow(
    column(6, selectInput("xvariable", "Select the X variable", column_names)),
    column(6, selectInput("yvariable", "Select the Y variable", column_names))
          ),
  fluidRow(
    column(6, selectInput("textvariable", "Select the annotation", column_names)),
    column(6, selectInput("colourvariable", "Colour points by", column_names))
          )
               )
  

server <- function(input, output, session) {
  output$myPlot <- renderPlotly({
    req(input$yvariable)
    plot_ly(data = data, x = ~get(input$xvariable), y = ~get(input$yvariable), color =~get(input$colourvariable), text = ~get(input$textvariable), type = "scatter", mode = "markers", alpha = 0.6, hoverinfo = "text") %>%
      layout(xaxis = list(title = " "), yaxis = list(title = " "), legend=list(title= list(text = " "))) 
  })
}

shinyApp(ui, server) 

}


#### PLOXYTIME PLOTS XY WITH ANIMATION ####
# usage ploxytime(user_df)
# must have a column called timepoint


ploxytime <- function(data, timevariable = "timepoint"){
  require(shiny)
  require(plotly)
  require(tidyverse)  
  

column_names <- (colnames(data))

#to conditionally add column where timepoint is one
#using this column breaks because of the code which removes frame title
#df <- df %>% rowwise() %>%  mutate(blanktime = ifelse("blanktime" %in% colnames(df), blanktime, 1))


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
