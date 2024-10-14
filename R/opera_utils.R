

#' Harmony Reader
#'
#' Imports all Harmony plate results from directory and subdirectories.
#' 
#' Any file named using Harmony default convention of PlateResults.txt is included
#' Platename and Evaluation number is added to each row,
#' this info comes from the metadata in the head of the txt file.
#' Multiple plates are appended to the bottom of the resulting dataframe
#' 
#' @param pthname Path or directory to read
#' @return Dataframe
#' @examples
#' df <- harmread_pl("/my_subdirectory")
#' pth <- getwd()
#' df <-harmread_pl(pth)
#' @export
harmread_pl <- function(pthname) {
  require(tidyverse)
  require(janitor)
  
  hinput <- data.frame()
  bartrack <- ""
  
  i<-1
  
  for (i in 1:length(list.files(path = pthname, full.names = TRUE, all.files = TRUE, recursive = TRUE, pattern="PlateResults.txt")))
    
  {bc <- read_tsv (list.files(path = pthname, full.names = TRUE, all.files = TRUE, recursive = TRUE, pattern="PlateResults.txt")[i], skip = 3, n_max = 1, col_names = FALSE, name_repair = "unique", show_col_types = FALSE) #gets platename from metadata
  ev <- read_tsv (list.files(path = pthname, full.names = TRUE, all.files = TRUE, recursive = TRUE, pattern="PlateResults.txt")[i], skip = 5, n_max = 1, col_names = FALSE, name_repair = "unique", show_col_types = FALSE) #gets evalname  
  temp<-read_tsv (list.files(path = pthname, full.names = TRUE, all.files = TRUE, recursive = TRUE, pattern="PlateResults.txt")[i], skip = 8, col_names = TRUE, name_repair = "unique", show_col_types = FALSE) #reads plate results
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




#' Harmony Object Reader
#'
#' Imports all Harmony object results from directory and subdirectories.
#' 
#' Any file which matches the filename specified is imported to one dataframe.
#' Platename and Evaluation number is added to each row,
#' this info comes from the metadata in the head of the txt file.
#' Multiple plates are appended to the bottom of the resulting dataframe
#' 
#' @param pthname Path or directory to read
#' @param flename Filename pattern
#' @return Dataframe
#' @examples
#' df <- harmread_ob("/my_subdirectory", "Objects_Population - fov.txt")
#' pth <- getwd()
#' df <-harmread_pl(pth, "Objects_Population - nuclei.txt")
#' @export
harmread_ob <- function(pthname, flename) {
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
  
  hinput<- clean_names(hinput, replace = c("µ" = "u")) #replace=janitor:::mu_to_u
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







#' Well2row
#' 
#' Makes row and column numbers
#' Requires well name in A01 format
#' 
#' @param dfname Dataframe
#' @param wellvar Variable to convert
#' @return Dataframe
#' @examples
#' new_df <- well2row(df, well)
#' @export
well2row <- function(dfname, wellvar) {
  require(tidyverse)
  dfw <- dfname %>% 
    mutate(row = str_sub({{wellvar}}, 1,1)) %>% 
    mutate(row = str_replace_all(row, c("A" = "1","B" = "2","C" = "3","D" = "4","E" = "5","F" = "6","G" = "7","H" = "8"))) %>% 
    mutate(col = str_sub({{wellvar}}, 2,3)) %>% 
    mutate(column = as.integer(col)) %>% 
    select(-col)  
  return(dfw)
}



#' Platemap
#'
#' Makes a colour map of samples on the plate.
#' 
#' Requires row and column in numerical format
#' Only works for 96 well at present. Dataframes with duplicate or multiple 
#' values for each well may cause problems, so aim for 96 or less observations.
#' 
#' @param dfname Dataframe
#' @param metricname Sample type, default is compound
#' @return ggplot object
#' @examples
#' platemap(df, compound)
#' @export
platemap <- function(dfname, metricname = compound) {
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
    scale_y_discrete(limits = c("H", "G", "F", "E", "D", "C", "B", "A")) +
    geom_vline(xintercept = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5), color="white", linewidth=0.25) + 
    geom_hline(yintercept = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5), color="white", linewidth=0.25) +
    theme_classic() +
    theme(axis.line = element_blank()) +
    ggtitle(ttitle)
  
}



#' Hotplate
#'
#' Makes a heatmap of the plate
#' 
#' Needs numerical row and column info
#' only works for 96 well plate at present 
#' if numerical row col not available, run well2row first
#' 
#' @param dfname Dataframe
#' @param metricname Variable to heatmap
#' @return ggplot object
#' @examples
#' hotplate(df, nuclei_number_of_objects)
#' @export
hotplate <- function(dfname, metricname) {
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


