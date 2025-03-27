#' Replace space with underscore
#' 
#' Self explanatory
#' 
#' Example usage: mtcars$newname <- repunderscore(rownames(mtcars))
#' 
#' @param x Variable you wish to remove spaces from
#' @return A variable with underscores replacing any whitespaces
#' @examples
#' mtcars$new <- repspace(rownames(mtcars))
#' mylist <- c("green apple", "red banana")
#' mylist <- repspace(mylist)
#' @export
repunderscore <- function(x, y = " ") gsub(y, "_", x)


#' Replace underscore with space
#' 
#' Name says it all.
#' 
#' This is the reverse of repunderscore.
#' Example usage: user_df$new <- repspace(user_df$colname)
#' 
#' @param x Variable you wish to remove underscores from
#' @return A variable with spaces instead of underscores
#' @examples
#' mtcars$new <- repspace(rownames(mtcars))
#' mylist <- c("green_apple", "red_banana")
#' mylist <- repspace(mylist)
#' @export
repspace <- function(x, y = "_") gsub(y, " ", x)

#' ZScore a variable
#' 
#' via Hadley Wickham/Twitter
#' 
#' Example usage: user_df$new <- rescale_z(user_df$colname)
#' 
#' @param x Numeric variable you wish to rescale
#' @return A new variable scaled by mean and sd
#' @examples
#' mtcars$z_mpg <- rescale_z(mtcars$mpg)
#' @export
rescale_z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

#' Squish a variable
#' 
#' Specifies lower and upper bounds of a variable to defined values

#' via Hadley Wickham/Twitter
#' Any values below minimum/ above maximum will be replaced with min/max value respectively
#' Example usage: user_df$new <- squish(user_df$colname)
#' 
#' @param x Numeric variable you wish to limit
#' @param min Lower limit for variable
#' @param max Upper limit for variable
#' @return A new variable confined by upper and lower bounds
#' @examples
#' mtcars$limits <- squish(mtcars$hp, 100, 220)
#' @export
squish <- function(x, min, max) {
  require(tidyverse)
  dplyr::case_when(
    x < min ~ min,
    x > max ~ max,
    TRUE ~ x
  )
}

#' Not in
#' 
#' Easier negative filtering
#' 
#' Example usage: aliens <- starwars %>% filter(species %nin% c("Human", "Droid"))
#' 
#' @return Logical list 
#' @examples
#' iris_filtered <- subset(iris, (Species %nin% c("setosa", "versicolor")))
#' @export
'%nin%' <- Negate('%in%')


#' Clip to Excel
#' 
#' Collects a dataframe as clipboard contents.
#' 
#' Do not use for massive dataframes
#' 
#' Example usage: clip2xl(df)
#' @param x Dataframe to copy
#' @param row.names (optional) logical
#' @param col.names (optional) logical
#' @return Pastable table for excel
#' @examples
#' clip2xl(iris)
#' @export
clip2xl <- function(x,row.names=FALSE,col.names=TRUE,...) {
  utils::write.table(x,file = "clipboard-1024",sep="\t",row.names=row.names,col.names=col.names,...)
}



#' Clip to CSV
#' 
#' Collects a dataframe as clipboard contents.
#' 
#' Do not use for massive dataframes
#' 
#' Example usage: clip2csv(df)
#' @param x Dataframe to copy
#' @param row.names (optional) logical
#' @param col.names (optional) logical
#' @return Pastable table in comma delimited text format
#' @examples
#' clip2csv(mtcars)
#' @export
clip2csv <- function(x,row.names=FALSE,col.names=TRUE,...) {
  utils::write.table(x,file = "clipboard-1024",sep=",",row.names=row.names,col.names=col.names,...)
}



#' Clip from Excel
#' 
#' Make a dataframe with clipboard contents, from Excel
#' 
#' Should include header row, tidy column names will be produced
#' 
#' Example usage: df <- clipfromxl()
#' @examples
#' df <- clipfromxl()
#' @export
clipfromxl <- function(){
  require(tidyverse)
  require(janitor)
  cp <- readr::read_tsv(file = clipboard())
  cp <- janitor::clean_names(cp, replace=janitor:::mu_to_u)
  return(cp)
}




#' Jiggle
#' 
#' Relocate (aka reorder) columns in a dataframe so that character columns are first.
#' 
#' 
#' Example usage: df <- jiggle(df)
#' @param dfname Dataframe
#' @return Re-ordered dataframe
#' @examples
#' iris <- jiggle(iris)
#' @import dplyr
#' @export
jiggle <- function(dfname) {
  # Input validation
  if (!is.data.frame(dfname)) {
    stop("Input must be a data frame")
  }
  require(tidyverse)
  dfjigged <- dfname %>% dplyr::relocate(where(is.numeric), .after = where(is.character))
  return(dfjigged)
  #Untested base-r equivalent
  #jiggle <- function(dfname) {
  #num_cols <- sapply(dfname, is.numeric)
  #char_cols <- sapply(dfname, is.character)
  #dfjigged <- dfname[, c(which(char_cols), which(num_cols))]
  #return(dfjigged)
  #}
}


#' Unique time saver
#' 
#' A shortcut for the unique function
#' 
#' When working with a new data set, the unique() function is often used
#' multiple times to get a short list of info within the data such as 
#' treatment groups or plate barcodes. This is a function for the lazy typist
#' to reduce unique(df$my_var) to u(my_var)
#' 
#' Eight keystrokes saved every time!
#' 
#' If your dataframe is not df, there is an option to specify dfname
#' 
#' @param my_var Variable to extract unique values
#' @param dfname Dataframe
#' @return Re-ordered dataframe
#' @examples
#' df <- iris
#' u(Species)
#' u(cyl, mtcars)
#' @export
u <- function(my_var, dfname = df) {
  dfname %>% 
    dplyr::distinct({{my_var}}) %>% 
    dplyr::pull()
}



