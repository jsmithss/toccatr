#' Loggy Log transformation for numeric variables
#'
#' One function to log transform all numeric variables in a dataset
#' 
#' All newly created columns names will be appended with log_
#' Only numeric columns are transformed, default log is natural log, but other bases
#' e.g. 2 or 10 can be specified.   
#' The offset is calculated automatically to prevent log of negative numbers but
#' can be added manually.  
#'
#' @param df A data frame
#' @param base Base for logging, default is e ≈ 2.718
#' @param offset To make all columns positive, default is 0
#' @return Data frame with numeric columns log_transformed
#' @examples
#' loggy(mtcars)
#' loggy(mtcars, 10)
#' @export
loggy <- function(df, base = exp(1), offset = 0) {
  # Input validation
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }
  
  # Process each column
  result <- df
  for (col in names(df)) {
    # Check if column is numeric and has no NA values
    if (is.numeric(df[[col]])) {
      # Check if column has non-positive values
      min_val <- min(df[[col]], na.rm = TRUE)
      if (min_val <= 0) {
        # Add offset to make all values positive
        current_offset <- abs(min_val) + offset + 1
        result[[paste0("log_", col)]] <- log(df[[col]] + current_offset, base = base)
      } else {
        result[[paste0("log_", col)]] <- log(df[[col]], base = base)
      }
    }
  }
  
  return(result)
}
