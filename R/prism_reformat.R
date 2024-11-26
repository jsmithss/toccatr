#' Reformat for Prism Columns
#'
#' Reformats your data so each group specified has its own column in prism
#' 
#' This is for a single column per group; replicates are listed vertically
#' 
#' @param ddframe Dataframe
#' @param met4prism Column to take values from
#' @param grp Metadata column to use as column names in output
#' @return Dataframe
#' @examples
#' forprism <- prismy_col(df, number_of_cells, compound)
#' xx <- starwars %>% filter(!is.na(species) & !is.na(height))
#' prism_out <- prismy_col(xx, height, species)
#' @export
prismy_col <- function(ddframe, met4prism, grp){
  
  mmetname <- deparse(substitute(met4prism))
  groupy <- deparse(substitute(grp))
  df_out <- ddframe %>% dplyr::group_by({{grp}}) %>% dplyr::mutate(repl = row_number()) %>% dplyr::ungroup()
  
  # select columns, make compound/replicate number and sort
  df_out <- df_out %>% dplyr::select(repl, {{met4prism}}, {{grp}})
  # expand to give all conditions the same number of replicates
  df_out <- df_out %>% 
    tidyr::complete({{grp}}, repl)
  
  # transpose column so that treatment and replicate number are column names
  df_out <- df_out %>% tidyr::pivot_wider(names_from = {{grp}}, values_from = {{met4prism}})
  
  # name the metric in case you are putting multiple metrics into one output file
  df_out <- df_out %>% dplyr::mutate(metric = mmetname, group = groupy) %>% dplyr::relocate(metric, group)
  
  return(df_out)
}

