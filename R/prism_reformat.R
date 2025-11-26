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
#' forprism <- example_timecourse |> filter(timepoint == 1) |> prismy_col(intensity, compound)
#' xx <- starwars %>% filter(!is.na(species) & !is.na(height))
#' prism_out <- prismy_col(xx, height, species)
#' @import dplyr
#' @import tidyr
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


#' Reformat for Prism by timepoint
#'
#' Reformats your data so each group (e.g. compound) has its own column
#' and each replicate is a subcolumn. 
#' Timepoints are arranged vertically. 
#' 
#' This makes an output suitable for pasting into Prisms grouped table format.  
#' 
#' 
#' @param ddframe Dataframe
#' @param met4prism Column to take values from
#' @param grp Text column to use as column names in output
#' @param timemeasure Column to use as times in output, defaults to timepoint
#' @param wellid Well name to group replicates by, defaults to metadata_well
#' @return Dataframe
#' @examples
#' for_prism <- prismy_time(example_timecourse, intensity, compound)
#' prism_out <- prismy_time(example_timecourse, intensity, compound, timepoint, metadata_well)
#' @import dplyr
#' @import tidyr
#' @export
prismy_time <- function(ddframe, met4prism, grp, timemeasure = timepoint, wellid = metadata_well){
  metricy <- deparse(substitute(met4prism))
  groupy <- deparse(substitute(grp))
  timey <- deparse(substitute(timemeasure))
  
  # make table of old plates and make give each replicate a number
  df_out <- ddframe |>
    dplyr::select({{timemeasure}}, {{grp}}, {{met4prism}}, {{wellid}}) |> 
    dplyr::group_by({{timemeasure}}, {{grp}})  |> 
    dplyr::arrange({{wellid}}) |> 
    dplyr::mutate(repl = row_number()) |> 
    dplyr::ungroup() |> 
    dplyr::arrange({{timemeasure}})
 
   
  # expand to give all conditions the same number of replicates
  df_out <- df_out  |>  
    tidyr::complete({{grp}}, repl, {{timemeasure}})  |>  
    dplyr::arrange({{grp}}, repl, {{timemeasure}})  |>  
    dplyr::select(-{{wellid}})

 
  # transpose column so that treatment and replicate number are column names
  df_out <- df_out |> 
    tidyr::pivot_wider(names_from = c({{grp}}, repl), values_from = {{met4prism}})
  
  # name the metric in case you are putting multiple metrics into one output file
  df_out <- df_out |> 
    dplyr::mutate(metric = metricy) |> 
    dplyr::relocate(metric)  |> 
    dplyr::arrange({{timemeasure}})
  return(df_out)
}


