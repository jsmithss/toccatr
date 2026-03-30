#' Makes multiple evaluations sit side-by-side
#'
#' When there are multiple evaluations in a long format, 
#' this function will reformat so that results of different evaluations
#' can be compared by putting results in wide format,
#' 
#' This only works for one plate at once, so filter by platename first
#' 
#' @param ddframe Dataframe
#' @param evalname Column to look for evaluation numbers, default is eval
#' @param id_cols Columns to use for matching data, defaults to metadata_well
#' @return Dataframe
#' @examples
#' ev_compare <- evals_to_wide(df, eval, c(metadata_well, timepoint))
#' simplest_use <- evals_to_wide(df)
#' @import dplyr
#' @import tidyr
#' @import janitor
#' @export


evals_to_wide <- function(ddframe, evalname = eval, id_cols = metadata_well) {

    # Check platename uniqueness if present
  if ("platename" %in% colnames(ddframe)) {
    n_plates <- dplyr::n_distinct(ddframe[["platename"]])
    if (n_plates > 1) {
      stop(
        "Column 'platename' contains ", n_plates, " distinct values: ",
        paste(unique(ddframe[["platename"]]), collapse = ", "),
        "\nThis function expects data from a single plate only."
      )
    }
  }
  
  eval_col    <- deparse(substitute(evalname))
  id_col_syms <- dplyr::select(ddframe, {{id_cols}}) |> colnames()
  value_cols  <- ddframe |>
    dplyr::select(-{{evalname}}, -{{id_cols}}) |>
    colnames()
  
  joined_df <- ddframe |>
    tidyr::pivot_wider(
      id_cols     = all_of(id_col_syms),
      names_from  = all_of(eval_col),
      values_from = all_of(value_cols),
      names_sep   = "_"
    )
  
  # Identify the first eval level to use as the "primary" suffix to strip
  first_eval  <- as.character(sort(unique(ddframe[[eval_col]]))[1])
  suffix_pat  <- paste0("_", first_eval, "$")
  
  # Remove excess suffix for first evaluation
  colnames(joined_df) <- stringr::str_remove(colnames(joined_df), suffix_pat)
  
  # Remove duplicated and empty columns 
  joined_df <- joined_df[!duplicated(as.list(joined_df))]
  joined_df <- janitor::remove_empty(joined_df, "cols")
  
  return(joined_df)
}
