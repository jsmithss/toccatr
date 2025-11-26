#' Anonymize a text/factor variable by replacing unique values with random words
#' 
#' @param data A dataframe containing the variable to anonymize
#' @param var_name The name of the variable to anonymize
#' @param word_list A character vector of words to use for anonymization
#' @param seed An optional seed for reproducible randomization
#' 
#' @return The dataframe with the specified variable anonymized
#' 
#' @examples
#' # Using palmerpenguins dataset
#' library(palmerpenguins)
#' anonymized_penguins <- anony(penguins, species, pokemonz, 123)
#' 
#' @import dplyr rlang
#' @export
anony <- function(data, var_name, word_list = pokemonz, seed = NULL) {
  
  # Set seed for reproducibility if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  var <- rlang::enquo(var_name)
  # Check if variable exists in dataframe
  var_chk <- rlang::as_name(var)
  
  # Check that the variable exists in the dataframe
  if (!(var_chk %in% names(data))) {
    stop(paste0("The variable '", var_chk, "' is not found in the dataframe."))
  }
  
  # Extract the variable (FIXED: was using 'df' instead of 'data')
  col_data <- data |> dplyr::pull(!!var)
  
  # Check if variable is text or factor (FIXED: was using 'var_col' instead of 'col_data')
  if (!is.character(col_data) && !is.factor(col_data)) {
    stop(paste("Variable", var_chk, "must be character or factor type"))
  }
  
  # Get unique values from the variable (excluding NAs) (FIXED: variable name)
  unique_vals <- unique(col_data[!is.na(col_data)])
  
  # Check if we have enough words for anonymization
  if (length(unique_vals) > length(word_list)) {
    warning(paste("Not enough words in word_list. Need", length(unique_vals), 
                  "but only have", length(word_list)))
  }
  
  # Sample random words without replacement for each unique value
  n_words_needed <- min(length(unique_vals), length(word_list))
  random_words <- sample(word_list, size = n_words_needed, replace = FALSE)
  
  # Create mapping from original values to random words
  mapping <- setNames(random_words[1:length(unique_vals)], unique_vals)
  
  # Apply the mapping to create anonymized variable (FIXED: use var_chk for string operations)
  data_anonymized <- data  |>  
    dplyr::mutate(!!var := case_when(
      is.na(.data[[var_chk]]) ~ NA_character_,
      TRUE ~ mapping[as.character(.data[[var_chk]])]
    ))
  
  # Convert back to factor if original was factor (FIXED: variable name)
  if (is.factor(col_data)) {
    data_anonymized[[var_chk]] <- as.factor(data_anonymized[[var_chk]])
  }
  
  # Print mapping for reference (FIXED: use var_chk for printing)
  cat("Anonymization mapping for variable '", var_chk, "':\n", sep = "")
  for (i in seq_along(mapping)) {
    cat("  ", names(mapping)[i], " -> ", mapping[i], "\n", sep = "")
  }
  
  return(data_anonymized)
}


#' Pokemon name list
#' 
#' A List of Single word pokemon
#'
#' This vector contains a list of 148 distinct Pokemon names; 
#' designed for use with the [anony()] function 
#'
#' @format A character vector with 148 elements.
#' @examples
#' pokemonz
#' anonymized_penguins <- anony(penguins, "species", pokemonz)
#' @export
pokemonz <- c("Bulbasaur", "Ivysaur", "Venusaur", "Charmander", "Charmeleon", 
  "Charizard", "Squirtle", "Wartortle", "Blastoise", "Caterpie", 
  "Metapod", "Butterfree", "Weedle", "Kakuna", "Beedrill", "Pidgey", 
  "Pidgeotto", "Pidgeot", "Rattata", "Raticate", "Spearow", "Fearow", 
  "Ekans", "Arbok", "Pikachu", "Raichu", "Sandshrew", "Sandslash", 
  "Nidorina", "Nidoqueen", "Nidorino", "Nidoking", "Clefairy", 
  "Clefable", "Vulpix", "Ninetales", "Jigglypuff", "Wigglytuff", 
  "Zubat", "Golbat", "Oddish", "Gloom", "Vileplume", "Paras", "Parasect", 
  "Venonat", "Venomoth", "Diglett", "Dugtrio", "Meowth", "Persian", 
  "Psyduck", "Golduck", "Mankey", "Primeape", "Growlithe", "Arcanine", 
  "Poliwag", "Poliwhirl", "Poliwrath", "Abra", "Kadabra", "Alakazam", 
  "Machop", "Machoke", "Machamp", "Bellsprout", "Weepinbell", "Victreebel", 
  "Tentacool", "Tentacruel", "Geodude", "Graveler", "Golem", "Ponyta", 
  "Rapidash", "Slowpoke", "Slowbro", "Magnemite", "Magneton", "Farfetch'd", 
  "Doduo", "Dodrio", "Seel", "Dewgong", "Grimer", "Muk", "Shellder", 
  "Cloyster", "Gastly", "Haunter", "Gengar", "Onix", "Drowzee", 
  "Hypno", "Krabby", "Kingler", "Voltorb", "Electrode", "Exeggcute", 
  "Exeggutor", "Cubone", "Marowak", "Hitmonlee", "Hitmonchan", 
  "Lickitung", "Koffing", "Weezing", "Rhyhorn", "Rhydon", "Chansey", 
  "Tangela", "Kangaskhan", "Horsea", "Seadra", "Goldeen", "Seaking", 
  "Staryu", "Starmie", "Scyther", "Jynx", "Electabuzz", "Magmar", 
  "Pinsir", "Tauros", "Magikarp", "Gyarados", "Lapras", "Ditto", 
  "Eevee", "Vaporeon", "Jolteon", "Flareon", "Porygon", "Omanyte", 
  "Omastar", "Kabuto", "Kabutops", "Aerodactyl", "Snorlax", "Articuno", 
  "Zapdos", "Moltres", "Dratini", "Dragonair", "Dragonite", "Mewtwo", 
  "Mew"
)


