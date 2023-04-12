#' @name get_file()
#' @description Short function to get the full file path for a type of file. Wrapper for `get_folder()`.
#' @param name name of city, formatted as path to city's folder. eg. `"search/nyc"`
#' @param type type of file needed from that city's folder. Can include file extension or not.

get_file = function(name, type){

  # test values
  #name = "search/nyc"; type = "searches"

  require(dplyr)
  require(stringr)

  # Get all paths
  # Get entries that match either the [value] or the [value].[extension]
  tab = tibble(file = dir(name)) %>%
    mutate(value = str_remove(file, "[.].*")) %>%
    filter(value == type | file == type)

  if(nrow(tab) > 1){ tab = slice(tab, 1); print("Note: More than 1 entry found. Returning just the first.") }

  # Get the formatted filepath
  result = tab %>% mutate(path = paste0(name, "/", file)) %>% with(path)

  return(result)
}
