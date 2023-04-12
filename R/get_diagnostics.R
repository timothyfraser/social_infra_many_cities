#' @name get_diagnostics()
#' @title Diagnostics
#'
#' @description Let's also count how often we are exceeding the 20 site max?
#' Eg. are our maps still useful? We have to interpret this qualitatively, and each researcher might have their own thresholds, but here are some guidelines for interpretation.
#' How often do searches exceed the visible limit of 20 sites returned?
#' Very rarely. I'd say if this number remains under 10%, you're in a good place.
#' If it remains under 5%, that's excellent.
#' Even with 10%, we can still *very easily* identify which cells have more social infrastructure.
#' We just can't compare *past* 20 sites per type per cell.
#' @param name name of city
#' @importFrom dplyr group_by mutate count ungroup summarize `%>%`
#' @importFrom readr read_rds

get_diagnostics = function(name){

  input = paste0(name, "/sites.rds")

  input %>%
    read_rds() %>%
    # For each search we made,
    group_by(id) %>%
    # How many points were returned for that cell-searchterm?
    count() %>%
    mutate(level = case_when(
      n < 20 ~ "OKAY",
      n == 20 ~ "AT LIMIT",
      n > 20 ~ "ABOVE LIMIT")) %>%
    group_by(level) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(percent = round(count / sum(count) * 100, 0) ) %>%
    return()
}

