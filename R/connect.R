#' @name connect()
#' @title `connect()` to database
#'
#' @description Function to connect to a spatial geopackage as an RSQLite database

connect = function(.path = NULL){
  require(RSQLite)
  require(dplyr)
  require(dbplyr)
  require(DBI)
  if(is.null(.path)){ .path = Sys.getenv("DATABASE") }
  con = dbConnect(RSQLite::SQLite(), .path)
  return(con)
}
