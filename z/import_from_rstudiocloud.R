#' `import_from_rstudiocloud.R`
#'
#' This project originated on RStudioCloud.
#' This code takes zip files that have been download from RStudioCloud,
#' and reassembles them into the correct file structure.

# Load packages
library(dplyr)
library(readr)
library(purrr)

# Supposing that these files are in my Downloads....
path = "C://Users//tmf77//Downloads"

dest = "C://Users//tmf77//OneDrive - Cornell University//Documents//rstudio//social_infra_many_cities"

# Get files
files = dir(path, pattern = ".zip", full.names = TRUE)

unzip(files[1], junkpaths = FALSE, exdir = dest)


source("z/commit.R")
