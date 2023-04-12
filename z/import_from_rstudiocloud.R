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

files %>% map(~unzip(zipfile = ., junkpaths = FALSE, exdir = dest, overwrite = FALSE))
# Commit those files
source("z/commit.R")


# Load functions
data("functions")

# Let's make a query.R doc for each folder, unless it has one already.
names = dir("search")
for(i in names){ get_query(name = i, open = FALSE, force = FALSE) }
