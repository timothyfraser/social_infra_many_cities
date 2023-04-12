# Get all installed packages
p = rownames(installed.packages())
required = c("dplyr", "tidyr", "readr", "sf", "googleway", "furrr", "future", "purrr", "ggplot2")
need = required[!required %in% p]
if(length(need) > 0){ install.packages(need)}
remove(p, need)
print("---packages installed.")
# return required, the list of packages you need
required
