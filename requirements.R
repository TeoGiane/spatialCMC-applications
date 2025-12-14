# Choose repository
repos <- "https://cloud.r-project.org/"

# List of required packages
package_deps <- c("argparser",
                  "devtools",
                  "dplyr",
                  "ggplot2",
                  "lubridate",
                  "ggmap",
                  "gridExtra",
                  "reshape2",
                  "RProtoBuf",
                  "salso",
                  "sf",
                  "spdep")

# Install missing packages from CRAN (in parallel)
new_packages <- package_deps[!(package_deps %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  num_cores <- parallel::detectCores() - 1
  install.packages(new_packages, repos = repos, Ncpus = num_cores)
}
