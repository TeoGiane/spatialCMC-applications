# Choose repository
repos <- "https://cloud.r-project.org/"

# List of required packages
package_deps <- c("argparser",
                  "devtools",
                  "dplyr",
                  "ggplot2",
                  "ggmap",
                  "gridExtra",
                  "reshape2",
                  "RProtoBuf",
                  "salso",
                  "sf",
                  "spdep")

# Install packages from CRAN (in parallel)
num_cores <- parallel::detectCores() - 1
install.packages(package_deps, repos = repos, Ncpus = num_cores)
