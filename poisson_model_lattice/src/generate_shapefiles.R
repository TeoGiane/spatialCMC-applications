# # ---- POISSON MODEL ON LATTICE - GENERATE SHAPEFILES ---- # #

# Command line input options via argparser
suppressMessages(library("argparser"))
opt_parser <- arg_parser(name = "generate_shapefiles", hide.opts = TRUE,
                         description = "Generate N syntethic datasets (in shapefile format) for the simulation study")
opt_parser <- add_argument(opt_parser, arg = "--num-datasets", type = "integer", default = 1,
                           help = "Number of datasets to generate")
opt_parser <- add_argument(opt_parser, arg = "--dest-dir", type = "character", default = "input",
                           help = "Directory to save generated datasets")
extra_args <- parse_args(opt_parser)


# Preliminary checks ------------------------------------------------------

# Set working directory relative to the script location
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  # Running in RStudio
  setwd(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))
} else {
  # Running from command line
  initial.options <- commandArgs(trailingOnly = FALSE)
  script.name <- sub("--file=", "", initial.options[grep("--file=", initial.options)])
  setwd(dirname(dirname(script.name)))
}
cat("Setting working directory to: ", getwd(), "\n") # Log

# Create input directory if not present
datasets_dir <- file.path(getwd(), extra_args$dest_dir)
if(!dir.exists(datasets_dir)){
  dir.create(datasets_dir, recursive = TRUE)
}
datasets_dir <- normalizePath(datasets_dir)
cat(sprintf("Destination Directory: %s\n", datasets_dir)) # Log


# Main code ---------------------------------------------------------------

# Required libraries
suppressMessages(library("sf"))

# Unit square polygon
box <- rbind(c(0,0), c(0,1), c(1,1), c(1,0), c(0,0))
square <- st_polygon(list(box))

# Generate municipalities and province geometries
Nprov <- 3; Nmun <- 900
geom_mun <- st_make_grid(square, n = rep(sqrt(Nmun), 2), offset = c(0,0))
geom_prov <- st_make_grid(square, n = c(Nprov, 1), offset = c(0,0))

# Generate indicator for province assignment
prov_allocs <- rep(rep(c(0,1,2), each = sqrt(Nmun)/Nprov), 30)
province_idx <- lapply(unique(prov_allocs), function(x) which(prov_allocs == x))

# Generate data in municipality
Ndata <- length(geom_mun)
gamma <- c(10,20)
clust_allocs <- c(rep(c(rep(1,15),rep(2,15)),15),
                  rep(c(rep(2,15), rep(1,15)),15))

# Set seed
set.seed(230196)

for (sim in 1:extra_args$num_datasets) {
  data <- rpois(Ndata, gamma[clust_allocs])
  offsets <- rep(1, length(data))

  # Generate sf object
  df_mun <- data.frame("data" = data, "offsets" = offsets)
  mun_sf <- st_sf(df_mun, geometry = geom_mun)
  mun_sf$province_idx <- prov_allocs
  mun_sf$true_clust <- as.factor(clust_allocs)

  if(exists("mun_sf")) {
    filename <- file.path(datasets_dir, sprintf("shapefile%03d.dat", sim))
    save(mun_sf, geom_prov, file = filename)
  }
}

# # ---- END OF SCRIPT ---- # #
