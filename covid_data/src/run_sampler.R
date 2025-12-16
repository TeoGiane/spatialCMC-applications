# # ---- COVID DATA - RUN SAMPLER ---- # #

# Command line input options via argparser
suppressMessages(library("argparser"))
opt_parser <- arg_parser(name = "run_sampler", hide.opts = TRUE,
                         description = "Run RspatialCMC sampler (CMC or MCMC) on the data stored in 'input-file'")
opt_parser <- add_argument(opt_parser, arg = "--input-file", type = "character",
                           help = "Relative path to input data file")
opt_parser <- add_argument(opt_parser, arg = "--use-mcmc", flag=TRUE,
                           help = "Use MCMC algorithm to fit the data?")
opt_parser <- add_argument(opt_parser, arg = "--hier-prior", type = "character",
                           help = "Hierarchy prior in google::protobuf ASCII format")
opt_parser <- add_argument(opt_parser, arg = "--mix-prior", type = "character",
                           help = "Mixing prior in google::protobuf ASCII format")
opt_parser <- add_argument(opt_parser, arg = "--algo-params", type = "character",
                           help = "Algorithm parameters in google::protobuf ASCII format")
opt_parser <- add_argument(opt_parser, arg = "--output-file", type = "character", default = "./output/chain.dat",
                           help = "Relative path to the output file")
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

# Check if input file exits
data_file <- file.path(getwd(), extra_args$input_file)
if(!file.exists(data_file)){
  stop(sprintf("%s does not exist", data_file))
}
data_file <- normalizePath(data_file)
cat(sprintf("Data file: %s\n", data_file)) # Log

# Create directory for output if does not exist
out_file <- file.path(getwd(), extra_args$output_file)
if(!dir.exists(dirname(out_file))) {
  dir.create(dirname(out_file), recursive = TRUE)
}
cat(sprintf("Output directory: %s\n", normalizePath(dirname(out_file)))) # Log


# Main code ---------------------------------------------------------------

# Required libraries
suppressMessages(library("RspatialCMC"))
suppressMessages(library("sf"))

# Load shapefile
covid_sf <- st_read(extra_args$input_file, quiet = TRUE)
cat(sprintf("Data loaded from shapefile: %s\n", extra_args$input_file)) # Log

# Choose if MCMC or CMC
run_mcmc <- extra_args$use_mcmc
cat(sprintf("run_mcmc: %s\n", run_mcmc))

# Set hierarchy parameters
hier_prior = extra_args$hier_prior
if(is.null(hier_prior)){
  stop("Missing '--hier-prior' parameter")
} else {
  cat(sprintf("Hierarchy prior: %s\n", hier_prior)) # Log
}

# Set mixing parameters
mix_prior = extra_args$mix_prior
if(is.null(mix_prior)){
  stop("Missing '--mix-prior' parameter")
} else {
  cat(sprintf("Mixing prior: %s\n", mix_prior)) # Log
}

# Set algorithm parameters
algo_params = extra_args$algo_params
if(is.null(algo_params)){
  stop("Missing '--algo-params' parameter")
} else {
  cat(sprintf("Algorithm parameters: %s\n", algo_params)) # Log
}

# Run SpatialCMC sampler (either MCMC or CMC)
if(!run_mcmc) {
  shard_alloc <- covid_sf$COD_REG - 1L
  CMC_fit <- run_cmc(as.numeric(covid_sf$T_20), st_geometry(covid_sf), shard_alloc,
                     algo_params, "PoissonGamma", hier_prior, "sPP", mix_prior, covariates = as.matrix(covid_sf$ET))
} else {
  MCMC_fit <- run_mcmc(as.numeric(covid_sf$T_20), st_geometry(covid_sf), algo_params,
                       "PoissonGamma", hier_prior, "sPP", mix_prior, covariates = as.matrix(covid_sf$ET))
}

# Save output to file
if (exists("CMC_fit")) {
  save(CMC_fit, file = out_file)
  cat(sprintf("CMC output saved to %s\n", out_file)) # Log
} else if (exists("MCMC_fit")) {
  save(MCMC_fit, file = out_file)
  cat(sprintf("MCMC output saved to %s\n", out_file)) # Log
} else {
  stop("Something went wrong: output has not been saved")
}

# # ---- END OF SCRIPT ---- # #
