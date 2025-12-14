# # ---- POISSON MODEL ON LATTICE - RUN SAMPLER ---- # #

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

# Extract cluster_allocation matrix from the MCMC chain
get_cluster_allocs <- function(chain) {
  t(sapply(chain, function(state){state$cluster_allocs}))
}

# Extract unique_values list from the MCMC chain
get_unique_values <- function(chain) {
  extract_unique_values <- function(cluster_state) {
    sapply(cluster_state, function(x){
      unp_x <- read(spatialcmc.PoissonState, x$custom_state$value)
      return(unp_x$rate)
    })
  }
  lapply(chain, function(state){extract_unique_values(state$cluster_states)})
}


# Main code ---------------------------------------------------------------

# Required libraries
suppressMessages(library("RspatialCMC"))
suppressMessages(library("sf"))

# Load shapefile
load(data_file)

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
  CMC_fit <- run_cmc(mun_sf$data, st_geometry(mun_sf), mun_sf$province_idx,
                     algo_params, "PoissonGamma", hier_prior, "sPP", mix_prior,
                     covariates = as.matrix(mun_sf$offsets), out_dir = dirname(out_file))
} else {
  MCMC_fit <- run_mcmc(mun_sf$data, st_geometry(mun_sf), algo_params,
                       "PoissonGamma", hier_prior, "sPP", mix_prior,
                       covariates = as.matrix(mun_sf$offsets), out_dir = dirname(out_file))
}

# Save output to file
if (exists("CMC_fit")) {
  save(CMC_fit, file = out_file)
} else if (exists("MCMC_fit")) {
  save(MCMC_fit, file = out_file)
} else {
  stop("Something went wrong: output has not been saved")
}


###########################################################################

###########################################################################
# Posterior inference -----------------------------------------------------

# # Deserialize chain
# chain <- sapply(fit, function(x){read(bayesmix.AlgorithmState,x)})

# # Get quantity of interest
# cluster_allocs <- get_cluster_allocs(chain)
# unique_values <- get_unique_values(chain)

# # Compute findings from the approximated posterior distribution
# Nclust <- apply(cluster_allocs, 1, function(x){length(unique(x))})
# sf_mun$best_clust <- as.factor(salso::salso(cluster_allocs, loss = "VI"))

# # Plot - Posterior number of clusters
# plt_nclust <- ggplot(data = data.frame(prop.table(table(Nclust))), aes(x=Nclust,y=Freq)) +
#   geom_bar(stat = "identity", color=NA, linewidth=0, fill='white') +
#   geom_bar(stat = "identity", color='steelblue', alpha=0.4, linewidth=0.7, fill='steelblue') +
#   xlab("N° of Clusters") + ylab("Post. Prob.")
# # pdf("plt_nclust.pdf", height = 4, width = 4); plt_nclust; dev.off()
# plt_nclust

# # Plot - Posterior similarity matrix
# # plt_psm <- ggplot(data = reshape2::melt(psm, c("x", "y"))) +
# #   geom_tile(aes(x=x, y=y, fill=value)) +
# #   scale_fill_gradient("Post. Prob. of Co-Clustering", low='steelblue', high='darkorange',
# #                       guide = guide_colorbar(title.position = "bottom", title.hjust = 0.5,
# #                                              direction = "horizontal", barwidth = unit(3,"in"))) +
# #   geom_rect(xmin=0.5, ymin=0.5, xmax=Ndata+0.5, ymax=Ndata+0.5, fill=NA, color='gray25', linewidth=0.7) +
# #   theme_void() + theme(legend.position = "bottom") + coord_equal()

# # Plot - Best cluster on the geometry
# # plt_true_clust <- ggplot() +
# #   geom_sf(data = sf_mun, aes(fill=true_clust), color='gray25', linewidth=0.5, alpha=0.75) +
# #   geom_sf(data = geom_prov, color='darkred', fill=NA, linewidth=2) +
# #   scale_fill_manual(values = c("1" = "steelblue", "2" = "darkorange")) +
# #   guides(fill = guide_legend(title = "Cluster", title.position = "bottom", title.hjust=0.5,
# #                              label.position = "bottom", keywidth = unit(1,"cm"))) +
# #   theme_void() + theme(legend.position = "none")

# plt_best_clust <- ggplot() +
#   geom_sf(data = sf_mun, aes(fill=best_clust), color='gray25', linewidth=0.5, alpha=0.75) +
#   scale_fill_manual(values = c("1" = "steelblue", "2" = "darkorange")) +
#   guides(fill = guide_legend(title = "Cluster", title.position = "bottom", title.hjust=0.5,
#                              label.position = "bottom")) +
#   theme_void() + theme(legend.position = "bottom")
# if(run_cmc){
#   plt_best_clust <- plt_best_clust +
#     geom_sf(data = geom_prov, color='darkred', fill=NA, linewidth=2)
# }
# plt_best_clust

# # Show posterior findings
# # titletext <- grid::textGrob(bquote(alpha~"="~.(alpha)~","~lambda~"="~.(lambda)),
# #                             gp=grid::gpar(fontsize=16))
# # gridExtra::grid.arrange(plt_nclust, plt_best_clust, ncol=2) #, top = titletext)
# # plt_psm

# ###########################################################################

# # Visualization
# df_text <- data.frame("label" = c(1,2,3), st_coordinates(st_centroid(geom_prov)))
# plt_shardsplit <- ggplot() +
#   geom_sf(data = geom_prov, fill=NA, color='gray25', linewidth=1) +
#   geom_text(data = df_text, aes(x=X,y=Y,label=label), size=8, color='gray25') +
#   theme_void()
# pdf("plt_shardsplit.pdf", height = 4, width = 4); plt_shardsplit; dev.off()

# plt_data <- ggplot() +
#   geom_sf(data = sf_mun, aes(fill=data), color='gray25', linewidth=0.5) +
#   scale_fill_gradient(low = 'steelblue', high = 'darkorange') +
#   guides(fill = guide_colorbar(title = "Data", direction = "horizontal", title.hjust=0.5,
#                                title.position = "bottom", label.position = "bottom", keywidth = unit(2.5,"in"))) +
#   theme_void() + theme(legend.position = "bottom")
# pdf("plt_data.pdf", height = 4, width = 4); plt_data; dev.off()

# plt_true_clust <- ggplot() +
#   geom_sf(data = sf_mun, aes(fill=true_clust), color='gray25', linewidth=0.5, alpha=0.75) +
#   geom_sf(data = geom_prov, color='darkred', fill=NA, linewidth=2) +
#   scale_fill_manual(values = c("1" = "steelblue", "2" = "darkorange")) +
#   guides(fill = guide_legend(title = "Cluster", title.position = "bottom", title.hjust=0.5,
#                              label.position = "bottom")) +
#   theme_void() + theme(legend.position = "bottom")
# pdf("plt_true_clust.pdf", height = 4, width = 4); plt_true_clust; dev.off()


# # Prove varie
# cell_size <- c(diff(st_bbox(geom_mun[1])[c(1,3)]), diff(st_bbox(geom_mun[1])[c(2,4)]))
# prov_1 <- st_make_grid(square, n = c(18,18), offset = c(6*cell_size[1], 6*cell_size[2]), cellsize = cell_size)
# prov_2 <- st_difference(st_make_grid(square, n = c(24,24), offset = c(3*cell_size[1], 3*cell_size[2]), cellsize = cell_size), st_union(prov_1))
# prov_3 <- st_difference(st_make_grid(square, n = c(30,30), offset = c(0*cell_size[1], 0*cell_size[2]), cellsize = cell_size),
#                         st_union(st_union(prov_1), st_union(prov_2)))
# geom_prov <- st_as_sfc(rbind(st_union(prov_1), st_union(prov_2), st_union(prov_3)))

# # plot(geom_mun); plot(prov_1, col='red', add=T); plot(prov_2, col='blue', add=T); plot(prov_3, col='green', add=T)
# shard_geoms <- list(prov_1, prov_2, prov_3)
# get_prov_allocs <- function(geom_mun, shard_geoms){
#   out <- numeric(length(geom_mun))
#   for (i in 1:length(shard_geoms)) {
#     idx <- unlist(st_contains(shard_geoms[[i]], st_centroid(geom_mun)))
#     out[idx] <- (i-1)
#   }
#   return(out)
# }

# prov_allocs <- get_prov_allocs(geom_mun, shard_geoms)

# # Prova suggerimento di mario - blocchi 5x5
# cell_size <- c(diff(st_bbox(geom_mun[1])[c(1,3)]), diff(st_bbox(geom_mun[1])[c(2,4)]))
# geom_prov <- st_make_grid(square, n = c(6,6), offset = c(0,0))
# prvidx <- sample(1:3, length(geom_prov), replace = T)
# geom_prov <- st_as_sfc(rbind(st_union(geom_prov[prvidx == 1]), st_union(geom_prov[prvidx == 2]), st_union(geom_prov[prvidx == 3])))

# get_prov_allocs <- function(geom_mun, geom_prov){
#   out <- numeric(length(geom_mun))
#   for (i in 1:length(geom_prov)) {
#     idx <- unlist(st_contains(geom_prov[i], st_centroid(geom_mun)))
#     out[idx] <- (i-1)
#   }
#   return(out)
# }

# prov_allocs <- get_prov_allocs(geom_mun, geom_prov)

# # Prova suggerimento di mario - blocchi 6x6
# cell_size <- c(diff(st_bbox(geom_mun[1])[c(1,3)]), diff(st_bbox(geom_mun[1])[c(2,4)]))
# geom_prov <- st_make_grid(square, n = c(5,5), offset = c(0,0))
# prvidx <- sample(1:3, length(geom_prov), replace = T)
# geom_prov <- st_as_sfc(rbind(st_union(geom_prov[prvidx == 1]), st_union(geom_prov[prvidx == 2]), st_union(geom_prov[prvidx == 3])))

# get_prov_allocs <- function(geom_mun, geom_prov){
#   out <- numeric(length(geom_mun))
#   for (i in 1:length(geom_prov)) {
#     idx <- unlist(st_contains(geom_prov[i], st_centroid(geom_mun)))
#     out[idx] <- (i-1)
#   }
#   return(out)
# }

# prov_allocs <- get_prov_allocs(geom_mun, geom_prov)

# # Un po' di codice per voronoi tessellation (Ogni punto è in un tassello)
# box <- rbind(c(0,0), c(0,1), c(1,1), c(1,0), c(0,0))
# square <- st_polygon(list(box))

# x <- runif(50); y <- runif(50)
# points <- st_cast(st_sfc(st_multipoint(cbind(x,y))), "POINT")
# tessellation <- st_collection_extract(st_voronoi(st_union(points)), "POLYGON")
# cropped_tessellation <- st_intersection(tessellation, square)

# # Plot
# plot(cropped_tessellation)
# plot(points, pch=16, col='blue', add = TRUE)

# # IDEA A GRANDI LINEE PER UNA SHARD PARTITIONING SPAZIALE E "RAPPRESENTATIVA" (+ O -)
# #
# # A questo punto sarebbe carino avere una selezione di punti iniziale con un processo
# # repulsivo dal quale è facile campionare. Da lì, genero la voronoi e mi ricavo delle
# # tessellazioni contenenti più di un'area (il check è fatto in base ai centroidi)
# # Dopo di che, gli inviluppi convessi di tali tessellazioni vengono raggruppati
# # tramite estrazione casuale con numero di possibili outcome pari al numero finale di shard.
# #
