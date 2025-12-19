# # ---- COVID DATA - GENERATE PLOTS ---- # #

# Command line input options via argparser
suppressMessages(library("argparser"))
opt_parser <- arg_parser(name = "generate_plots", hide.opts = TRUE,
                         description = "Generate plots for the simulation study")
opt_parser <- add_argument(opt_parser, arg = "--data-file", type = "character", default = NULL,
                           help = "Relative path to the data file to use.")
opt_parser <- add_argument(opt_parser, arg = "--shard-geom-file", type = "character", default = NULL,
                           help = "Relative path to the shard geometry file to use.")
opt_parser <- add_argument(opt_parser, arg = "--sim-file", type = "character", default = NULL,
                           help = "Relative path to the simulation file to use.")
opt_parser <- add_argument(opt_parser, arg = "--output-dir", type = "character", default = "plots",
                           help = "Relative path to save the generated plots")
extra_args <- parse_args(opt_parser)

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

# Check if input data file exists
data_file <- file.path(getwd(), extra_args$data_file)
if (!file.exists(data_file)) {
  stop(sprintf("Data file: '%s' does not exist", data_file))
}

# Check if input shard geometry file exists
shard_geom_file <- file.path(getwd(), extra_args$shard_geom_file)
if (!file.exists(shard_geom_file)) {
  stop(sprintf("Shard geometry file: '%s' does not exist", shard_geom_file))
}

# Check if input simulation file exists
sim_file <- file.path(getwd(), extra_args$sim_file)
if (!file.exists(sim_file)) {
  stop(sprintf("Simulation file: '%s' does not exist", sim_file))
}

# Create directory for output if does not exist
out_dir <- file.path(getwd(), extra_args$output_dir)
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}
cat(sprintf("Created output directory: %s\n", out_dir)) # Log


# Main code ---------------------------------------------------------------

# Required libraries
suppressMessages(library("RProtoBuf"))
suppressMessages(library("RspatialCMC"))
suppressMessages(library("ggplot2"))
suppressMessages(library("sf"))

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

# Load data from file
covid_sf <- st_read(data_file, quiet = TRUE)
shard_geom <- st_read(shard_geom_file, quiet = TRUE)
Ndata <- nrow(covid_sf)
cat(sprintf("Loaded data from: %s\n", data_file)) # Log

# Load serialized chain from file
load(sim_file)
cat(sprintf("Loaded simulation output from: %s\n", sim_file)) # Log

# Rename serialized chain according to the loaded sim_file
if (exists("CMC_fit")) {
  fit <- CMC_fit; rm(CMC_fit); run_cmc <- TRUE
} else if (exists("MCMC_fit")) {
  fit <- MCMC_fit; rm(MCMC_fit); run_cmc <- FALSE
} else {
  stop("Something went wrong: output has not been loaded")
}

# Deserialize chain
import_protobuf_messages()
chain <- sapply(fit, function(x){ read(bayesmix.AlgorithmState, x) })

# Get quantity of interest
cluster_allocs <- get_cluster_allocs(chain)
unique_values <- get_unique_values(chain)

# Compute findings from the approximated posterior distribution
Nclust <- apply(cluster_allocs, 1, function(x){length(unique(x))})
covid_sf$best_clust <- as.factor(salso::salso(cluster_allocs, loss = "VI"))

# Plot - Posterior number of clusters
plt_nclust <- ggplot(data = data.frame(prop.table(table(Nclust))), aes(x=Nclust,y=Freq)) +
  geom_bar(stat = "identity", color=NA, linewidth=0, fill='white') +
  geom_bar(stat = "identity", color='steelblue', alpha=0.4, linewidth=0.7, fill='steelblue') +
  xlab("N° of Clusters") + ylab("Post. Prob.")
# Save
cat("Saving plt_nclust.tiff... ") # Log
ggsave(file.path(out_dir, "plt_nclust.tiff"), plot = plt_nclust,
       device = "tiff", dpi=600, compression = "lzw", height = 4, width = 4)
cat("Done!\n") # Log

# Plot - Posterior similarity matrix (sorted by best cluster estimate)
# best_arrangment <- unlist(sapply(unique(covid_sf$best_clust), function(c) { which(covid_sf$best_clust == c) }))
# psm <- salso::psm(cluster_allocs[, best_arrangment])
# psm_df <- reshape2::melt(psm, c("x", "y"))
# plt_psm <- ggplot(data = psm_df) +
#   geom_tile(aes(x=x, y=y, fill=value)) +
#   scale_fill_gradient("Post. Prob. of Co-Clustering", low='white', high='darkorange',
#                       guide = guide_colorbar(title.position = "bottom", title.hjust = 0.5,
#                                              direction = "horizontal", barwidth = unit(3,"in"))) +
#   geom_rect(xmin=0.5, ymin=0.5, xmax=Ndata+0.5, ymax=Ndata+0.5, fill=NA, color='gray25', linewidth=0.7) +
#   theme_void() + theme(legend.position = "bottom") + coord_equal()
# cat("Saving plt_psm.tiff... ") # Log
# ggsave(file.path(out_dir, "plt_psm.tiff"), plot = plt_psm,
#        device = "tiff", dpi=600, compression = "lzw", height = 4, width = 4)
# cat("Done!\n") # Log

# Plot - Best cluster on the geometry
plt_best_clust <- ggplot() +
  geom_sf(data = covid_sf, aes(fill=best_clust), color='gray25', alpha=0.75, linewidth=0.1) +
  guides(fill = guide_legend(title = "Cluster", title.position = "bottom", title.hjust=0.5,
                             label.position = "bottom")) +
  theme_void() + theme(legend.position = "none")
if(run_cmc){
  plt_best_clust <- plt_best_clust +
    geom_sf(data = shard_geom, color='darkred', fill=NA)
}
cat("Saving plt_best_clust.tiff... ") # Log
ggsave(file.path(out_dir, "plt_best_clust.tiff"), plot = plt_best_clust,
       device = "tiff", dpi=600, compression = "lzw", height = 4, width = 4)
cat("Done!\n") # Log

cat(sprintf("All plots have been saved to: %s\n\n", out_dir)) # Log


# # ---- END OF SCRIPT ---- # #


# Show posterior findings
# titletext <- grid::textGrob(bquote(alpha~"="~.(alpha)~","~lambda~"="~.(lambda)),
#                             gp=grid::gpar(fontsize=16))
# gridExtra::grid.arrange(plt_nclust, plt_best_clust, ncol=2) #, top = titletext)
# plt_psm

###########################################################################

# Visualization
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


# Prove varie
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

# Prova suggerimento di mario - blocchi 5x5
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

# Prova suggerimento di mario - blocchi 6x6
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

# Un po' di codice per voronoi tessellation (Ogni punto è in un tassello)
# box <- rbind(c(0,0), c(0,1), c(1,1), c(1,0), c(0,0))
# square <- st_polygon(list(box))

# x <- runif(50); y <- runif(50)
# points <- st_cast(st_sfc(st_multipoint(cbind(x,y))), "POINT")
# tessellation <- st_collection_extract(st_voronoi(st_union(points)), "POLYGON")
# cropped_tessellation <- st_intersection(tessellation, square)

# # Plot
# plot(cropped_tessellation)
# plot(points, pch=16, col='blue', add = TRUE)

# IDEA A GRANDI LINEE PER UNA SHARD PARTITIONING SPAZIALE E "RAPPRESENTATIVA" (+ O -)
#
# A questo punto sarebbe carino avere una selezione di punti iniziale con un processo
# repulsivo dal quale è facile campionare. Da lì, genero la voronoi e mi ricavo delle
# tessellazioni contenenti più di un'area (il check è fatto in base ai centroidi)
# Dopo di che, gli inviluppi convessi di tali tessellazioni vengono raggruppati
# tramite estrazione casuale con numero di possibili outcome pari al numero finale di shard.
#
