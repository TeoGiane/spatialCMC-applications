# # ---- First test on SBNP Clustering for Massive datasets ---- # #

# Required libraries
library("RspatialCMC")
library("ggplot2")
library("parallel")
library("sf")

# Build spatialCMC (if necessary)
# build_spatialCMC()

###########################################################################
# Auxiliary functions -----------------------------------------------------

# # Extract cluster_allocation matrix from the MCMC chain
get_cluster_allocs <- function(chain) {
  t(sapply(chain, function(state){state$cluster_allocs}))
}

# # Extract unique_values list from the MCMC chain
get_unique_values <- function(chain) {
  extract_unique_values <- function(cluster_state) {
    sapply(cluster_state, function(x){
      unp_x <- read(spatialcmc.PoissonState, x$custom_state$value)
      return(unp_x$rate)
    })
  }
  lapply(chain, function(state){extract_unique_values(state$cluster_states)})
}

###########################################################################

###########################################################################
# Generate Data and Set Parameters ----------------------------------------

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
set.seed(1996)
data <- rpois(Ndata, gamma[clust_allocs])
offsets <- rep(1, length(data))

# Generate sf object
df_mun <- data.frame("data" = data, "offsets" = offsets)
sf_mun <- st_sf(df_mun, geometry = geom_mun)
sf_mun$province_idx <- prov_allocs
sf_mun$true_clust <- as.factor(clust_allocs)

# Generate common timestamp (for scenario and output matching)
# timestamp <- format(Sys.time(), "%Y%m%d-%H%M")

# Save generated scenario scenario
# filename <- sprintf("%s/input/scenario1_%s.dat", getwd(), timestamp)
# save.image(file = filename)

###########################################################################
# SpatialCMC run ----------------------------------------------------------

# Choose if MCMC or CMC
run_cmc <- TRUE

# Set hierarchy parameters
hier_prior =
  "
  fixed_values {
    shape: 3.25
    rate: 0.25
  }
  "

# Set mixing parameters
mix_prior =
  "
  fixed_value {
    totalmass: 1.0
    lambda: 0.35
  }
  "

# Set algorithm parameters
algo_params =
  "
  algo_id: 'Neal2'
  rng_seed: 10092022
  iterations: 4000
  burnin: 1000
  init_num_clusters: 5
  "

# Run SpatialCMC sampler (either MCMC or CMC)
if(run_cmc){
  fit <- run_cmc(sf_mun$data, st_geometry(sf_mun), sf_mun$province_idx,
                 algo_params, "PoissonGamma", hier_prior, "sPP", mix_prior,
                 covariates = as.matrix(sf_mun$offsets))
} else {
  fit <- run_mcmc(sf_mun$data, st_geometry(sf_mun),
                  algo_params, "PoissonGamma", hier_prior, "sPP", mix_prior,
                  covariates = as.matrix(sf_mun$offsets))
}

###########################################################################

###########################################################################
# Posterior inference -----------------------------------------------------

# Deserialize chain
chain <- sapply(fit, function(x){read(bayesmix.AlgorithmState,x)})

# Get quantity of interest
cluster_allocs <- get_cluster_allocs(chain)
unique_values <- get_unique_values(chain)

# Compute findings from the approximated posterior distribution
Nclust <- apply(cluster_allocs, 1, function(x){length(unique(x))})
sf_mun$best_clust <- as.factor(salso::salso(cluster_allocs, loss = "VI"))

# Plot - Posterior number of clusters
plt_nclust <- ggplot(data = data.frame(prop.table(table(Nclust))), aes(x=Nclust,y=Freq)) +
  geom_bar(stat = "identity", color=NA, linewidth=0, fill='white') +
  geom_bar(stat = "identity", color='steelblue', alpha=0.4, linewidth=0.7, fill='steelblue') +
  xlab("N° of Clusters") + ylab("Post. Prob.")
# pdf("plt_nclust.pdf", height = 4, width = 4); plt_nclust; dev.off()
plt_nclust

# Plot - Posterior similarity matrix
# plt_psm <- ggplot(data = reshape2::melt(psm, c("x", "y"))) +
#   geom_tile(aes(x=x, y=y, fill=value)) +
#   scale_fill_gradient("Post. Prob. of Co-Clustering", low='steelblue', high='darkorange',
#                       guide = guide_colorbar(title.position = "bottom", title.hjust = 0.5,
#                                              direction = "horizontal", barwidth = unit(3,"in"))) +
#   geom_rect(xmin=0.5, ymin=0.5, xmax=Ndata+0.5, ymax=Ndata+0.5, fill=NA, color='gray25', linewidth=0.7) +
#   theme_void() + theme(legend.position = "bottom") + coord_equal()

# Plot - Best cluster on the geometry
# plt_true_clust <- ggplot() +
#   geom_sf(data = sf_mun, aes(fill=true_clust), color='gray25', linewidth=0.5, alpha=0.75) +
#   geom_sf(data = geom_prov, color='darkred', fill=NA, linewidth=2) +
#   scale_fill_manual(values = c("1" = "steelblue", "2" = "darkorange")) +
#   guides(fill = guide_legend(title = "Cluster", title.position = "bottom", title.hjust=0.5,
#                              label.position = "bottom", keywidth = unit(1,"cm"))) +
#   theme_void() + theme(legend.position = "none")

plt_best_clust <- ggplot() +
  geom_sf(data = sf_mun, aes(fill=best_clust), color='gray25', linewidth=0.5, alpha=0.75) +
  scale_fill_manual(values = c("1" = "steelblue", "2" = "darkorange")) +
  guides(fill = guide_legend(title = "Cluster", title.position = "bottom", title.hjust=0.5,
                             label.position = "bottom")) +
  theme_void() + theme(legend.position = "bottom")
if(run_cmc){
  plt_best_clust <- plt_best_clust +
    geom_sf(data = geom_prov, color='darkred', fill=NA, linewidth=2)
}
plt_best_clust

# Show posterior findings
# titletext <- grid::textGrob(bquote(alpha~"="~.(alpha)~","~lambda~"="~.(lambda)),
#                             gp=grid::gpar(fontsize=16))
# gridExtra::grid.arrange(plt_nclust, plt_best_clust, ncol=2) #, top = titletext)
# plt_psm

###########################################################################

# Visualization
df_text <- data.frame("label" = c(1,2,3), st_coordinates(st_centroid(geom_prov)))
plt_shardsplit <- ggplot() +
  geom_sf(data = geom_prov, fill=NA, color='gray25', linewidth=1) +
  geom_text(data = df_text, aes(x=X,y=Y,label=label), size=8, color='gray25') +
  theme_void()
pdf("plt_shardsplit.pdf", height = 4, width = 4); plt_shardsplit; dev.off()

plt_data <- ggplot() +
  geom_sf(data = sf_mun, aes(fill=data), color='gray25', linewidth=0.5) +
  scale_fill_gradient(low = 'steelblue', high = 'darkorange') +
  guides(fill = guide_colorbar(title = "Data", direction = "horizontal", title.hjust=0.5,
                               title.position = "bottom", label.position = "bottom", keywidth = unit(2.5,"in"))) +
  theme_void() + theme(legend.position = "bottom")
pdf("plt_data.pdf", height = 4, width = 4); plt_data; dev.off()

plt_true_clust <- ggplot() +
  geom_sf(data = sf_mun, aes(fill=true_clust), color='gray25', linewidth=0.5, alpha=0.75) +
  geom_sf(data = geom_prov, color='darkred', fill=NA, linewidth=2) +
  scale_fill_manual(values = c("1" = "steelblue", "2" = "darkorange")) +
  guides(fill = guide_legend(title = "Cluster", title.position = "bottom", title.hjust=0.5,
                             label.position = "bottom")) +
  theme_void() + theme(legend.position = "bottom")
pdf("plt_true_clust.pdf", height = 4, width = 4); plt_true_clust; dev.off()


# Prove varie
cell_size <- c(diff(st_bbox(geom_mun[1])[c(1,3)]), diff(st_bbox(geom_mun[1])[c(2,4)]))
prov_1 <- st_make_grid(square, n = c(18,18), offset = c(6*cell_size[1], 6*cell_size[2]), cellsize = cell_size)
prov_2 <- st_difference(st_make_grid(square, n = c(24,24), offset = c(3*cell_size[1], 3*cell_size[2]), cellsize = cell_size), st_union(prov_1))
prov_3 <- st_difference(st_make_grid(square, n = c(30,30), offset = c(0*cell_size[1], 0*cell_size[2]), cellsize = cell_size),
                        st_union(st_union(prov_1), st_union(prov_2)))
geom_prov <- st_as_sfc(rbind(st_union(prov_1), st_union(prov_2), st_union(prov_3)))

# plot(geom_mun); plot(prov_1, col='red', add=T); plot(prov_2, col='blue', add=T); plot(prov_3, col='green', add=T)
shard_geoms <- list(prov_1, prov_2, prov_3)
get_prov_allocs <- function(geom_mun, shard_geoms){
  out <- numeric(length(geom_mun))
  for (i in 1:length(shard_geoms)) {
    idx <- unlist(st_contains(shard_geoms[[i]], st_centroid(geom_mun)))
    out[idx] <- (i-1)
  }
  return(out)
}

prov_allocs <- get_prov_allocs(geom_mun, shard_geoms)

# Prova suggerimento di mario - blocchi 5x5
cell_size <- c(diff(st_bbox(geom_mun[1])[c(1,3)]), diff(st_bbox(geom_mun[1])[c(2,4)]))
geom_prov <- st_make_grid(square, n = c(6,6), offset = c(0,0))
prvidx <- sample(1:3, length(geom_prov), replace = T)
geom_prov <- st_as_sfc(rbind(st_union(geom_prov[prvidx == 1]), st_union(geom_prov[prvidx == 2]), st_union(geom_prov[prvidx == 3])))

get_prov_allocs <- function(geom_mun, geom_prov){
  out <- numeric(length(geom_mun))
  for (i in 1:length(geom_prov)) {
    idx <- unlist(st_contains(geom_prov[i], st_centroid(geom_mun)))
    out[idx] <- (i-1)
  }
  return(out)
}

prov_allocs <- get_prov_allocs(geom_mun, geom_prov)

# Prova suggerimento di mario - blocchi 6x6
cell_size <- c(diff(st_bbox(geom_mun[1])[c(1,3)]), diff(st_bbox(geom_mun[1])[c(2,4)]))
geom_prov <- st_make_grid(square, n = c(5,5), offset = c(0,0))
prvidx <- sample(1:3, length(geom_prov), replace = T)
geom_prov <- st_as_sfc(rbind(st_union(geom_prov[prvidx == 1]), st_union(geom_prov[prvidx == 2]), st_union(geom_prov[prvidx == 3])))

get_prov_allocs <- function(geom_mun, geom_prov){
  out <- numeric(length(geom_mun))
  for (i in 1:length(geom_prov)) {
    idx <- unlist(st_contains(geom_prov[i], st_centroid(geom_mun)))
    out[idx] <- (i-1)
  }
  return(out)
}

prov_allocs <- get_prov_allocs(geom_mun, geom_prov)

# Un po' di codice per voronoi tessellation (Ogni punto è in un tassello)
box <- rbind(c(0,0), c(0,1), c(1,1), c(1,0), c(0,0))
square <- st_polygon(list(box))

x <- runif(50); y <- runif(50)
points <- st_cast(st_sfc(st_multipoint(cbind(x,y))), "POINT")
tessellation <- st_collection_extract(st_voronoi(st_union(points)), "POLYGON")
cropped_tessellation <- st_intersection(tessellation, square)

# Plot
plot(cropped_tessellation)
plot(points, pch=16, col='blue', add = TRUE)

# IDEA A GRANDI LINEE PER UNA SHARD PARTITIONING SPAZIALE E "RAPPRESENTATIVA" (+ O -)
#
# A questo punto sarebbe carino avere una selezione di punti iniziale con un processo
# repulsivo dal quale è facile campionare. Da lì, genero la voronoi e mi ricavo delle
# tessellazioni contenenti più di un'area (il check è fatto in base ai centroidi)
# Dopo di che, gli inviluppi convessi di tali tessellazioni vengono raggruppati
# tramite estrazione casuale con numero di possibili outcome pari al numero finale di shard.
#
