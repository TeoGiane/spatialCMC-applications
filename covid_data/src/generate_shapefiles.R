# # ---- COVID DATA - GENERATE PROCESSED DATASET ---- # #

# Command line input options via argparser
suppressMessages(library("argparser"))
opt_parser <- arg_parser(name = "generate_shapefiles", hide.opts = TRUE,
                         description = "Generate processed COVID-19 dataset from raw data")
opt_parser <- add_argument(opt_parser, arg = "--raw-data-dir", type = "character",
                           help = "Relative path to the directory containing raw data files", default = "raw")
opt_parser <- add_argument(opt_parser, arg = "--dest-dir", type = "character", default = "input",
                           help = "Relative path to the directory to save generated datasets")
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
cat("Setting working directory to: ", getwd(), "\n")

# Check if directories exist
raw_data_dir <- file.path(getwd(), extra_args$raw_data_dir)
if (!dir.exists(raw_data_dir)) {
  stop(paste("Raw data directory", raw_data_dir, "does not exist!"))
}

# Create destination directory if it doesn't exist
dest_dir <- file.path(getwd(), extra_args$dest_dir)
if (!dir.exists(dest_dir)) {
  dir.create(dest_dir, recursive = TRUE)
  cat("Created destination directory: ", dest_dir, "\n")
}


# Main code ---------------------------------------------------------------

# Load required libraries
suppressMessages(library("dplyr"))
suppressMessages(library("lubridate"))
suppressMessages(library("sf"))

# Import raw shapefiles
cat("Importing shapefiles... ")
reg_sf <- st_read(file.path(raw_data_dir,"Limiti01012020","Reg01012020","Reg01012020_WGS84.shp"), quiet = TRUE) %>% st_make_valid() %>% st_cast()
prov_sf <- st_read(file.path(raw_data_dir,"Limiti01012020","ProvCM01012020","ProvCM01012020_WGS84.shp"), quiet = TRUE) %>% st_make_valid() %>% st_cast()
mun_sf <- st_read(file.path(raw_data_dir,"Limiti01012020","Com01012020","Com01012020_WGS84.shp"), quiet = TRUE) %>% st_make_valid()
cat("Done!\n")

# Import raw data from all sources
cat("Importing raw data... ")
mortality_df_raw <- read.csv(file.path(raw_data_dir, "comuni_giornaliero_31dicembre.csv"), encoding = "latin1")
population_df_raw <- read.csv(file.path(raw_data_dir,"POSAS_2021_it_Comuni.csv"), sep = ";", skip = 1)
income_df_names <- names(read.csv(file.path(raw_data_dir,"Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2020.csv"), sep=";", header = T, nrows = 0))
income_df_raw <- read.csv(file.path(raw_data_dir ,"Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2020.csv"), sep = ";", skip = 1, header = F) %>%
  select(-c("V51","V52")) %>% `colnames<-`(income_df_names) %>% filter(Regione != "Mancante/errata")
rm(income_df_names)
cat("Done!\n")

# Merge Montericcardo into Pesaro municipality
cat("Merging Monteciccardo into Pesaro municipality... ")
merged_mun <- mun_sf %>%
  filter(COMUNE %in% c("Monteciccardo", "Pesaro")) %>%
  summarise(COD_RIP = 3, COD_REG = 11, COD_PROV = 41, COD_CM = 0, COD_UTS = 41,
    PRO_COM = 41044, PRO_COM_T = "041044", COMUNE = "Pesaro", COMUNE_A = NA, CC_UTS = 1,
    SHAPE_LENG = as.numeric(st_length(st_boundary(st_union(geometry)))),
    SHAPE_AREA = as.numeric(st_area(st_union(geometry))),
    geometry = st_union(geometry)
  )
mun_sf <- mun_sf %>% filter(!COMUNE %in% c("Monteciccardo", "Pesaro")) %>% bind_rows(merged_mun)
rm(merged_mun)
cat("Done!\n")

# Data clean-up to extract useful information
cat("Cleaning and processing data... ")
mortality_df <- mortality_df_raw %>%
  mutate(ME = as.factor(month(as.Date(sprintf("2024%04g", GE), format="%Y%m%d")))) %>%
  rename(COD_REG = REG, COD_PROV = PROV) %>%  filter(ME %in% c("3","4","5")) %>%
  select(-c("TIPO_COMUNE", "GE", matches("(M|F)_[0-9+]"))) %>%
  group_by(COD_REG, COD_PROV, NOME_REGIONE, NOME_PROVINCIA, NOME_COMUNE, COD_PROVCOM) %>%
  summarise(across(matches("T_[0-9+]"), sum), .groups = "drop") %>%
  rowwise() %>% mutate(ET = mean(c_across(T_11:T_19), na.rm = TRUE)) %>% ungroup() %>%
  select(!matches("T_1[1-9]"))
cat("Done!\n")

# Computing the % ofover-65 population and population density in each municipality
cat("Computing population statistics... ")
population_df <- population_df_raw %>%
  filter(Età != 999) %>%
  group_by(Codice.comune, Comune) %>%
  summarise(TotalPop = sum(Totale), Over65 = sum(if_else(Età >= 65, Totale, 0)), .groups = "drop") %>%
  mutate(PercOver65 = Over65 / TotalPop) %>%
  left_join(mun_sf, by = c("Codice.comune" = "PRO_COM")) %>%
  mutate(PopDens = TotalPop / (SHAPE_AREA * 1e-6)) %>%
  select(Codice.comune, Comune, TotalPop, PercOver65, PopDens)
cat("Done!\n")

# Compute Average Personal Income in each municipality
cat("Computing income statistics... ")
income_df <- income_df_raw %>%
  select(!matches("Reddito.[^c]|Imposta|Bonus|Addizionale|Reddito.complessivo.m")) %>% rowwise() %>%
  mutate(TotFreq = sum(c_across(matches("Frequenza")), na.rm = TRUE),
         TotIncome = sum(c_across(matches("Ammontare.in.euro")), na.rm = TRUE),
         MeanIncome = TotIncome / TotFreq) %>% ungroup() %>%
  select(Codice.Istat.Comune, Denominazione.Comune, TotFreq, TotIncome, MeanIncome)
cat("Done!\n")

# Revert Codice istat for Montecopiolo and Sassofeltrio municipalities (Referendum Mismatch, easier in this way)
income_df$Codice.Istat.Comune[income_df$Denominazione.Comune == "MONTECOPIOLO"] <- mun_sf$PRO_COM[mun_sf$COMUNE == "Montecopiolo"]
income_df$Codice.Istat.Comune[income_df$Denominazione.Comune == "SASSOFELTRIO"] <- mun_sf$PRO_COM[mun_sf$COMUNE == "Sassofeltrio"]

# Merging data from all sources
cat("Merging all datasets... ")
real_data_sf <- mun_sf %>%
  left_join(mortality_df, by = c("PRO_COM" = "COD_PROVCOM", "COD_REG", "COD_PROV")) %>%
  left_join(population_df, by = c("PRO_COM" = "Codice.comune")) %>%
  left_join(income_df, by = c("PRO_COM" = "Codice.Istat.Comune")) %>%
  select(COD_REG, NOME_REGIONE, COD_PROV, NOME_PROVINCIA, PRO_COM, COMUNE, T_20, ET, TotalPop, PercOver65, PopDens, TotFreq, TotIncome, MeanIncome) %>%
  rename(COD_MUN = PRO_COM, NAME_REG = NOME_REGIONE, NAME_PROV = NOME_PROVINCIA, NAME_MUN = COMUNE) %>%
  mutate(TotIncome = as.character(TotIncome))
cat("Done!\n")

# Fill-up info from two municipalities with missing data
to_fill <- which(is.na(real_data_sf$ET))
real_data_sf[to_fill, "NAME_REG"] <- "Piemonte"
real_data_sf[to_fill, "NAME_PROV"] <- c("Cuneo","Vercelli")

# Remove data for which ET == 0
real_data_sf <- real_data_sf[which(real_data_sf$ET != 0),]
real_data_sf <- real_data_sf %>% st_make_valid() %>% st_cast()

# Create reduced dataset for northern Italy only
north_regions <- c("Valle d'Aosta/Vallée d'Aoste", "Piemonte", "Liguria", "Lombardia",
                   "Trentino-Alto Adige/Südtirol", "Veneto", "Friuli-Venezia Giulia", "Emilia-Romagna")
north_data_sf <- real_data_sf %>% filter(NAME_REG %in% north_regions)
north_reg_sf <- reg_sf %>% filter(COD_REG %in% unique(north_data_sf$COD_REG))
north_prov_sf <- prov_sf %>% filter(COD_REG %in% unique(north_data_sf$COD_REG))

# Create proper output directories
dir.create(file.path(dest_dir), recursive = T, showWarnings = F)

# Save shapefiles in GPKG format
st_write(real_data_sf, file.path(dest_dir, "covid_data_fullitaly.gpkg"), delete_layer = TRUE, quiet = TRUE)
cat("Processed COVID-19 dataset for all of Italy saved to", file.path(dest_dir, "covid_data_fullitaly.gpkg"), "\n") # log

st_write(north_data_sf, file.path(dest_dir, "covid_data_northitaly.gpkg"), delete_layer = TRUE, quiet = TRUE)
cat("Processed COVID-19 dataset for northern Italy saved to", file.path(dest_dir, "covid_data_northitaly.gpkg"), "\n") # log

st_write(reg_sf, file.path(dest_dir, "regions_fullitaly.gpkg"), delete_layer = TRUE, quiet = TRUE)
cat("Regions shapefile for all of Italy saved to", file.path(dest_dir, "regions_fullitaly.gpkg"), "\n") # log

st_write(north_reg_sf, file.path(dest_dir, "regions_northitaly.gpkg"), delete_layer = TRUE, quiet = TRUE)
cat("Regions shapefile for northern Italy saved to", file.path(dest_dir, "regions_northitaly.gpkg"), "\n") # log

st_write(prov_sf, file.path(dest_dir, "provinces_fullitaly.gpkg"), delete_layer = TRUE, quiet = TRUE)
cat("Provinces shapefile for all of Italy saved to", file.path(dest_dir, "provinces_fullitaly.gpkg"), "\n") # log

st_write(north_prov_sf, file.path(dest_dir, "provinces_northitaly.gpkg"), delete_layer = TRUE, quiet = TRUE)
cat("Provinces shapefile for northern Italy saved to", file.path(dest_dir, "provinces_northitaly.gpkg"), "\n") # log

# # ---- END OF SCRIPT ---- # #