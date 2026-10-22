# SET UP ####

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(terra)  # mapping
library(geodata)  # country maps
library(fuzzySim)  # splist2presabs()
library(modEvA)  # arrangePlots()


# IMPORT SPECIES AND EFFORT DATA ####

dat_in <- read.csv("../inputs/species/species.csv")
nrow(dat_in)
head(dat_in)
unique(dat_in$species)  # 6 species

sampled_cells <- read.csv("../inputs/species/sampled_cells.csv")
nrow(sampled_cells)
head(sampled_cells)
sampled_cells <- sampled_cells[,1]


# shape data for modelling ####

# create species abbreviations:
dat_in$spcode <- fuzzySim::spCodes(dat_in$species)

# transpose to wide format:
dat <- fuzzySim::splist2presabs(dat_in, sites.col = "utm10", sp.col = "spcode")
head(dat)

# export species data to disk:
dir.create("../outputs/tables", showWarnings = FALSE)
write.csv(dat, "../outputs/tables/dat_species.csv", row.names = FALSE)


# export species names and codes:

names(dat_in)
species <- unique(dat_in[ , c("species", "spcode")])
species
nrow(species)
write.csv(species, "../outputs/tables/species_codes.csv", row.names = FALSE)


# IMPORT AND CROP UTM GRID MAP ####

utm <- vect("/vsicurl/https://github.com/AMBarbosa/Iberian-UTM-grid/raw/refs/heads/main/utm10_ib_crop_hires.fgb")
utm
plot(utm)

# add coordinate columns:
crs(utm, proj = TRUE)
coords <- terra::crds(terra::centroids(utm))
head(coords)
colnames(coords) <- c("lon", "lat")
terra::values(utm) <- data.frame(utm, coords)
head(utm)

utm_projected <- terra::project(utm, "epsg:25830")  # ETRS89 / UTM zone 30N
coords_projected <- terra::crds(terra::centroids(utm_projected))
head(coords_projected)
colnames(coords_projected) <- c("utm_x", "utm_y")
values(utm) <- data.frame(utm, coords_projected)
head(utm)

# identify UTM zone unions (for interpolation afterwards):
utm$zone_union <- as.integer((utm$LETRAX == "Q" & utm$UTM10X %in% 4:6 | utm$LETRAX == "T" & utm$UTM10X %in% 3:5) | (utm$LETRAX == "Y" & utm$UTM10X %in% 4:6 | utm$LETRAX == "B" & utm$UTM10X %in% 3:5))
plot(utm, "zone_union", border = NA)

# keep only relevant columns:
head(utm)
utm <- utm[ , c("CODIGO", "zone_union")]
names(utm)[1] <- "utm10"
head(utm)


# MERGE SPECIES AND UTM GRID ####

head(utm)
head(dat)
utm <- merge(utm, dat, by.x = "utm10", by.y = "utm10", all.x = TRUE)
head(utm)

plot(utm, "Trikir", lwd = 0.1)


# ADD INFO ON SURVEYED CELLS ACROSS THE MAP ####

utm$surveyed <- 0L
utm$surveyed[utm$utm10 %in% sampled_cells] <- 1L
nrow(utm)
sum(utm$surveyed)
sum(utm$surveyed == 0)

names(utm)
spp <- sort(unique(dat_in$spcode))

for (s in spp) {
  df <- values(utm)
  utm[ , s][is.na(df[ , s]) & df$surveyed == 1] <- 0
}

plot(utm, "surveyed", border = NA)
plot(utm, "Trikir", border = NA)


# export to disk:
head(utm)
names(utm)

ncol(utm)
utm <- utm[ , c("utm10", "zone_union", "surveyed", spp)]
ncol(utm)

head(utm)
terra::writeVector(utm, "../outputs/maps/utm.fgb", filetype = "FlatGeobuf", overwrite = TRUE)


# PLOT PRESENCE-ABSENCE MAPS ####

# par(mfrow = c(4, 2))
par(mfrow = modEvA::arrangePlots(length(spp)))
for(i in spp) {
  plot(utm, i, border = NA, col = c("salmon", "darkblue"), mar = c(1, 1, 1.5, 3), axes = FALSE, legend = ifelse(i == spp[length(spp)], TRUE, FALSE), main = i)
}
