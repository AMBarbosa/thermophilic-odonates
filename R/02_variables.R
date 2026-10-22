# SET UP ####

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(terra)  # mapping

species <- read.csv("../outputs/tables/species_codes.csv")


# IMPORT VARIABLES ####

# the below .zip file needs to be downloaded into 'inputs' first
# from https://datadryad.org/dataset/doi:10.5061/dryad.rxwdbrv8x
# (Baquero et al. 2021)
zip_file <- list.files("../inputs", pattern = "\\.zip$", full.names = TRUE)
unzip(zip_file, list = TRUE)  # shows contents
unzip(zip_file, files = "variables_Baquero_et_al.csv", exdir = "../inputs")
vars <- read.csv("../inputs/variables_Baquero_et_al.csv")
head(vars)
unlink("../inputs/variables_Baquero_et_al.csv")


# remove special characters in column names:

names(vars)
names(vars) <- gsub(pattern = "\\.", replacement = "_", x = names(vars))
names(vars)


# MERGE VARIABLES WITH UTM GRID ####

utm <- vect("../outputs/maps/utm.GPKG")
par(mfrow = c(1, 1))
plot(utm, lwd = 0.2)
head(utm)  # contains species, merged with previous script
head(vars[1:4])

nrow(utm)
nrow(vars)

utm$utm_code <- substr(utm$utm10, 4, 7)

utm <- merge(utm, vars, by.x = "utm_code", by.y = "UTM_10X10", all.x = TRUE)

setdiff(vars$UTM_10X10, utm$utm_code)  # "XN95", checked and looks like it could be in France
sort(utm$utm_code[grep("XN", utm$utm_code)])  # has up to "XN94"
# it's only one cell and marginal, so I'll leave it

utm
head(utm)
nrow(utm)

# map some variables:
names(utm)
plot(utm, "Tmax30_REM_1971_2000", border = NA, type = "continuous")
plot(utm, "PrAvemax5d_REM_2021_2050", border = NA, type = "continuous")

# create column specifying where variables missing:
utm$has_vars <- NA_integer_
utm$has_vars[!is.finite(utm$Tmax30_REM_1971_2000)] <- 0
utm$has_vars[is.finite(utm$Tmax30_REM_1971_2000)] <- 1
plot(utm, "has_vars", border = NA)
plot(utm, "zone_union", border = NA)
plot(utm, "surveyed", border = NA)

# reorder and rename columns:
names(utm)
(var_names <- names(utm)[grep("1971_2000|2021_2050", names(utm))])
ncol(utm)

utm <- utm[ , c("utm_code", "zone_union", "surveyed", "has_vars", species$spcode, var_names)]
ncol(utm)
names(utm)

# export updated grid to disk:
writeVector(utm, "../outputs/maps/utm.GPKG", overwrite = TRUE)
