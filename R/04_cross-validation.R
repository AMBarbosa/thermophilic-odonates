# SET UP ####

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(terra)
# library(gstat)  # range of spatial autocorrelation (SAC) in presences
# library(gridExtra)  # arrange SAC plots
library(blockCV)
library(geodata)
library(embarcadero)
library(modEvA)
library(reshape2)  # wide to long format for grouped boxplots
library(ggplot2)  # grouped boxplots

source("https://raw.githubusercontent.com/AMBarbosa/unpackaged/master/predict_bart_df")

species <- read.csv("../outputs/tables/species_codes.csv")
(spcodes <- species$spcode)

utm <- terra::vect("../outputs/maps/utm.GPKG")


# SAC ####

names(utm)

utm_prj <- terra::project(utm, "EPSG:25830")  # to get SAC range in metres
utm_sp <- as(utm_prj, "Spatial")  # for gstat::variogram()
utm_sp <- utm_sp[utm_sp$surveyed == 1, ]  # for gstat::variogram()

sac_range <- vector("numeric", nrow(species))
sac_plots <- vector("list", nrow(species))
names(sac_range) <- names(sac_plots) <- species$spcode

for (s in species$spcode) {
  variogram_model <- gstat::variogram(get(s) ~ 1, data = utm_sp)
  fitted_model <- gstat::fit.variogram(variogram_model, model = gstat::vgm(model = c("Exp", "Sph", "Gau", "Mat")))
  sac_plots[[s]] <- plot(variogram_model, fitted_model, main = s)
  # plot(utm_prj, s, border = NA, main = s)
  sac_range[s] <- fitted_model$range[length(fitted_model$range)]
}

sac_range
#   Anaeph    Croery    Ortchr    Triann    Trikir    Zygtor
# 6418.214 14051.346 16689.568 12031.728 12380.541 29843.466

summary(sac_range)

plot(utm, species$spcode, border = NA, col = c("lightgrey", "darkblue"), legend = FALSE, axes = FALSE, mar = c(0,0,0,0))


# plot variograms:

plot(sac_plots[[1]])

par(mfrow = modEvA::arrangePlots(length(spcodes)), mar = c(2, 2, 1, 0))  # ignored by plot variogram (lattice), hence grid.arrange() below

plot(sac_plots[[1]])

gridExtra::grid.arrange(sac_plots[[1]], sac_plots[[2]], sac_plots[[3]], sac_plots[[4]], sac_plots[[5]], sac_plots[[6]], nrow = 3)


# GET CROSS-VALIDATION BLOCKS ####

# area <- terra::expanse(terra::aggregate(utm))
# block_size <- sqrt(area / 20)
block_size <- 100000

set.seed(2)
# blocks <- spatialBlock(speciesData = as(centroids(utm), "Spatial"), theRange = 100000, k = 10, selection = "systematic")
blocks <- blockCV::cv_spatial(terra::centroids(utm), size = block_size, k = 5, selection = "systematic")  # latest blockCV version


blocks_sv <- terra::vect(blocks$blocks)

writeVector(blocks_sv, "../outputs/maps/spatial_blocks.GPKG", overwrite = TRUE)

blocks_sv <- terra::vect("../outputs/maps/spatial_blocks.GPKG")

iberia <- terra::vect("../outputs/maps/iberia.gpkg")
portugal <- terra::aggregate(geodata::gadm("Portugal", level = 1, path = "../outputs/maps"))

names(blocks_sv)

par(mfrow = c(1, 1))
plot(blocks_sv, "folds", col = hcl.colors(10, "geyser"), border = "darkgrey", axes = FALSE, mar = c(0, 0, 0, 0), legend = FALSE, main = "")
lines(iberia)
lines(portugal)
text(centroids(blocks_sv), blocks_sv$folds, col = blocks_sv$folds, halo = TRUE, font = 2)

plot(utm, species$spcode[1], border = NA, col = c(NA, "grey30"), add = TRUE, legend = FALSE)

# add folds to data table:
# utm$cvfold <- blocks$foldID
utm$cvfold <- blocks$folds_ids  # phase 2,  blockCV version
head(utm)

plot(utm, "cvfold", border = NA)
plot(utm[utm$Anaeph == 1, ], col = "grey20", border = NA, add = TRUE)
plot(utm[utm$Trikir == 1, ], col = "brown", border = NA, alpha = 0.5, add = TRUE)

table(utm$cvfold)
#     546 655 665 661 571 544 606 698 614 648
# now 534 725 640 675 697 599 648 632 489 568
# now 1133 1373 1272 1164 1265 (k = 5)
# now 1246  943 1250 1447 1321

# see how many cells and presences in each fold:

names(utm)

for (spc in spcodes) {
  vals <- values(utm)
  pres <- vals[vals[ , spc] == 1, ]
  cat("\n", spc, "- ")
  cat(table(pres[ , "cvfold"]))
}
# all have at least some presences in each fold


# COMPUTE MODELS IN EACH FOLD ####

names(utm)
var_cols <- grep("1971_2000", names(utm))  # present
names(utm)[var_cols]

names(utm) <- gsub("Hum_relat", "Humrelat", names(utm))
climods <- unique(sapply(strsplit(names(utm)[var_cols], "_"), getElement, 2))
climods

(folds <- sort(unique(utm$cvfold)))

spblock_preds <- values(utm)[ , c("utm_code", spcodes, "cvfold")]
head(spblock_preds)

system.time(for (s in spcodes) {
  cat(s, "\n")

  for (m in climods) {
    cat("-",  m, "\n")

    for (f in folds) {
      cat("   fold", f, "\n")  # inform of progress
      dat_train <- subset(values(utm), cvfold != f)

      mod_fold <- bart(y.train = dat_train[ , s], x.train = dat_train[ , var_cols], keeptrees = TRUE, verbose = FALSE)
      spblock_preds[ , paste(s, m, "fold", f, sep = "_")] <- predict_bart_df(mod_fold, values(utm))
      gc()  # clean up memory before next loop iteration
    }  # end for f
  }  # end for m
})  # end for s
# took ~120 mins with 10 folds (and 8 species), ~30 mins with 5 (and 6 species)

names(spblock_preds)
head(spblock_preds)
tail(spblock_preds)
spblock_preds[ , 13:23]

write.csv(spblock_preds, "../outputs/tables/spblock_preds.csv", row.names = FALSE)

spblock_preds <- read.csv("../outputs/tables/spblock_preds.csv")


# EVALUATE EACH MODEL ON ITS VALIDATION FOLD ####

names(spblock_preds)

fold_cols <- grep("_fold", names(spblock_preds))
names(spblock_preds)[fold_cols]

metrics <- c("AUC", "sTSS", "MCS")

# create an empty table to receive the cross-validation results:
crossval <- as.data.frame(matrix(nrow = length(folds), ncol = length(spcodes) * length(metrics) * length(climods)))
crossval_names <- c(outer(spcodes, climods, FUN = paste, sep = "_"))
crossval_names <- c(outer(crossval_names, metrics, FUN = paste, sep = "_"))
crossval_names
names(crossval) <- crossval_names
crossval  # for now it's only filled with NAs

names(spblock_preds)
names(crossval)

for (s in spcodes)  for (m in climods)  for (f in folds) {
  cat(s, m, f, "\n")
  fold_name <- paste0("fold_", f)
  fold_col <- names(spblock_preds)[grep(paste0(s, "_", m, "_", "fold_", f, "$"), names(spblock_preds))]
  fold_dat <- subset(spblock_preds, cvfold == f)
  crossval[f, paste(s, m, "AUC", sep = "_")] <- AUC(obs = fold_dat[ , s], pred = fold_dat[ , fold_col], simplif = TRUE, plot = FALSE)
  crossval[f, paste(s, m, "sTSS", sep = "_")] <- threshMeasures(obs = fold_dat[ , s], pred = fold_dat[ , fold_col], thresh = "maxTSS", measures = "TSS", simplif = TRUE, standardize = TRUE, plot = FALSE)
  crossval[f, paste(s, m, "MCS", sep = "_")] <- MillerCalib(obs = fold_dat[ , s], pred = fold_dat[ , fold_col], plot = FALSE)$slope
}

crossval

write.csv(crossval, "../outputs/tables/crossval.csv", row.names = FALSE)


crossval <- read.csv("../outputs/tables/crossval.csv")

names(crossval)
head(crossval)


crossval_t <- as.data.frame(t(crossval))
names(crossval_t) <- paste0("fold", 1:length(folds))
names(crossval_t)
rownames(crossval_t)

splits <- strsplit(rownames(crossval_t), "_")
crossval_t$species <- sapply(splits, getElement, 1)
crossval_t$climod <- sapply(splits, getElement, 2)
crossval_t$metric <- sapply(splits, getElement, 3)
head(crossval_t)


# convert table to long format for the grouped boxplot:

head(crossval_t)
crossval_long <- reshape2::melt(crossval_t, id.vars = grep(c("species|climod|metric"), names(crossval_t)), measure.vars = grep("fold", names(crossval_t)), variable.name = "fold")
head(crossval_long)

crossval_long <- crossval_long[order(crossval_long$metric), ]

# make the boxplots:

# cols <- rainbow(3, s = 0.5)
# boxplot(value ~ fold + climod, data = crossval_long,
#         col = cols,
#         names = c("", "A", "", "", "B", ""), xaxs = FALSE)
# legend("topleft", fill = cols, legend = c(1,2,3), horiz = T)

# pdf("../outputs/images/FigS2_modev.pdf", width = 6, height = 8)  # pdf plot loses grid / colours

mfrow <- modEvA::arrangePlots(length(spcodes))

# png("../outputs/images/FigS2_modev.png", width = 500, height = 700)
pdf("../outputs/images/FigS2_modev.pdf")
ggplot(crossval_long,
       aes(value, metric, fill = climod)) +
  theme_bw() +  # https://www.datanovia.com/en/blog/ggplot-themes-gallery/
  geom_boxplot(orientation = "y") +
  # facet_wrap(~ species, nrow = mfrow[1], ncol = mfrow[2])
  facet_wrap(~ species, nrow = mfrow[1], ncol = mfrow[2]) + # phase 2
  geom_vline(xintercept = 1, col = "blue", lty = 2, lwd = 0.5) +
  geom_vline(xintercept = 0.7, col = "red", lty = 3) +
  geom_vline(xintercept = c(0.5, 1.5), col = "orange", lty = 3)
dev.off()
