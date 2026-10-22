# SET UP ####

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(terra)  # mapping
library(fuzzySim)


# IMPORT SPECIES AND BASEMAP ####

species <- read.csv("../outputs/tables/species_codes.csv")
(spcodes <- sort(species$spcode))

(utm <- vect("../outputs/maps/utm.GPKG"))


# FIG 1: PRESENCE MAPS ####

names(utm)
utm[utm$surveyed == 0, spcodes] <- NA

# plot(utm, names(dat)[-1], col = c("lightgrey", "black"), border = NA, axes = FALSE, mar = c(1, 1, 1, 3), nr = 4, legend = FALSE)
# legend("bottomright", legend = c("presence", "absence", "no data"), fill = c("black", "lightgrey", NA), border = c(NA, NA, "black"), bty = "n")

# plot(utm, names(dat)[-1], col = c("salmon", "darkblue"), border = NA, axes = FALSE, mar = c(1, 1, 1, 3), nr = 4, legend = FALSE)
# legend("bottomright", legend = c("presence", "absence", "no data"), fill = c("darkblue", "salmon", NA), border = c(NA, NA, "black"), bty = "n")

# pdf("../outputs/images/Fig1_presences.pdf", width = 5.5, height = 7.5)
# par(mfrow = c(4, 2))
pdf("../outputs/images/Fig1_presences.pdf", width = 5, height = 6)
par(mfrow = modEvA::arrangePlots(length(spcodes)))
for(s in spcodes) {
  # last_spc <- s == spcodes[length(spcodes)]
  # if (last_spc) margins <- c(1, 1, 1.5, 3) else margins <- c(1, 1, 1.5, 1)
  margins <- c(1, 1, 1.5, 1)
  plot(utm, s, border = NA, col = c("grey", "black"), mar = margins, axes = FALSE, legend = FALSE, main = s)
  # if (last_spc) legend("bottomright", legend = c("presence", "absence", "no data"), fill = c("black", "grey", NA), border = c(NA, NA, "black"), bty = "n")
}
legend(x = -15, y = 41, legend = c("presence", "absence", "no data"), fill = c("black", "grey", NA), border = c(NA, NA, "black"), bty = "n", xpd = NA, y.intersp = 0.7)
dev.off()


# FIG 2: VARIABLE IMPORTANCE ####

varimps <- read.csv("../outputs/tables/variable_importance.csv")
varimps
varimps_min <- min(varimps[ , -1])
varimps_max <- max(varimps[ , -1])
varimps_min
varimps_max

mfrow <- modEvA::arrangePlots(length(spcodes))

pdf("../outputs/images/Fig2_variable_importance.pdf", width = 4, height = 7.5)
par(mar = c(6.4, 3, 2, 1), mfrow = mfrow)
for (spc in spcodes) {
  varimps_df_spc <- varimps[ , grep(spc, colnames(varimps))]
  varimps_df_spc <- t(varimps_df_spc)
  varnames <- varimps[ , "variable"]
  colnames(varimps_df_spc) <- sapply(strsplit(varnames, "_"), getElement, 1)
  boxplot(varimps_df_spc, beside = TRUE, las = 2, ylim = c(varimps_min, varimps_max), main = spc, cex.axis = 0.9)
}
dev.off()


# FIG 3: CONSENSUS FAVOURABILITY + CI (PRESENT) ####

preds_pres_cons <- read.csv("../outputs/tables/preds_pres_consensus_filled.csv")
preds_fut_cons <- read.csv("../outputs/tables/preds_fut_consensus_filled.csv")
head(preds_pres_cons)
head(preds_fut_cons)

names(utm)
utm_pres_cons <- merge(utm[ , 1], preds_pres_cons)
names(utm_pres_cons)

# plot(utm_pres_cons, grep("_F_consensus", names(utm_pres_cons)), type = "continuous", border = NA, mar = c(1, 1, 1, 4), nr = 8, axes = FALSE)

pdf("../outputs/images/Fig3_fav_present.pdf", width = 6, height = 8)
par(mfrow = c(length(spcodes), 3))
for (s in spcodes) {
  plot(utm_pres_cons, grep(paste0(s, "_F_consensus_filled"), names(utm_pres_cons)), type = "continuous", border = NA, range = c(0, 1), axes = FALSE, legend = TRUE, mar = c(1, 1, 1.5, 3.5), main = paste(s, "prediction"))
  plot(utm_pres_cons, grep(paste0(s, "_F_lower_consensus_filled"), names(utm_pres_cons)), type = "continuous", border = NA, range = c(0, 1), axes = FALSE, legend = TRUE, mar = c(1, 1, 1.5, 3.5), main = "Lower bound")
  plot(utm_pres_cons, grep(paste0(s, "_F_upper_consensus_filled"), names(utm_pres_cons)), type = "continuous", border = NA, range = c(0, 1), axes = FALSE, legend = TRUE, mar = c(1, 1, 1.5, 3.5), main = "Upper bound")
}
dev.off()


# FIG 4: CONSENSUS FAVOURABILITY + CI (FUTURE) ####

names(utm)
utm_fut_cons <- merge(utm, preds_fut_cons)
names(utm_fut_cons)

pdf("../outputs/images/Fig4_fav_future.pdf", width = 6, height = 8)
par(mfrow = c(length(spcodes), 3))
for (s in spcodes) {
  plot(utm_fut_cons, grep(paste0(s, "_F_consensus"), names(utm_fut_cons)), type = "continuous", border = NA, range = c(0, 1), axes = FALSE, legend = TRUE, mar = c(1, 1, 1.5, 3.5), main = paste(s, "prediction"))
  plot(utm_fut_cons, grep(paste0(s, "_F_lower_consensus"), names(utm_fut_cons)), type = "continuous", border = NA, range = c(0, 1), axes = FALSE, legend = TRUE, mar = c(1, 1, 1.5, 3.5), main = "Lower bound")
  plot(utm_fut_cons, grep(paste0(s, "_F_upper_consensus"), names(utm_fut_cons)), type = "continuous", border = NA, range = c(0, 1), axes = FALSE, legend = TRUE, mar = c(1, 1, 1.5, 3.5), main = "Upper bound")
}
dev.off()


# FIG 5: FAVOURABILITY CHANGE (MAPS + BARPLOTS) ####

preds_change <- read.csv("../outputs/tables/preds_change_filled.csv")
head(preds_change)

range(preds_change[ , -1], na.rm = TRUE)  # -0.3963291 0.4902107 - now -0.3986822 0.4788817
lim <- max(abs(preds_change[ , -1]), na.rm = TRUE)
lim  # 0.4902107 - now 0.4788817 - now 0.4266982

utm_change <- merge(utm[ , 1], preds_change)
names(utm_change)
#pal <- hcl.colors(20, "Spectral")
#pal <- hcl.colors(20, "PRGn")
pal <- hcl.colors(20, "BrBG")

(mfrow <- modEvA::arrangePlots(length(spcodes)))

# pdf("../outputs/images/Fig5a_fav_change_maps.pdf", width = 5.5, height = 7.5)
# plot(utm_change, spcodes, type = "continuous", col = pal, range = c(-lim, lim), border = NA, mar = c(1, 1, 1, 4), axes = FALSE, nr = mfrow[1])
# dev.off()
#
# # get the min and max for same y axis limits in all barplots:
# frc <- vector("list", length(spcodes))
# names(frc) <- spcodes
# for (spc in spcodes) {
#   # p <- preds_pres_cons[ , grep(spc, names(preds_pres_cons))]
#   # f <- preds_fut_cons[ , grep(spc, names(preds_fut_cons))]
#   p <- preds_pres_cons[ , paste0(spc, "_F_consensus_filled")]
#   f <- preds_fut_cons[ , paste0(spc, "_F_consensus_filled")]
#   frc[[spc]] <- fuzzyRangeChange(pred1 = p, pred2 = f, main = spc, las = 2, cex.names = 1.2, plot.type = "bar", border = NA)
# }
# frc
# ymin <- min(sapply(frc, getElement, 2))
# ymax <- max(sapply(frc, getElement, 2))
#
# # get the plots:
# pdf("../outputs/images/Fig5b_fav_change_barplots.pdf", width = 5.5, height = 7.5)
# par(mfrow = mfrow, mar = c(5, 6, 1, 6))  # mar = c(4.7, 3.5, 2, 1)
# for (spc in spcodes) {
#   p <- preds_pres_cons[ , paste0(spc, "_F_consensus_filled")]
#   f <- preds_fut_cons[ , paste0(spc, "_F_consensus_filled")]
#   frc[[spc]] <- fuzzyRangeChange(pred1 = p, pred2 = f, las = 2, cex.names = 1.2, ylim = c(ymin, ymax), main = "", x.lab = FALSE, plot.type = "bar", border = NA)
#   # add labels at 45 degrees:
#   text(x = (1:5) + seq(0.1, 0.5, by = 0.1), y = -0.08, labels = rownames(frc[[spc]]), xpd = TRUE, srt = 40, adj = 1, cex = 1.2, col = "white", font = 2)  # halo
#   text(x = (1:5) + seq(0, 0.4, by = 0.1), y = -0.08, labels = rownames(frc[[spc]]), xpd = TRUE, srt = 40, adj = 1, cex = 1.2)
# }
# dev.off()


pdf("../outputs/images/Fig5_fav_change.pdf", width = 6, height = 7.5)
par(mfrow = c(length(spcodes) / 2, 4), mar = c(4.7, 3, 3, 1))  # mar = c(4.7, 3.5, 2, 1)
for (spc in spcodes) {
  # plot(utm_change, spc, type = "continuous", col = pal, range = c(-lim, lim), border = NA, mar = c(0, 0.1, 0, 0), axes = FALSE, plg = list(ext = c(xmax(utm) - 2.5, xmax(utm) - 2.2, ymin(utm) + 0.5, ymax(utm) - 3), border = NA))  # vertical legend
  plot(utm_change, spc, type = "continuous", col = pal, range = c(-lim, lim), border = NA, mar = c(0, 0.1, 0, 0), axes = FALSE, plg = list(ext = ext(xmin(utm) + 2, xmax(utm) - 4, ymin(utm) - 1, ymin(utm) - 0.5), x = "bottom"))  # horizontal legend
  # mtext(species[species$spcode == spc, "species"], adj = 0, font = 4)
  mtext(spc, adj = 1, font = 2)
  p <- preds_pres_cons[ , paste0(spc, "_F_consensus_filled")]
  f <- preds_fut_cons[ , paste0(spc, "_F_consensus_filled")]
  fuzzyRangeChange(pred1 = p, pred2 = f, las = 2, cex.names = 0.8, ylim = c(ymin, ymax), main = "", plot.type = "bar", border = NA, cex.axis = 0.7, x.lab = FALSE)
  text(x = (1:5) + seq(0, 0.4, by = 0.1), y = -0.08, labels = rownames(frc[[spc]]), xpd = TRUE, srt = 40, adj = 1, cex = 1)
}
dev.off()


# save individual maps for Rocio manual figure 5:
for (m in names(utm_change)[-1]) {
  # pdf(paste0("../outputs/images/", m, "_change.pdf"), width = 5, height = 3.8)
  plot(utm_change, m, type = "continuous", col = pal, range = c(-lim, lim), border = NA, mar = c(1, 1, 1, 4), axes = FALSE)
  # dev.off()
}


# FIG S1: SPATIAL CROSS-VALIDATION BLOCKS ####

blocks_sv <- terra::vect("../outputs/maps/spatial_blocks.GPKG")
iberia <- terra::vect("../outputs/maps/iberia.gpkg")
portugal <- terra::aggregate(geodata::gadm("Portugal", level = 1, path = "../outputs/maps"))

pdf("../outputs/images/FigS1_blocks.pdf", width = 4, height = 3.5)
plot(blocks_sv, "folds", col = hcl.colors(10, "geyser"), border = "darkgrey", axes = FALSE, mar = c(0, 0, 0, 0), legend = FALSE, main = "")
lines(iberia)
lines(portugal)
text(centroids(blocks_sv), blocks_sv$folds, col = blocks_sv$folds, halo = TRUE, font = 2)
dev.off()


# FIG S2 and following ones ####

# made in section "RESPONSE CURVES" of script 06_variable_importance.R
