# SET UP ####

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(terra)  # mapping
library(fuzzySim)  # prediction consensus
library(modEvA)  # arrangePlots


# IMPORT DATA ####

preds_pres <- read.csv("../outputs/tables/preds_pres.csv")
preds_fut <- read.csv("../outputs/tables/preds_fut.csv")

utm <- vect("../outputs/maps/utm.GPKG")


# COMPUTE PREDICTION CONSENSUS ####

names(preds_pres)

(pred_names <- names(preds_pres))
# fav_names <- pred_names[grep("_F", pred_names)]
(fav_names <- pred_names[grep("_F$", pred_names)])
(lower_names <- pred_names[grep("_F_lower", pred_names)])
(upper_names <- pred_names[grep("_F_upper", pred_names)])
fav_names
lower_names
upper_names

(spcodes <- unique(sapply(strsplit(fav_names, "_"), getElement, 1)))
(climods <- unique(sapply(strsplit(fav_names, "_"), getElement, 2)))

par(mfrow = c(1, 1))

# present:
fav_pres <- preds_pres[ , c("utm_code", fav_names, lower_names, upper_names)]
names(fav_pres)
for (spc in spcodes) {
  fav_spc <- fav_pres[ , grep(spc, c(fav_names, lower_names, upper_names))]
  cons_fav <- fuzzyConsensus(fav_spc[ , grep("_F$", names(fav_spc))])
  cons_lower <- fuzzyConsensus(fav_spc[ , grep("_F_lower", names(fav_spc))])
  cons_upper <- fuzzyConsensus(fav_spc[ , grep("_F_upper", names(fav_spc))])
  fav_pres[ , paste0(spc, "_F_consensus")] <- cons_fav
  fav_pres[ , paste0(spc, "_F_lower_consensus")] <- cons_lower
  fav_pres[ , paste0(spc, "_F_upper_consensus")] <- cons_upper
}
tail(fav_pres)

# future:
fav_fut <- preds_fut[ , c("utm_code", fav_names, lower_names, upper_names)]
names(fav_fut)
for (spc in spcodes) {
  fav_spc <- fav_fut[ , grep(spc, c(fav_names, lower_names, upper_names))]
  cons_fav <- fuzzyConsensus(fav_spc[ , grep("_F$", names(fav_spc))])
  cons_lower <- fuzzyConsensus(fav_spc[ , grep("_F_lower", names(fav_spc))])
  cons_upper <- fuzzyConsensus(fav_spc[ , grep("_F_upper", names(fav_spc))])
  fav_fut[ , paste0(spc, "_F_consensus")] <- cons_fav
  fav_fut[ , paste0(spc, "_F_lower_consensus")] <- cons_lower
  fav_fut[ , paste0(spc, "_F_upper_consensus")] <- cons_upper
}
tail(fav_fut)

gc()


# SELECT CONSENSUS COLUMNS AND EXPORT TO CSV ###

names(fav_pres)
names(fav_fut)
fav_pres <- fav_pres[ , c(1, grep("_consensus", names(fav_pres)))]
fav_fut <- fav_fut[ , c(1, grep("_consensus", names(fav_fut)))]
names(fav_pres)
names(fav_fut)

write.csv(fav_pres, "../outputs/tables/preds_pres_consensus.csv", row.names = FALSE)
write.csv(fav_fut, "../outputs/tables/preds_fut_consensus.csv", row.names = FALSE)


# COMPUTE BAQUERO INDEX ####

# compare the predicted (posterior mean) favourability with the median between the lower and upper CI bounds

names(fav_pres)

# add median (between lower and upper fav bound) and difference (between predicted and median fav) to consensus tables:

# present:
for (spc in spcodes) {
  fav_name <- paste0(spc, "_F_consensus")
  lower_name <- paste0(spc, "_F_lower_consensus")
  upper_name <- paste0(spc, "_F_upper_consensus")
  fav_median <- apply(fav_pres[ , c(lower_name, upper_name)], 1, median)
  fav_pres[ , paste0(spc, "_F_median")] <- fav_median
  fav_diff <- fav_pres[ , fav_name] - fav_pres[ , c(paste0(spc, "_F_median"))]
  fav_pres[ , paste0(spc, "_F_diff")] <- fav_diff
}
tail(fav_pres)
names(fav_pres)

# future:
for (spc in spcodes) {
  fav_name <- paste0(spc, "_F_consensus")
  lower_name <- paste0(spc, "_F_lower_consensus")
  upper_name <- paste0(spc, "_F_upper_consensus")
  fav_median <- apply(fav_fut[ , c(lower_name, upper_name)], 1, median)
  fav_fut[ , paste0(spc, "_F_median")] <- fav_median
  fav_diff <- fav_fut[ , fav_name] - fav_fut[ , c(paste0(spc, "_F_median"))]
  fav_fut[ , paste0(spc, "_F_diff")] <- fav_diff
}
tail(fav_fut)
names(fav_fut)


# save updated tables to csv:
write.csv(fav_pres, "../outputs/tables/preds_pres_consensus.csv", row.names = FALSE)
write.csv(fav_fut, "../outputs/tables/preds_fut_consensus.csv", row.names = FALSE)


# map and plot difference between predicted (posterior) and median fav:

names(utm)

utm_pres <- merge(utm, fav_pres)
utm_fut <- merge(utm, fav_fut)
names(utm_pres)
names(utm_fut)

utm_pres_dif <- utm_pres[ , grep("_F_diff", names(utm_pres))]
utm_fut_dif <- utm_fut[ , grep("_F_diff", names(utm_fut))]

lim <- max(abs(data.frame(utm_pres_dif, utm_fut_dif)), na.rm = TRUE)
pal <- hcl.colors(20, "BrBG")

#png("../outputs/images/UpperBoundProximity_present.png", width = 480, height = 600)
par(oma = c(0, 0, 2, 0))
mfrow <- modEvA::arrangePlots(length(spcodes))
plot(utm_pres_dif, names(utm_pres_dif), type = "continuous", col = pal, range = c(-lim, lim), border = NA, mar = c(1, 1, 2, 4), axes = FALSE, nr = mfrow[1], nc = mfrow[2])
title("Upper bound proximity - Present", outer = TRUE)
#dev.off()

#png("../outputs/images/UpperBoundProximity_future.png", width = 480, height = 600)
par(oma = c(0, 0, 2, 0))
plot(utm_fut_dif, names(utm_fut_dif), type = "continuous", col = pal, range = c(-lim, lim), border = NA, mar = c(1, 1, 2, 4), axes = FALSE, nr = mfrow[1], nc = mfrow[2])
title("Upper bound proximity - Future", outer = TRUE)
#dev.off()


par(mar = c(4, 4, 2, 3), oma = c(0, 0, 1, 0), mfrow = mfrow)
for (spc in spcodes) {
  pre <- fav_pres[ , paste0(spc, "_F_consensus")]
  med <- fav_pres[ , paste0(spc, "_F_median")]
  dif <- fav_pres[ , paste0(spc, "_F_diff")]
  # layout(matrix(c(1, 1, 1, 2), nrow = 2, ncol = 2, byrow = TRUE))
  # plot(utm_pres_dif, paste0(spc, "_F_diff"), border = NA)
  # par(mar = c(7, 3, 2, 3))
  plot(med, pre, pch = 20, cex = 0.1, xlim = c(0, 1), ylim = c(0, 1), xlab = "median", ylab = "predicted", col = "darkgrey", main = spc)
  abline(0, 1, col = "blue")
  mean_diff <- mean(dif, na.rm = TRUE)
  text(0, 0.7, paste("mean diff =\n", round(mean_diff, 5)), adj = 0)
}
title("Present", outer = TRUE)

par(mfrow = mfrow, mar = c(5, 3, 2, 1), oma = c(0, 0, 1, 0))
for (spc in spcodes) {
  pre <- fav_pres[ , paste0(spc, "_F_consensus")]
  med <- fav_pres[ , paste0(spc, "_F_median")]
  fuzzyRangeChange(med, pre, las = 2)
}
title("Present", outer = TRUE)

par(mfrow = mfrow, mar = c(5, 3, 2, 1), oma = c(0, 0, 1, 0))
for (spc in spcodes) {
  dif <- fav_pres[ , paste0(spc, "_F_diff")]
  hist(na.omit(dif))
}
title("Present", outer = TRUE)


# MAP PREDICTION CONSENSUS ####

nrow(preds_pres)  # 6207
nrow(utm)  # 6207

names(fav_pres)
names(fav_fut)

utm_pres <- merge(utm, fav_pres)
utm_fut <- merge(utm, fav_fut)

plot(utm_pres, grep("_F_consensus", names(utm_pres)), type = "continuous", border = NA, mar = c(1, 1, 1, 3.5), axes = FALSE, range = c(0, 1), nr = mfrow[1])

#plot(utm_fut, grep("F_lower_consensus", names(utm_fut)), type = "continuous", border = NA, mar = c(1, 1, 1, 3), axes = FALSE, range = c(0, 1), nr = 4)
