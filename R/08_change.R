# SET UP ####

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(fuzzySim)  # fuzzyRangeChange
library(terra)  # mapping


# IMPORT PREDICTIONS AND BASEMAP ####

preds_pres <- read.csv("../outputs/tables/preds_pres_consensus.csv")
preds_fut <- read.csv("../outputs/tables/preds_fut_consensus.csv")

utm <- vect("../outputs/maps/utm.GPKG")


# COMPUTE FUZZY RANGE CHANGE ####

names(preds_pres)
names(preds_fut)

# get species codes for the loop:
fav_names <- names(preds_pres)
fav_names <- fav_names[grep("_consensus$", fav_names)]
fav_names
spcodes <- unique(sapply(strsplit(fav_names, "_"), getElement, 1))
spcodes

mfrow <- modEvA::arrangePlots(length(spcodes))

par(mfrow = mfrow, mar = c(4.5, 3.5, 2, 1))
for (spc in spcodes) {
  #   p <- preds_pres[ , grep(spc, names(preds_pres))]
  #   f <- preds_fut[ , grep(spc, names(preds_fut))]
  p <- preds_pres[ , paste0(spc, "_F_consensus")]
  f <- preds_fut[ , paste0(spc, "_F_consensus")]
  fuzzyRangeChange(pred1 = p, pred2 = f, main = spc, las = 2)
}


# MAP FAVOURABILITY CHANGE ####

change <- data.frame(utm_code = preds_pres[ , 1], preds_fut[ , -1] - preds_pres[ , -1])
head(change)
names(change)
change <- change[ , c(1, grep("_F_consensus", names(change)))]
names(change)
names(change) <- gsub("_F_consensus", "", names(change))
tail(change)

# export to csv:
write.csv(change, "../outputs/tables/preds_change.csv", row.names = FALSE)

# define limits for the colour scale:
range(change[ , -1], na.rm = TRUE)  # -0.3963291 0.4902107 - now -0.3133295  0.4266982
lim <- max(abs(change[ , -1]), na.rm = TRUE)
lim  # 0.4902107 - now 0.4266982

# map the change with a colourblind-friendly palette:
utm_change <- merge(utm[ , 1], change)
names(utm_change)
#pal <- hcl.colors(20, "Spectral")
#pal <- hcl.colors(20, "PRGn")
pal <- hcl.colors(20, "BrBG")
plot(utm_change, names(utm_change)[-1], type = "continuous", col = pal, range = c(-lim, lim), border = NA, mar = c(1, 1, 1, 4), axes = FALSE, nr = mfrow[1])
