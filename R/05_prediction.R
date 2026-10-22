# SET UP ####

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(modEvA)  # MESS(), arrangePlots()
library(terra)  # mapping
library(embarcadero)  # BART models
library(fuzzySim)  #  favourability

source("https://raw.githubusercontent.com/AMBarbosa/unpackaged/master/predict_bart_df")


# IMPORT DATA AND MODELS ####

utm <- terra::vect("../outputs/maps/utm.GPKG")
names(utm)[1:20]
plot(utm, "Tmax30_REM_1971_2000", lwd = 0.1)
nrow(utm)  # 6207

dat_pres <- read.csv("../outputs/tables/dat_pres.csv")
dat_fut <- read.csv("../outputs/tables/dat_fut.csv")
names(dat_pres)
names(dat_fut)
nrow(dat_pres)  # 6207
nrow(dat_fut)  # 6207

plot(merge(utm, dat_pres, by = "utm_code"), "Humrelat_MM5", lwd = 0.1)  # OK if 'by' in merge()

(mod_files <- list.files("../outputs/models", full.names = TRUE))
(mod_names <- basename(tools::file_path_sans_ext(mod_files)))
models <- lapply(mod_files, readRDS)
names(models) <- mod_names

(spcodes <- unique(sapply(strsplit(mod_names, "_"), getElement, 1)))
(climods <- unique(sapply(strsplit(mod_names, "_"), getElement, 2)))

iberia <- terra::vect("../outputs/maps/iberia.gpkg")

gc()


# COMPUTE MESS ####

names(dat_pres)[1:20]
names(dat_fut)[1:20]

messes <- data.frame(utm_code = na.omit(dat_pres[ , !(names(dat_pres) %in% spcodes)])$utm_code)

for (m in climods) {
  cat(m, "\n")
  dat_pres_m <- dat_pres[ , grep(m, names(dat_pres))]
  dat_fut_m <- dat_fut[ , c(1, grep(m, names(dat_fut)))]
  messes[ , m] <- MESS(na.omit(dat_pres_m), na.omit(dat_fut_m), id.col = 1)$TOTAL
}

tail(messes)

# messes[ , -1] [messes[ , -1] > 0] <- NA

names(utm)
utm_mess <- merge(utm[ , "utm_code"], messes, by = "utm_code")
names(utm_mess)
nrow(utm_mess)  # 5664

(mess_min <- min(values(utm_mess)[ , -1], na.rm = TRUE))
# mess_min_which <- names(which.min(apply(messes[, -grep("utm_code", names(messes))], 2, min)))

mfrow <- modEvA::arrangePlots(length(climods))

pdf("../outputs/images/FigS3_MESS.pdf", width = 4.5, height = 6)
# par(mfrow = mfrow)
layout(matrix(c(1, 2, 3, 4, 5, 5), 3, 2, byrow = TRUE))
for (m in climods) {
    plot(utm_mess, m,
         col = heat.colors(100), border = NA,
         type = "continuous", range = c(mess_min, 0),
         main = m, axes = FALSE, mar = c(0, 1, 2, 4),
         legend = ifelse(m == names(utm_mess)[length(names(utm_mess))], TRUE, FALSE), plg = list(ext = c(5, 6, 37, 42), border = NA))  # legend only in last plot
    lines(iberia, lwd = 0.5)
}
dev.off()


# GET MODEL PREDICTIONS ####

mod_names

# make tables to receive the predictions:
tail(dat_pres)
preds_pres <- dat_pres[ , c("utm_code", spcodes)]
preds_fut <- dat_fut[ , "utm_code", drop = FALSE]
head(preds_pres)
head(preds_fut)

names(utm)
par(mfrow = c(1, 1))
plot(utm, "Anaeph", border = NA)
plot(utm, "Tmax30_REM_1971_2000", border = NA)

system.time(for (spc in spcodes) for (climod in climods) {
  cat(spc, climod, "\n")
  mod <- models[[paste(spc, climod, sep = "_")]]

  cat("- present\n")
  preds <- predict_bart_df(mod, dat_pres, quantiles = c(0.05, 0.95))
  P_name <- paste(spc, climod, "P", sep = "_")
  preds_pres[ , P_name] <- preds[ , 1]  # mean posterior

  F_name <- paste(spc, climod, "F", sep = "_")
  preds_pres[ , F_name] <- fuzzySim::Fav(pred = preds[ , 1], sample.preval = prevalence(model = mod))  # mean posterior
  preds_pres[ , paste0(F_name, "_lower")] <- fuzzySim::Fav(pred = preds[ , 2], sample.preval = prevalence(model = mod))  # lower bound
  preds_pres[ , paste0(F_name, "_upper")] <- fuzzySim::Fav(pred = preds[ , 3], sample.preval = prevalence(model = mod))  # upper bound
  #preds_pres[ , paste0(F_name, "_uncert")] <- preds_pres[ , paste0(F_name, "_upper")] - preds_pres[ , paste0(F_name, "_lower")]

  cat("- future\n")
  preds <- predict_bart_df(mod, dat_fut, quantiles = c(0.05, 0.95))
  P_name <- paste(spc, climod, "P", sep = "_")
  preds_fut[ , P_name] <- preds[ , 1]

  F_name <- paste(spc, climod, "F", sep = "_")
  preds_fut[ , F_name] <- fuzzySim::Fav(pred = preds[ , 1], sample.preval = prevalence(model = mod))
  preds_fut[ , paste0(F_name, "_lower")] <- fuzzySim::Fav(pred = preds[ , 2], sample.preval = prevalence(model = mod))
  preds_fut[ , paste0(F_name, "_upper")] <- fuzzySim::Fav(pred = preds[ , 3], sample.preval = prevalence(model = mod))
})  # took ~3 mins

tail(preds_pres)
tail(preds_fut)


# save tables with predictions to disk:

write.csv(preds_pres, "../outputs/tables/preds_pres.csv", row.names = FALSE)
write.csv(preds_fut, "../outputs/tables/preds_fut.csv", row.names = FALSE)

# preds_pres <- read.csv("../outputs/tables/preds_pres.csv")
# preds_fut <- read.csv("../outputs/tables/preds_fut.csv")


# MAP PREDICTIONS ####

names(utm)

# intersect(names(utm), names(preds_pres))

utm <- merge(utm, preds_pres[ , !(names(preds_pres) %in% spcodes)], by = "utm_code")

spc <- "Anaeph"
climod <- "REM"

par(mfrow = c(1, 1))
plot(utm, paste0(spc, "_", climod, "_F"), type = "continuous", border = NA, mar = c(1, 1, 1, 3.5), axes = FALSE, range = c(0, 1))

# for (spc in spcodes) {
#   plot(utm, paste0(spc, "_F"), type = "continuous", border = NA, main = spc, mar = c(1, 1, 1, 3), axes = FALSE, range = c(0, 1))
# }

# for (fav in fav_cols) {
#   spc <- sapply(strsplit(fav, "_"), `[`, 1)
#   presences <- subset(values(utm), values(utm)[ , spc] == 1, select = c("lon", "lat"))
#   plot(utm, spc, col = c("lightgrey", "black"), border = NA, mar = c(1, 1, 1, 3), axes = FALSE)
#   plot(utm, fav, border = NA, mar = c(1, 1, 1, 3), type = "continuous", range = c(0, 1), axes = FALSE)
#   points(presences, pch = 20, cex = 0.1, col = "red")
# }

png("../outputs/images/preds/pred%02d.png")
par(mfrow = c(2, 2))
for (spc in spcodes) for (climod in climods) {
  message(spc, climod)
  plot(utm, spc, col = c("lightgrey", "black"), border = NA, mar = c(1, 1, 1, 3), axes = FALSE, main = spc)
  plot(utm, paste0(spc, "_", climod, "_F"), type = "continuous", border = NA, mar = c(1, 1, 1, 3.5), axes = FALSE, range = c(0, 1), main = paste(climod, "prediction"))
  plot(utm, paste0(spc, "_", climod, "_F_lower"), type = "continuous", border = NA, mar = c(1, 1, 1, 3.5), axes = FALSE, range = c(0, 1), main = paste(climod, "lower bound"))
  plot(utm, paste0(spc, "_", climod, "_F_upper"), type = "continuous", border = NA, mar = c(1, 1, 1, 3.5), axes = FALSE, range = c(0, 1), main = paste(climod, "upper bound"))
}
dev.off()
