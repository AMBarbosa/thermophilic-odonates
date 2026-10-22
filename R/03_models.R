# SET UP ####

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(terra)  # mapping
library(embarcadero)  # BART models


# IMPORT DATA ####

utm <- terra::vect("../outputs/maps/utm.GPKG")
plot(utm, lwd = 0.2)
head(utm) # contains species and variables from previous scripts

plot(utm, "Hum_relat_MM5_2021_2050", lwd = 0.2)

# assign NAs to variables where appropriate:
(var_cols <- grep("1971_2000|2021_2050", names(utm), value = TRUE))

# remove '_' within variable names (to use '_' as splitting character later):
names(utm) <- gsub("Hum_relat", "Humrelat", names(utm))

plot(utm, var_cols[1], lwd = 0.1)
head(utm)

# assign NAs to species where appropriate:
(species <- read.csv("../outputs/tables/species_codes.csv"))
(spcodes <- species$spcode)
plot(utm, spcodes[1], border = NA)
utm[ , spcodes][utm$surveyed == 0] <- NA
head(utm)
plot(utm, spcodes[1], border = NA)  # ok

# map some columns:
names(utm)
plot(utm, "Anaeph", border = NA)
plot(utm, "Trikir", border = NA)
plot(utm, "Tmax30_REM_1971_2000", border = NA, type = "continuous")

nrow(utm)  # 6207
sum(utm$surveyed)  # 5315
sapply(as.data.frame(utm[ , spcodes]), sum, na.rm = TRUE)
# Anaeph Croery Ortchr Triann Trikir Zygtor
#    655   2818   1285   1700    938     95

# separate variables into present and future:
dat <- terra::values(utm, na.rm = FALSE)
head(dat)
nrow(dat)  # 6207
nrow(utm)  # 6207
(vars_pres <- names(dat)[endsWith(names(dat), "1971_2000")])
(vars_fut <- names(dat)[endsWith(names(dat), "2021_2050")])


# SELECT VARIABLES? ####

# but how to select variables across climate models for each species?
# use all variables instead, and compute variable importance afterwards


# SEPARATE PRESENT AND FUTURE DATA ####

names(dat)
dat_pres <- dat[ , c("utm_code", spcodes, vars_pres)]
dat_fut <- dat[ , c("utm_code", vars_fut)]
nrow(dat_pres)  # 6207
nrow(dat_fut)  # 6207

# homogenize variable names for predict():
names(dat_pres) <- gsub("_1971_2000", "", names(dat_pres))
names(dat_fut) <- gsub("_2021_2050", "", names(dat_fut))
head(dat_pres)
head(dat_fut)

plot(merge(utm, dat_pres), "Humrelat_MM5", lwd = 0.1)  # ok

# export dat pres and fut to disk:
write.csv(dat_pres, "../outputs/tables/dat_pres.csv", row.names = FALSE)
write.csv(dat_fut, "../outputs/tables/dat_fut.csv", row.names = FALSE)


# COMPUTE MODELS ####

(vars <- unique(sapply(strsplit(vars_pres, "_"), getElement, 1)))
(climods <- unique(sapply(strsplit(vars_pres, "_"), getElement, 2)))

models <- vector("list", length(spcodes) * length(climods))
names(models) <- sort(c(outer(spcodes, climods, FUN = paste, sep = "_")))
names(models)
length(models)
models

system.time(for (m in names(models)) {
  gc()
  N <- grep(m, names(models))
  cat(N, m, "\n")

  spc <- sapply(strsplit(m, "_"), getElement, 1)
  climod <- sapply(strsplit(m, "_"), getElement, 2)

  mod_bart <- dbarts::bart(y.train = dat_pres[ , spc], x.train = dat_pres[ , paste(vars, climod, sep = "_")], keeptrees = TRUE, verbose = FALSE, seed = N)
  invisible(mod_bart$fit$state)

  models[[m]] <- mod_bart
})  # took ~5 minutes

length(models)
summary(models[[1]])
summary(models[[length(models)]])


# save model objects to disk:

dir.create("../outputs/models/", showWarnings = FALSE)
for (m in names(models)) {
  saveRDS(models[[m]], paste0("../outputs/models/", m, ".rds"))
}


# RETUNE MODELS ####

# aborted because takes very long and would also be needed for cross-validation?

# dir.create("../outputs/models_retuned/")
# models_retuned <- vector("list", length(models))
# names(models_retuned) <- names(models)
#
# for (m in names(models)) {
#   message(m)
#   mod <- retune(models[[m]])
#   invisible(mod$fit$state)
#   #models_retuned[[m]] <- mod
#   saveRDS(mod, paste0("../outputs/models_retuned/", m, ".rds"))
# }
