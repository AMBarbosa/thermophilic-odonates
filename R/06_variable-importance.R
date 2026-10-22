# SET UP ####

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(embarcadero)
library(ggplot2)
library(modEvA)


# IMPORT MODELS ####

(mod_files <- list.files("../outputs/models", full.names = TRUE))
(mod_names <- basename(tools::file_path_sans_ext(mod_files)))

models <- lapply(mod_files, readRDS)
names(models) <- mod_names

species <- read.csv("../outputs/tables/species_codes.csv")
(spcodes <- sort(species$spcode))


# VARIABLE IMPORTANCE ACROSS CLIMATE MODELS ####

(varimps <- lapply(models, embarcadero::varimp, plots = FALSE))  # each ordered by varimp

(varimps <- lapply(varimps, function(x) x[order(as.character(x$names)), ]))  # all ordered by variable name

varimps_df <- data.frame(variable = as.character(varimps[[1]]$names), sapply(varimps, getElement, "varimps"))
varimps_df

write.csv(varimps_df, "../outputs/tables/variable_importance.csv", row.names = FALSE)

# min and max for axis limits:
names(varimps_df)
varimps_min <- min(varimps_df[ , -grep("variable", names(varimps_df))])
varimps_max <- max(varimps_df[ , -grep("variable", names(varimps_df))])
varimps_min
varimps_max

# png("../outputs/images/variable_importance_barplot.png", width = 480, height = 700)
# par(mar = c(7, 3, 2, 1), mfrow = c(4, 2))
# for (spc in spcodes) {
#   mods_spc <- models[grep(spc, mod_names)]
#   varimps_list_spc <- lapply(mods_spc, varimp, plots = FALSE)
#   varimps_matx_spc <- t(cbind(sapply(varimps_list_spc, function(x) x[ , "varimps"])))
#   varnames <- rownames(varimps_list_spc[[1]])
#   colnames(varimps_matx_spc) <- sapply(strsplit(varnames, "_"), getElement, 1)
#   barplot(varimps_matx_spc, beside = TRUE, las = 2, ylim = c(0, varimps_max), main = spc)
#   abline(h = varimps_min, lty = 2)
#   abline(h = varimps_max, lty = 2)
# }
# dev.off()

mfrow <- modEvA::arrangePlots(length(spcodes))

# pdf("../outputs/images/Fig2_variable_importance.pdf", width = 4, height = 6)
par(mar = c(6.4, 3, 2, 1), mfrow = mfrow)
for (spc in spcodes) {
  mods_spc <- models[grep(spc, mod_names)]
  varimps_list_spc <- lapply(mods_spc, varimp, plots = FALSE)
  varimps_list_spc <- lapply(varimps_list_spc, function(x) x[order(as.character(x[, "names"])),])
  varimps_matx_spc <- t(cbind(sapply(varimps_list_spc, function(x) x[ , "varimps"])))
  # varnames <- rownames(varimps_list_spc[[1]])
  varnames <- as.character(varimps_list_spc[[1]][ , "names"])
  colnames(varimps_matx_spc) <- sapply(strsplit(varnames, "_"), getElement, 1)
  boxplot(varimps_matx_spc, beside = TRUE, las = 2, ylim = c(varimps_min, varimps_max), main = spc, cex.axis = 0.9)  # , xaxt = "n"
  #text(x = 1:ncol(varimps_matx_spc), y = par("usr")[3] - 0.25, labels = colnames(varimps_matx_spc), adj = 1, xpd = TRUE, srt = 45)
  #grid(10, 10)
}
# dev.off()


# try a ggplot grouped boxplot:

varimps

varimps_df2 <- do.call(rbind.data.frame, varimps)
str(varimps_df2)
varimps_df2$names <- as.character(varimps_df2$names)
head(varimps_df2)

varimps_long <- data.frame(
  species = sapply(strsplit(rownames(varimps_df2), "_"), getElement, 1),
  variable = sapply(strsplit(varimps_df2$names, "_"), getElement, 1),
  model = sapply(strsplit(varimps_df2$names, "_"), getElement, 2),
  importance = varimps_df2$varimps,
  row.names = NULL)

head(varimps_long)

# pdf("../outputs/images/variable_importance_byclimod.pdf", width = 6, height = 7.5)
ggplot(varimps_long, aes(x = variable, y = importance, color = model, fill = model)) +
  geom_boxplot() +
  facet_wrap(~species, nrow = mfrow[1], ncol = mfrow[2], scales = "free") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 6))
# dev.off()


# RESPONSE CURVES ####

names(models)
head(models[[1]]$varcount)
colnames(models[[1]]$varcount)

# system.time(partial1 <- partial(models[[1]], x.vars = colnames(models[[1]]$varcount)[1], smooth = 5, trace = FALSE))
#
# system.time(pdb_mod1 <- dbarts::pdbart(x.train = models[[1]]))  # took 9 minutes, so approx 6 hours for 40 models
#
#
partials <- vector("list", length(models))
names(partials) <- names(models)

system.time(for (i in 1:length(models)) {
  message(names(models)[i])
  partials[[i]] <- dbarts::pdbart(x.train = models[[i]], pl = FALSE)
  gc()
})  # took 110 mins

dir.create("../outputs/variables", showWarnings = FALSE)
saveRDS(partials, "../outputs/variables/partials.rds")

# partials <- readRDS("../outputs/variables/partials.rds")

names(partials)
lapply(partials, names)
# ...
# $Zygtor_WRB
# [1] "fd" "levs" "xlbs" "bartcall" "yhat.train" "y" "fit"

par(mfrow = c(3, 3))  # for 9 variables
plot(partials[[1]])

partials[[1]]$fd
source("plot_pdbart_edited.R")  # edited to enable using type="l"

# following code extracted from plot.pdbart at https://github.com/cran/dbarts/blob/master/R/plot.R

names(partials)[1]
x <- partials[[1]]
yrange <- range(x$fd)

par(mfrow = c(3, 3))  # for 9 variables
for(i in 1:length(x$fd)) {
  tsum <- apply(x$fd[[i]], 2, quantile, probs = c(0.05, 0.5, 0.95))
  plot(range(x$levs[[i]]), yrange, type = 'n', xlab = "", ylab = "", main = x$xlbs[i])
  lines(x$levs[[i]], tsum[2,])
}  # (when x is "Anaeph_MM5")


# get one figure per species with the variable response curves
# variables in plot rows, climods in plot columns:

names(partials)
names_partials_spp <- sapply(strsplit(names(partials), "_"), getElement, 1)
grep("Anaeph", names(partials))

(spcodes <- unique(sapply(strsplit(names(partials), "_"), getElement, 1)))

for (spc in spcodes) {
  N <- grep(spc, spcodes) + 3  # because Figs up to S3 were taken
  # png(paste0("../outputs/images/FigS", N, "_resp_curves_", spc, ".png"), width = 900, height = 1200)
  pdf(paste0("../outputs/images/FigS", N, "_resp_curves_", spc, ".pdf"), width = 7, height = 12)
  par(mfcol = c(9, 5), mar = c(4, 2, 2, 1))  # , oma = c(0, 0, 1.5, 0)
  for (i in grep(spc, names(partials))) {
    plot(partials[[i]],
         type = "l",
         col = c("black", "darkgrey"),
         ylab = "",
         cex.lab = 1)
    # title(spc, font = 3, outer = TRUE, cex.main = 3)
  }
  # title(species[species$spcode == spc, "species"], font.main = 4, cex.main = 1.2, outer = TRUE)
  dev.off()
}
