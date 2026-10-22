# this script takes a long time to run!

# SET UP ####

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(terra)  # mapping


# IMPORT PREDICTIONS AND BASEMAP ####

preds_pres <- read.csv("../outputs/tables/preds_pres.csv")
preds_fut <- read.csv("../outputs/tables/preds_fut.csv")

utm <- vect("../outputs/maps/utm.GPKG")

names(utm)
names(preds_pres)
names(preds_fut)

utm_pres <- merge(utm[ , c("utm_code", "zone_union", "has_vars")], preds_pres, by = "utm_code", all.x = TRUE)
utm_fut <- merge(utm[ , c("utm_code", "zone_union", "has_vars")], preds_fut, by = "utm_code", all.x = TRUE)

fav_cols <- names(preds_pres)[grep("_F$|_F_lower|_F_upper|_consensus", names(preds_pres))]
fav_cols


# IDENTIFY BORDER CELLS ####

names(utm)
plot(utm)
plot(utm, c("has_vars", "zone_union"), border = NA, nr = 2)

adj <- adjacent(utm, type = "rook")
adj
table(adj)
utm$border <- 0L
utm$border[which(table(adj) < 8)] <- 1L
plot(utm, "border", border = NA)


# INTERPOLATE PREDICTIONS AT UTM ZONE UNIONS ####

# the following took way too long:
#
# fav_interp <- utm[ , "utm_code", drop = FALSE]
#
# gc()
# n <- 0
# for (f in "Trikir_WRB_F") {  # fav_cols "Trikir_WRB_F"
#   n <- n + 1
#   message("interpolating fav ", n, " of ", length(fav_cols), " - ", f, "...\n")
#   fav_idw <- gstat::idw(as.formula(paste(f, "~ 1")), as(utm[!na_rows, ], "Spatial"), newdata = as(utm, "Spatial"), maxdist = 19500)
#   fav_interp[ , paste0(f, "_idw")] <- fav_idw[ , "var1.pred"]
#   rm(fav_idw)
#   gc()
# }
#
# system.time(fav_idw <- idw(as.formula(paste(f, "~ 1")), as(utm[!na_rows, ], "Spatial"), newdata = as(utm, "Spatial"), maxdist = 19500))
#
# fav_idw2 <- idw(as.formula(paste(f, "~ 1")), as(utm[!na_rows, ], "Spatial"), newdata = as(utm[na_rows, ], "Spatial"), maxdist = 19500)


# DEFINE FUNCTION FOR FILLING NA POLYGONS ####

fill <- function(x,  # SpatVector polygon map
                 fill.cols,  # names/indexes of column(s) whose NA values to fill
                 adj.type = "queen",  # argument 'type' for 'terra::adjacent'
                 fun = "mean",  # it's the only one implemented right now
                 plot = TRUE,  # plot the 'before' and 'after' maps
                 south_to_north = FALSE,
                 ...)  # arguments for 'terra::plot'
{

  # version 1.0 (26 May 2022)

  # if there are polygons surrounded by NAs (i.e. without any adjacent values), they are filled in a subsequent round using the updated (newly filled) adjacent values

  # future enhancements:
  # - run 'adjacent' only for NA polygons
  # - implement for functions other than 'mean'
  # - implement "real-time" filling, i.e. each fill can be used by next one (I think it's already done)
  # - fix south_to_north so that it doesn't disorganize the map

  if (!inherits(x, "SpatVector")) stop ("'x' must be of class SpatVector.")
  if (!(all(fill.cols %in% 1:ncol(x)) || all(fill.cols %in% names(x)))) stop ("'fill.cols' must be among the columns in 'x'.")
  if (!adj.type %in% c("rook", "queen", "touches", "intersects")) stop ("Invalid 'adj.type'.")
  if (fun != "mean") stop ("Sorry, 'mean' is currently the only implemented 'fun'.")

  if (south_to_north) {
    x$ord <- 1:nrow(x)
    x <- x[order(crds(x)[ , "y"]), ]
  }

  cat("Identifying adjacent polygons...\n")
  adjacents <- as.data.frame(terra::adjacent(x, type = adj.type))

  output <- values(x)[ , fill.cols, drop = FALSE]

  column_count <- 0
  for (column in fill.cols) {
    column_count <- column_count + 1
    cat("\nFilling column ", column_count, " of ", length(fill.cols), ":\n", sep = "")

    out <- values(x)[ , column]
    na_rows <- which(!is.finite(values(x)[ , column]))

    round_count <- 0
    while (sum(!is.finite(out)) > 0) {
      round_count <- round_count + 1
      na_rows <- which(!is.finite(out))
      na_adjs <- adjacents$from %in% na_rows

      cat("- averaging adjacent values...\n")
      progbar <- txtProgressBar(min = 0, max = length(na_rows), style = 3, char = "-")
      for (i in na_rows) {
        gc()
        setTxtProgressBar(progbar, match(i, na_rows))
        neighbs <- adjacents[adjacents$from == i, "to"]
        neighb_vals <- out[neighbs]
        neighb_mean <- mean(neighb_vals, na.rm = TRUE)
        out[i] <- neighb_mean
      }  # end for i
      cat("\n...round", round_count, "completed.\n")
    }  # end while
    cat("No more rounds for this column.\n")

    if (plot) {
      if (is.numeric(column)) column <- names(x)[column]
      x$filled <- out
      plot(x, c(column, "filled"), ...)
    }

    output[ , column] <- out
  }  # end for column

  if (south_to_north) {
    output <- output[x$ord, ]
  }

  cat("Finished!\n")
  return(output)
}


# FILL CONSENSUS PREDICTIONS AT UTM ZONE UNIONS ####

preds_pres_cons <- read.csv("../outputs/tables/preds_pres_consensus.csv")
preds_fut_cons <- read.csv("../outputs/tables/preds_fut_consensus.csv")
names(preds_pres_cons)
names(preds_fut_cons)

cons_cols <- names(preds_pres_cons)[grep("_consensus", names(preds_pres_cons))]
cons_cols
length(cons_cols)  # 24 - now 18

# present:
utm_pres_cons <- merge(utm[ , "utm_code", drop = FALSE], preds_pres_cons, by = "utm_code", all.x = TRUE)
system.time(pres_cons_filled <- fill(utm_pres_cons, cons_cols, type = "continuous", border = NA, mar = c(1, 1, 1.3, 3.1), axes = FALSE, nr = 2))
names(utm_pres_cons)
names(pres_cons_filled)
utm_pres_cons[ , paste0(cons_cols, "_filled")] <- pres_cons_filled
head(utm_pres_cons)

# future:
utm_fut_cons <- merge(utm[ , "utm_code", drop = FALSE], preds_fut_cons, by = "utm_code", all.x = TRUE)
system.time(fut_cons_filled <- fill(utm_fut_cons, cons_cols, plot = FALSE))  # took 24 mins
utm_fut_cons[ , paste0(cons_cols, "_filled")] <- fut_cons_filled
head(utm_fut_cons)

# remove filling in coastal cells (leave it only in UTM zone unions)
# to avoid extrapolating where not really necessary:
values(utm_pres_cons)[utm$border == 1, -1] <- NA
head(utm_pres_cons)
values(utm_fut_cons)[utm$border == 1, -1] <- NA
head(utm_fut_cons)

plot(utm_pres_cons, "Trikir_F_upper_consensus_filled", type = "continuous", border = NA, mar = c(1, 1, 1, 3.1), axes = FALSE)
plot(utm_fut_cons, "Trikir_F_upper_consensus_filled", type = "continuous", border = NA, mar = c(1, 1, 1, 3.1), axes = FALSE)

# save filled consensus to CSV:
names(utm_pres_cons)
write.csv(values(utm_pres_cons[ , c(1, grep("_filled", names(utm_pres_cons)))]), "../outputs/tables/preds_pres_consensus_filled.csv", row.names = FALSE)
write.csv(values(utm_fut_cons[ , c(1, grep("_filled", names(utm_fut_cons)))]), "../outputs/tables/preds_fut_consensus_filled.csv", row.names = FALSE)


# FILL FAVOURABILITY CHANGE ####

preds_change <- read.csv("../outputs/tables/preds_change.csv")
tail(preds_change)

utm_change <- merge(utm[ , 1, drop = FALSE], preds_change)
names(utm_change)

gc()
system.time(system.time(change_filled <- fill(utm_change, names(utm_change)[-1], plot = FALSE)))  # took 6.5 minutes
head(change_filled)

(spcodes <- unique(sapply(strsplit(fav_cols, "_"), getElement, 1)))
(mfrow <- modEvA::arrangePlots(length(spcodes)))

values(utm_change) <- data.frame(utm[ , 1, drop = FALSE], change_filled)
head(utm_change)
lim <- max(abs(values(utm_change)[ , -1]), na.rm = TRUE)
pal <- hcl.colors(20, "BrBG")
plot(utm_change, 2:ncol(utm_change), border = NA, type = "continuous", col = pal, range = c(-lim, lim), mar = c(1, 1, 1, 4), axes = FALSE, nr = mfrow[1])
plot(utm_change, 2, border = NA, type = "continuous", col = pal, legend = FALSE, axes = FALSE)

# remove filling in coastal cells (leave it only in UTM zone unions)
# to avoid extrapolating where not really necessary:
values(utm_change)[utm$border == 1, -1] <- NA
head(utm_change)
plot(utm_change, 2, border = NA, type = "continuous", col = pal, legend = FALSE, axes = FALSE, nr = 4)

write.csv(values(utm_change), "../outputs/tables/preds_change_filled.csv", row.names = FALSE)


# FILL INDIVIDUAL MODEL PREDICTIONS ####

system.time(pres_filled <- fill(utm_pres, fav_cols, type = "continuous", border = NA, mar = c(1, 1, 1, 3.1), axes = FALSE, nr = 2))  # took 13997.905 secs (3.9 hours) (with plot=TRUE)
utm_pres[ , paste0(fav_cols, "_filled")] <- pres_filled
head(utm_pres[ , paste0(fav_cols, "_filled")])

fut_filled <- fill(utm_fut, fav_cols, plot = FALSE)  # took 14103.480 secs (3.9 hours) with plot=FALSE
utm_fut[ , paste0(fav_cols, "_filled")] <- fut_filled
head(utm_fut[ , paste0(fav_cols, "_filled")])

# remove filling in coastal cells (leave it only in UTM zone unions)
# to avoid extrapolating where not really necessary:
values(utm_pres)[utm$border == 1, -1] <- NA
head(utm_pres)
plot(utm_pres, "Trikir_MM5_F_upper_filled", border = NA, type = "continuous", legend = FALSE, axes = FALSE, nr = 4)
values(utm_fut)[utm$border == 1, -1] <- NA
head(utm_fut)
plot(utm_fut, "Trikir_MM5_F_lower_filled", border = NA, type = "continuous", legend = FALSE, axes = FALSE, nr = 4)

# select filled columns and export to CSV:
names(utm_pres)
names(utm_fut)

utm_pres <- utm_pres[ , c(1, grep("_filled", names(utm_pres)))]
utm_fut <- utm_fut[ , c(1, grep("_filled", names(utm_fut)))]
names(utm_pres)
names(utm_fut)

write.csv(values(utm_pres), "../outputs/tables/preds_pres_filled.csv", row.names = FALSE)
write.csv(values(utm_fut), "../outputs/tables/preds_fut_filled.csv", row.names = FALSE)
