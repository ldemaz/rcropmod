## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, collapse = T, 
                      comment = "#>", fig.width = 5, fig.height = 5, 
                      fig.align = "center")
library(rcropmod)

## ---- message=FALSE, warning=FALSE, eval=FALSE---------------------------
#  p_geno <- "~/DSSAT45/GENOTYPE/"  # path to genotype folder
#  
#  # back-up existing file
#  file.rename(fp(p_geno, "MZCER045.CUL"), fp(p_geno, "MZCER045.BAK"))
#  
#  # copy new one over
#  file.copy(system.file("extdata", "MZCER045.CUL", package = "rcropmod"),
#            fp(p_geno, "MZCER045.CUL"))

## ---- eval = FALSE-------------------------------------------------------
#  p_soil <- "~/DSSAT45/SOIL/"
#  file.copy(system.file("extdata", "WI.SOL.zip", package = "rcropmod"),
#            fp(p_soil, "WI.SOL.zip"))
#  unzip(fp(p_soil, "WI.SOL.zip"), exdir = p_soil) # unzip
#  file.remove(fp(p_soil, "WI.SOL.zip"))  # remove zip file

## ------------------------------------------------------------------------
# read soil raster and csv into data.table
zmsolref <- fread(system.file("extdata", "zamsoils.csv", package = "rcropmod"))
zmsolgrid <- raster(system.file("extdata", "zmsoilgrid.grd", 
                                package = "rcropmod"))

## ------------------------------------------------------------------------
xy <- cbind("x" = unname(weathdat$xyz["lon"]), 
            "y" = unname(weathdat$xyz["lat"]))
pt <- cbind.data.frame("x" = xy[, "x"], "y" = xy[, "y"]) 
coordinates(pt) <- ~x + y  # convert to spatialPoints

# Identify which soil grid cell grid the point intersects
soilid <- extract(zmsolgrid, pt)  # intersects point with raster of cell IDs 
profs <- zmsolref[CELL5M == soilid]  # select corresponding profile names

## ---- message = FALSE, eval = FALSE--------------------------------------
#  profdat <- read_sol_hor(solfile = "~/DSSAT45/SOIL/WI.SOL",
#                          profiles = profs[, SoilProfile])
#  profdat$prof  # the first set of profile data

## ---- eval = FALSE-------------------------------------------------------
#  # combine WTH name, coordinates, soil grid id, and profile data
#  field <- cbind("WTH" = "TEST7932", xy, soilid, profdat)
#  
#  # Create unique field ID (FID) and name for output X file that drives DSSAT
#  # experiments
#  field[, c("ID_FIELD", "XNAME") := list(fid(field$WTH), xname(field$WTH, .N))]
#  
#  # Fixed parameters
#  field$CR <- "MZ"  # crop to plant (maize)
#  field$PPOP <- 3.7  # maize planting density
#  field$PDATE <- 79305 # planting dates (have to provide)
#  field$SDATE <- field$ICDAT <- 79298 # initial condition and starting date
#  field$PLRS <- 90   # row spacing
#  field$FAMN <- 5  # per Thornton et al (2009)
#  field$H20_s <- 0.05  # initial soil moisture content of soil
#  field$NYERS <- 31  # number of years to run each simulation
#  
#  # create table with linking fixed field parameters to weather and soil data
#  xtab <- x_tab(fields = field)
#  
#  # Now varying treatment parameters
#  cult <- c("HY0001", "HY0006")  # cultivar names
#  CLNUM <- 1:length(cult)  # number for each cultivar
#  pdates <- strftime(seq.Date(as.Date("1979-11-01"), as.Date("1979-12-31"), 14),
#                     "%y%j")  # planting date vector, converted by YYDOY
#  
#  # combine treatments, use t_tab function to assign additional X file parameters
#  tcomb <- expand.grid(list("PDATE" = pdates, "INGENO" = cult),
#                       stringsAsFactors = FALSE)
#  ttab <- cbind("N" = 1:nrow(tcomb), tcomb,
#                t_tab(tvars = c("PDATE", "INGENO"), topts = c("MP", "CU"),
#                      ttab = tcomb))  # use t_tab func
#  
#  # Now join the variable treatment table to the fixed table, splitting fields
#  # into a list
#  xtabl <- lapply(1:nrow(xtab), function(x) { # x <- 1
#    print(x)
#    d <- xtab[x, ]  # split field off (1 row == 1 field)
#    d2 <- do.call(rbind, lapply(1:nrow(ttab), function(x) d))  # expand rows
#    upd_col <- colnames(d2)[which(colnames(d2) %in% colnames(ttab))]
#    d2[, c(upd_col) := ttab[, upd_col]]  # update columns having variable values
#    xt <- cbind(data.table(ttab), d2[, !colnames(d2) %in% upd_col, with = FALSE])
#  })

## ---- eval=FALSE---------------------------------------------------------
#  xrun <- lapply(xtabl, function(x) {  # x <- xtabl[[1]]
#    print(x$XNAME[1])
#    xf <- copy(x)
#    xfnm <- x_file(xtab = xf, outdir = "~/DSSAT45/Maize", z = "01",
#                   xtype = ".MZX")
#    bname <- batch_file(xl = list(xf), xfiles = xfnm,
#                      outdir = "~/DSSAT45/Maize", btype = "MAIZE")
#    exec_csm(projdir = getwd(), csmdir = "~/DSSAT45/", rundir = "~/DSSAT45/Maize",
#             bname = bname)
#    sdat <- read_csm_outfile(rundir = "~/DSSAT45/Maize", type = "summary",
#                             vars = c("RUNNO", "TRNO", "FNAM", "SOIL_ID...",
#                                      "PDAT", "MDAT", "HWAH", "PRCM"))
#  })

## ---- echo=FALSE, eval=FALSE---------------------------------------------
#  # save for faster vignette building
#  setwd("~/Dropbox/projects/rcropmod/public/rcropmod/")
#  save(xrun, file = "vignettes/disabled/xrun.rda")

## ---- echo=FALSE---------------------------------------------------------
# save for faster vignette building
load("~/Dropbox/projects/rcropmod/public/rcropmod/vignettes/disabled/xrun.rda")

## ------------------------------------------------------------------------
sdat <- rbindlist(xrun)
setnames(sdat, names(sdat), gsub("\\_ID|NO|\\.", "", names(sdat)))

# merge with soil profiles 
sdat <- merge(sdat, profs[, -1, with = FALSE], by.x = "SOIL", 
              by.y = "SoilProfile")

# set up indices for identifying where cultivar treatments start and end
ind <- seq(1, 10, 5)  
cult <- rep(0, nrow(sdat))
for(i in 1:2) cult[which(sdat$TR %in% ind[i]:(ind[i] + 4))] <- i
sdat[, CU := cult]
setcolorder(sdat, c("RUN", "TR", "CU", "FNAM", "SOIL", "PDAT", "MDAT",
                    "HWAH", "PRCM", "SharePct"))  # reorder, for tidiness

## ---- message = FALSE----------------------------------------------------
sdat_red <- sdat[, list("HWAH" = weighted.mean(HWAH, w = SharePct)), 
                 by = list(PDAT, CU)]
sdat_red[, PDY := substr(PDAT, 5, 7)]  # get planting dates with out year
sdat_red[, YR := substr(PDAT, 1, 4)]  # get planting dates with out year

## ------------------------------------------------------------------------
cols <- c("red", "blue")
yrng <- round(range(sdat_red$HWAH) / 1000) * 1000
plot(1:5, 1:5, ylim = yrng, pch = "", xlab = "PDATE", ylab = "HWAH",
     xaxs = "i", yaxs = "i", las = 2, xaxt = "n")
axis(1, at = 1:5, labels = unique(sdat_red[, PDY]), las = 2)
polygon(x = c(0, 0, 5, 5, 0), y = c(0, yrng[2], yrng[2], 0, 0), col = "grey")
for(i in 1:length(ind)) {
  lines(sdat_red[, mean(HWAH), by = list(PDY, CU)][CU == i][, V1], 
        col = cols[i])
}

## ------------------------------------------------------------------------
plot(1:31, 1:31, ylim = yrng, pch = "", xlab = "YEAR", ylab = "HWAH", 
     xaxs = "i", yaxs = "i", las = 2, xaxt = "n")
axis(1, at = 1:31, labels = unique(sdat_red[, YR]), las = 2)
polygon(x = c(0, 0, 31, 31, 0), y = c(0, yrng[2], yrng[2], 0, 0), col = "grey")
for(i in 1:length(ind)) {
  lines(sdat_red[, mean(HWAH), by = list(YR, CU)][CU == i][, V1], 
        col = cols[i])
}

## ------------------------------------------------------------------------
cols <- RColorBrewer::brewer.pal(n = 5, name = "Spectral")
plot(1:31, 1:31, ylim = yrng, pch = "", xlab = "YEAR", ylab = "HWAH", 
     xaxs = "i", yaxs = "i", las = 2, xaxt = "n")
axis(1, at = 1:31, labels = unique(sdat_red[, YR]), las = 2)
polygon(x = c(0, 0, 31, 31, 0), y = c(0, yrng[2], yrng[2], 0, 0), col = "grey")
pdy <- unique(sdat_red$PDY)
for(i in 1:length(pdy)) {
  sdat_red[, mean(HWAH), by = list(PDY, YR)][PDY == pdy[i]][, 
           lines(V1, col = cols[i])] 
}

