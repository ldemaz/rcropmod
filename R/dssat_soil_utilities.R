# soil utility functions for DSSAT
# load("~/Dropbox/projects/dssattx/external/data/ctysoilsm.rda")

#' DSSAT45 root growth function
#' @param depth Horizon depth
#' @return Root distributions for DSSAT soil profiles
#' @keywords internal
#' @export 
srgf <- function(depth) {
   hmid <- rowMeans(cbind(c(0, depth[-length(depth)]), depth))
   v <- exp(-0.02 * hmid)
   v[1] <- ifelse(min(depth) < 15, 1, v[1])
   return(v)
}

#' Evaporation limit function from Du Toit, source unknown
#' @param clay1 Clay percentage for the top soil horizon
#' @keywords internal
#' @export 
slu1 <- function(clay1) {
   ifelse((5 + 0.15 * (100 - (98 - clay1))) > 9.9, 9.9, 
          (5 + 0.15 * (100 - (98 - clay1))))
}

#' Soil drainage rate
#' @param clay Clay percentage
#' @keywords internal
#' @export 
#' @details Based on Estes et al (2013), following Schulze et al for ACRU model.
#' Region-specific functions might be better to use
sldr <- function(clay) {
   sldr <- ifelse(clay > 0 & clay <= 3.3, 0.8, 0)  # Sand < 3.3 % clay
   sldr <- ifelse(clay > 3.3 & clay <= 10, 0.7, sldr)  # Loamy sand - 3.3-10 %
   sldr <- ifelse(clay > 10 & clay <= 20, 0.65, sldr)  # Sandy loam - 10-20 %
   sldr <- ifelse(clay > 20 & clay <= 35, 0.5, sldr)  # Sandy clay loam - 20-35 %
   sldr <- ifelse(clay > 35 & clay <= 45, 0.4, sldr)  # Sandy clay - 35-45 %
   sldr <- ifelse(clay > 45, 0.15, sldr)  # Clay - >45 %
   return(sldr)
}

#' Calcutes the runoff curve number following Gijsman et al (2007)
#' @param slo Slope in percent
#' @param  depth Total depth of profile (in cm)
#' @param textclass Texture class (from soil_class function)
#' @param drainage Soil drainage rate (from SLDR function)
#' @keywords internal
#' @export
# slo <- 4; depth <- 200; texture <- "SLm"; drainage <- sldr(30)
# soilss <- soils[grid == 3468]
slro <- function(slo, depth, texture, drainage) {
  curvenum <- cbind("A" = c(61, 64, 68, 71), "B" = c(73, 76, 80, 83), 
                    "C" = c(81, 84, 88, 91), "D" = c(84, 87, 91, 94))
  slopevals <- cbind(c(0, 2, 5, 10), c(2, 5, 10, 1000))
  csoils <- c("Cl", "SiCl", "SiClLo", "ClLo")
  if(length(unique(slo)) > 1) {
     stop("Messed up slope values for profile", call. = FALSE)
  }
  mxdepth <- max(depth)
  sldrmu <- mean(drainage)  # mean drainage rate
  grps <- LETTERS[1:4]
  if(((mxdepth > 150) & all(texture == "Sa")) | (sldrmu >= 0.75)) {
    grp <- grps[1]
  }
  if(((mxdepth < 150) & all(texture == "Sa")) | 
    ((sldrmu < 0.75) & (sldrmu >= 0.6))) {
    grp <- grps[2]
  }
  if(((mxdepth < 80) & all(texture %in% csoils)) | (sldrmu < 0.3)) {
    grp <- grps[3]
  }
  if(((mxdepth < 80) & (sldrmu < 0.25)) | 
     (all(texture == "Cl") & (sldrmu < 0.3))) {
    grp <- grps[4]
  }
  if(!exists("grp")) {
    grp <- grps[2]  # if not yet classed, group B
  }
  ind <- which((slo[1] >= slopevals[, 1]) & (slo[1] < slopevals[, 2]))
  scs <- curvenum[ind, which(colnames(curvenum) %in% grp)]
  return(scs)
}

## Makes sure that depth of first horizon is at least 15 cm or less
## @param horizons Dataframe of soil profiles with multiple horizons
## @details Function will add in new layer above the first listed layer one
## with half the depth.
# horizon_fix <-  function(soildt) {
#   depths <- cbind(soildf$low_depth[-nrow(soildf)], soildf$low_depth[-1])
#   depthsdiff <- depths[, 2] - depths[, 1]
#   if(any(depthsdiff < 0)) {
#      print("Horizons out of order, reordering")
#      soildf <- soildf[order(soildf$low_depth), ]
#   } else{
#      soildf <- soildf
#   }
#   if(soildf$low_depth[1] > 15) {
#     print(paste("Filling in a new topsoil horizon because depth of current top",
#                 "soil is greater than 15 cm"))
#     hor1 <- soildf[1, ]
#     hor1$low_depth <- round(hor1$low_depth / 2)
#     out <- rbind(hor1, soildf)
#   } else {
#     print("Horizons in order and top horizon is less than 15 cm deep")
#     out <- soildf
#   }
#   out
# }

#' Creates a DSSAT .SOL (soil input) file
#' @param soildt A data.table containing, at a minimum, the critical variable
#' set (see details)
#' @param filename Output file name, including path (ideally to DSSATXX/SOIL). 
#' See details regarding naming convention.
#' @param headstring An optional description for the top of the file.
#' @param overwrite TRUE or FALSE (default) to overwrite existing file.
#' @param tier4 TRUE or FALSE (default) to write fourth tier of soil parameters. 
#' These contain data that are usually not available. 
#' @details This function takes an entire soil input data.table and writes it 
#' out into a DSSAT .SOL soil input file. The input data.table must contain, 
#' at a minimum, the variables listed in \strong{ds_solcritvars}, named exactly 
#' as specified. These, and any other additional variables passed in through 
#' soildt should be named according to the \emph{dtnames} column in 
#' \strong{ds_solkey}. The corresponding DSSAT variable names are in the 
#' \emph{dssatnames} columns. In creating the file, note that DSSAT soil input 
#' files should have a name that matches the first two characters of the profile 
#' names within it, otherwise the model will not be able to find the soil 
#' profile. So if the profiles begin with AB, the soil file should be named 
#' AB.SOL. The soil profile name itself should be 10 characters long, beginning 
#' with the two character common prefix, following by additional characters and 
#' numbers, e.g. ABAA000001, AB00000001. The file should be written or 
#' transferred into the DSSATXX/SOIL directory. For large area applications, 
#' it might be preferable to create several soil files, perhaps one for each 
#' district, province or state. 
#' @export  
#@examples
#dssat_soil_print(soil, filename = "TT", overwrite = TRUE)
# soildt <- copy(soil)
dssat_soil_print <- function(soildt, filename, headstring = "SOIL file", 
                             overwrite = FALSE, tier4=FALSE) {
  # set up
  DT <- copy(soildt)  # copy data.table to prevent modification of orig
  profs <- DT[, unique(prof)]  # unique profile IDs
  unnms <- ds_solkey[ds_solkey$soltier == "unnamed", ]
  t1nms <- ds_solkey[ds_solkey$soltier == "tier1", ]
  t2nms <- ds_solkey[ds_solkey$soltier == "tier2", ]
  t3nms <- ds_solkey[ds_solkey$soltier == "tier3", ]
  t4nms <- ds_solkey[ds_solkey$soltier == "tier4", ]
  
  # DT[, c("fc", "wp") := NULL]
  if(!all(ds_solcritvars %in% names(DT))) {
     notin <- ds_solcritvars[!ds_solcritvars %in% names(DT)]
     stop(paste0("Soil input file is missing critical parameter(s): ", 
                paste0(notin, collapse = "; "), ". Please add these as needed.",
                " Parameters fc, wp, sat, and bd can be calculated using",
                " pedotransfer functions provided by this package, if ",
                " soil clay, sand (or silt), and OC are provided.",
                " A future update of this package will have these calculated", 
                " automatically if missing."),
          call. = FALSE)
  }
  
  # DT[2, fc := NA]
  for(i in names(DT)) {
     if(any(DT[, is.na(get(i))])) {
       stop(paste("Soil dataset has missing data in it. This function does not", 
                  "currently have gap-filling functionality. Please fix", "
                  first!"), call. = FALSE)
     }
  }

  # Process individual profiles
  # tier4=FALSE; i <- profs[1]
  sols <- do.call(rbind, lapply(profs, function(i) {
    DF <- DT[prof == i]
    if(DF[1, depth] > 15) warning("First horizon depth should be < 15 cm")

    # Prepare row inputs
    # For average soil clay and sand percentages, need to take weight means and
    # averages between top and bottom of horizons
    hthick <- (DF[, depth] - c(0, DF[-.N, depth]))  # horizon thickness
    claymids <- rowMeans(cbind(c(DF[1, clay], DF[-.N, clay]), DF[, clay])) 
    sandmids <- rowMeans(cbind(c(DF[1, sand], DF[-.N, sand]), DF[, sand])) 
    claymu <- sum(claymids * hthick / sum(hthick))
    sandmu <- sum(sandmids * hthick / sum(hthick))

    # unnamed toprow
    vals <- data.table(unnms[!unnms %in% names(DF)])
    texture <- ifelse("texture" %in% vals$dtname, 
                      soil_class(claymu, sandmu), DF$texture)
    inst <- ifelse("inst" %in% vals$dtname, vals[dtname == "inst", dum],
                   DF$inst)
    descr <- ifelse("descr" %in% vals$dtname, DF$prof[1], DF$descr)
    hrow <- sprintf("%12-s %11-s %3s %8i %25-s",
                    paste0("*", DF[1, prof]), inst, texture, DF[, max(depth)], 
                    paste(inst, DF[1, prof], sep = " "))
    
    # 1st tier
    vals <- data.table(t1nms[!t1nms %in% names(DF)])
    site <- ifelse("site" %in% vals$dtname, vals[dtname == "site", dum], 
                   DF$site[1])
    ctry <- ifelse("country" %in% vals$dtname, vals[dtname == "country", dum], 
                   DF$country[1])
    scs <- ifelse("scs" %in% vals$dtname, vals[dtname == "scs", dum], DF$scs)
    row1 <- sprintf("%5s %14s %12.3f %8.3f %18-s", site, ctry, DF$lat[1], 
                    DF$long[1], scs)
    
    # 2nd tier
    vals <- data.table(t2nms[!t2nms %in% names(DF)])
    dumvals <- t2nms[!t2nms$dtname %in% c("evap", "drain", "runoff"), "dtname"]
    # scol <- ifelse("col" %in% vals$dtname, -99, DF$col[1])
    # alb <- ifelse("alb" %in% vals$dtname, vals[dtname == "alb", dum], DF$alb[1])
    v <- lapply(dumvals, function(x) {
      as.numeric(ifelse(x %in% vals$dtname, vals[dtname == x, dum], 
                        DF[1, x, with = FALSE])) 
    })
    names(v) <- dumvals
    
    drainage <- sldr(claymu)
    row2 <- sprintf("%6s %5.2f %5.1f %5.2f %5.1f %5.2f %5.2f %5s %5s %5s",
                    v$col, as.numeric(v$alb), slu1(DF$clay[1]), drainage, 
                    unname(slro(DF$slope[1], max(DF$depth), texture, drainage)),
                    v$nfact, v$pfact, v$phmeth, v$pmeth, v$kmeth)
    
    # 3rd tier
    vals <- data.table(t3nms[!t3nms %in% names(DF)])
    
    # Calculate SRGF if rootg factor is not already in input table, or if it is
    # provided and has missing values (in which case all values overwritten with
    # DSSAT SRGF formula)
    # hmid <- rowMeans(cbind(c(0, DF[-.N, depth]), DF[, depth]))  # horizon middle
    # DF[, rootg := srgf(depth)]; DF[2, rootg := -99]
    # DF[, srgf := NULL]; DF[, rootg := NULL]
    if(!"rootg" %in% names(DF)) DF[, rootg := srgf(depth)]
    if("rootg" %in% names(DF)) {
       if(DF[, !all(between(rootg, 0, 1))]) DF[, rootg := srgf(depth)]
    } 
    
    # Figure out whether any non-critical variables are provided. Defaults if not
    dnms <- c("depth", "wp", "fc", "sat", "bd", "oc", "rootg") # critical vars
    dumvals <- t3nms[!t3nms$dtname %in% c(dnms, names(DF)), "dtname"]
    for(i in dumvals) {
      v <- as.numeric(ifelse(i %in% vals$dtname, vals[dtname == i, dum], 
                             DF[1, i, with = FALSE]))
      DF[, c(i) := v]
    }
    
    # adjust variable rounding
    rnds <- c(0, 0, 3, 3, 3, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 0, 3)
    DFn <- do.call(cbind.data.frame, lapply(1:length(rnds), function(x) {  
      # x <- 2
      nm <- t3nms$dtname[x]
      rv <- DF[[nm]]
      rnd <- ifelse(sum(rv) / length(rv) == -99, 0, rnds[x]) 
      fval <- as.character(format(round(as.numeric(as.character(rv)), rnd),
                                  nsmall = rnd))
      fval
    }))
    colnames(DFn) <- t3nms$dtname
    
    # sprint it
    sprintstr <- paste("%6s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s", 
                       "%5s %5s %5s %5s") 
    row3 <- sprinter(DFn, "depth", sprintstr, 
                     t3nms[t3nms$dtname != "depth", "dtname"])
    
    # 4th tier (optional)
    if(tier4 == TRUE) {
      vals <- data.table(t4nms[!t4nms %in% names(DF)])

      # Figure out whether any variables are provided as inputs
      dumvals <- t4nms[!t4nms$dtname %in% names(DF), "dtname"]
      for(i in dumvals) {
         v <- as.numeric(ifelse(i %in% vals$dtname, vals[dtname == i, dum], 
                                DF[1, i, with = FALSE]))
         DF[, c(i) := v]
      }
      
      # adjust variable rounding
      rnds <- c(0, 0, 3, 3, 3, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 0, 3)
      t4nmv <- c("depth", t4nms$dtname)  # need to recycle depth
      DFn <- do.call(cbind.data.frame, lapply(1:length(rnds), function(x) {  
         # x <- 2
         nm <- t4nmv[x]
         rv <- DF[[nm]]
         rnd <- ifelse(sum(rv) / length(rv) == -99, 0, rnds[x]) 
         fval <- as.character(format(round(as.numeric(as.character(rv)), rnd),
                                     nsmall = rnd))
         fval
      }))
      colnames(DFn) <- t4nmv
      
      # sprint it
      sprintstr <- paste("%6s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s", 
                         "%5s %5s %5s %5s") 
      row4 <- sprinter(DFn, "depth", sprintstr, t4nms$dtname)
    } 

    # Combine all elements into printable table
    sol <- rbind(hrow,  # header line
       # 1st tier with header
       "@SITE        COUNTRY          LAT     LONG SCS FAMILY ", row1,
       # 2nd tier with header    
       "@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE", row2,
       # 3rd tier (horizons) 
       paste0("@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC",
              "  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC"), row3)
    if(exists("row4") & tier4 == TRUE) {
       sol <- rbind(sol, 
                    paste0("@  SLB  SLPX  SLPT  SLPO CACO3  SLAL  SLFE  SLMN",
                           "  SLBS  SLPA  SLPB  SLKE  SLMG  SLNA  SLSU  SLEC",
                           "  SLCA"), row4, c(""))
    } else {
       sol <- rbind(sol, c(""))
    }
    return(sol)
  }))
  
  # write it out
  # fname <- "external/ext_data/test"
  # headstring <- "blahblah"
  fname <- paste0(filename, ".SOL")
  if(file.exists(fname) & overwrite == FALSE) {
     stop("File exists--set overwrite to TRUE if you don't care!")
  }
  print(paste("Writing DSSAT soil file:", fname))
  write(paste("*SOILS:", headstring), file = fname)
  write(c(""), file = fname, append = TRUE)
  write(sols, file = fname, append = TRUE)
  print(paste("You can check", fname, "now!"))
}




