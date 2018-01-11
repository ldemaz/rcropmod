#' Read in profile data from DSSAT .SOL file
#' @description This reads most horizon data from an existing DSSAT soil file, 
#' so that the parameters can be used in experimental files or for analysis
#' @param solfile Name of .SOL file to read in
#' @param profiles Vector of profile name to read in (must be exact) 
#' @return List of soil parameters of interest
#' @details This function currently pulls nearly all the horizon data from a 
#' DSSAT .sol profile. It is a fuller version of read_SOL_prof
#' @keywords DSSAT
#' @examples
#' f <- "~/DSSAT45/SOIL/WI.SOL"  # .SOL file (WISE)
#' sprofs <- read_sol_prof(solfile = f, profiles = "WI_ACLS021")
#' sprofs[, c(1:7, 11:16), with = FALSE]
#' @export
read_sol_prof <- function(solfile, profiles) {
   sdat <- read_lines2(solfile)
   profilesr <- unique(profiles)
   if(length(profilesr) < length(profiles)) {
      print(paste("Warning: Be aware that there were duplicate profiles. These",
                  "have been pruned out, assuming that profile names in the",
                  ".PRO file are unique"))
   }
   idx <- unlist(lapply(profilesr, function(x) { # x <- profilesr[11]
      a <- which(stringi::stri_detect_fixed(sdat, x))
      if(length(a) == 0) stop(paste(x, "is not in the SOL file"), call. = FALSE)
      a
   }))
   # idx <- which(Reduce("+", lapply(profiles, grepl, sdat, fixed = TRUE)) == 1)
   # ndx <- grep("^\r$", sdat)
   ndx <- which(nchar(sdat) < 3)
   ndx <- ndx[ndx > min(idx)]
   ndx <- sapply(idx, function(x) ndx[ndx > x][1] - 1)
   pdat <- do.call(rbind.data.frame, lapply(1:length(idx), function(i) { #i <- 1
      p <- sdat[idx[i]:ndx[i]]
      ntier <- grep("@", p)  # index of SOL headers
      hind <- c(ntier[3] + 1, ifelse(length(ntier) == 4, ntier[4] - 1,
                                     length(p)))
      #hcol <- rbind("slb" = c(2, 6), "slll" = c(14, 18), "sdul" = c(20, 24))
      hcol <- rbind("SLB" = c(2, 6), "SLLL" = c(14, 18), "SDUL" = c(20, 24),
                    "SSAT" = c(26, 30), "SRGF" = c(32, 36), "SSKS" = c(38, 42),
                    "SBDM" = c(44, 48), "SLOC" = c(50, 54), "SLCL" = c(56, 60),
                    "SLSI" = c(62, 66), "SLCF" = c(68, 72), "SLNI" = c(74, 78),
                    "SLHW" = c(80, 84), "SLHB" = c(86, 88), "SCEC" = c(90, 94),
                    "SADC" = c(96, 100))
      
      pd <- sapply(1:nrow(hcol), function(x) { # x <- 15
         sapply(hind[1]:hind[2], function(y) { # y <- 7
            suppressWarnings(as.numeric(substr(p[y], hcol[x, 1], hcol[x, 2])))
         })
      })
      
      pd2 <- matrix(pd, length(pd) / ncol(pd), ncol(pd)) # error if single column; corrected by Di Tian
      colnames(pd2) <- tolower(rownames(hcol)) #c("SLB", "SDLL", "SDUL")
      pd_v <- unlist(lapply(colnames(pd2), function(x) {
         d <- pd2[, x]
         c(d, rep(NA, 10 - length(d)))
      }))
      out <- cbind.data.frame(as.character(profilesr[i]), t(pd_v))
      return(out)
   }))
   cnames <- c("SLB", "SLLL", "SDUL", "SSAT", "SRGF", "SSKS", "SBDM", "SLOC",
               "SLCL", "SLSI", "SLCF", "SLNI", "SLHW", "SLHB", "SCEC", "SADC")
   colnames(pdat) <- c("prof", unlist(lapply(cnames, paste0, 1:10)))
   
   pdat <- data.table(pdat)
   pdat[, prof := as.character(prof)]
   return(pdat)
}
