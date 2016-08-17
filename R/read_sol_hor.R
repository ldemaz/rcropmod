#' Read iprofile data from DSSAT .SOL file
#' @description This reads in horizon depths and water-holding variables from a 
#' DSSAT .SOL file, for use in initializing the experimental files
#' @param solfile Name of .SOL file to read in
#' @param profiles Vector of profile name to read in (must be exact) 
#' @return List of soil parameters of interest
#' @details This function is stripped-down version of read_sol_prof, intended to 
#' pull out just the Horizon depths, drained upper and lower limits, so that 
#' these data can be used to initialize starting soil moisture conditions in 
#' DSSAT x files. Complete profiles can be read using read_sol_prof.
#' @keywords DSSAT
#' @examples
#' f <- "~/DSSAT45/SOIL/WI.SOL"  # .SOL file (WISE)
#' read_sol_hor(solfile = f, profiles = "WI_ACLS021") 
#' @export
read_sol_hor <- function(solfile, profiles) {
   sdat <- lmisc::read_lines2(solfile)
   idx <- which(Reduce("+", lapply(profiles, grepl, sdat, fixed = TRUE)) == 1)
   ndx <- grep("^\r$", sdat)
   ndx <- ndx[ndx > min(idx)]
   ndx <- sapply(idx, function(x) ndx[ndx > x][1] - 1)
   pdat <- do.call(rbind.data.frame, lapply(1:length(idx), function(i) {
      p <- sdat[idx[i]:ndx[i]]
      ntier <- grep("@", p)  # index of SOL headers
      hind <- c(ntier[3] + 1, ifelse(length(ntier) == 4, ntier[4] - 1, 
                                     length(p)))
      hcol <- rbind("slb" = c(2, 6), "slll" = c(14, 18), "sdul" = c(20, 24))
      dul_ll0 <- sapply(1:nrow(hcol), function(x) {
         sapply(hind[1]:hind[2], function(y) {
            as.numeric(substr(p[y], hcol[x, 1], hcol[x, 2]))
         })
      })

      dul_ll<-matrix(dul_ll0,length(dul_ll0)/3, 3) # error if single column; corrected by Di Tian
      colnames(dul_ll) <- c("SLB", "SDLL", "SDUL")
      dul_llv <- unlist(lapply(colnames(dul_ll), function(x) {
         d <- dul_ll[, x]
         c(d, rep(NA, 10 - length(d)))
      }))
      return(dul_llv)
   }))
   colnames(pdat) <- unlist(lapply(c("SLB", "SLLL", "SDUL"), paste0, 1:10))
   pdat <- as.data.table(pdat)
   pdat <- cbind("prof" = gsub("\\*", "", gsub(" .*.", "", profiles)), pdat)
   pdat[, prof := as.character(prof)]
   return(pdat)
}
