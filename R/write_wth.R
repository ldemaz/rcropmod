#' Write out DSSAT weather file
#' @description Write out a weather file for DSSAT
#' @param xy vector giving longitude then latitude (degrees)
#' @param elev elevation of weather site/station (m)
#' @param dates Vector of POSIX dates passed from weather function
#' @param srad Solar radiation (mj/m^2/day)
#' @param tmax Maximum temperature (deg C)
#' @param tmin Minimum temperature (deg C)
#' @param tmu Vector of mean temperatures (deg C)
#' @param prec Daily rainfall (mm)
#' @param tdewe Dew point temperate (deg C, optional)
#' @param wind Wind speed (m/s, optional)
#' @param outdir Output directory name
#' @param name Name for output wth file 
#' @return A printed WTH file in the specifed location
#' @details For now this function writes out -99 values for DEWP and WIND if 
#' missing. This could be made more efficient if these variables could be 
#' dropped.
#' @keywords DSSAT
#' @export
write_wth <- function(dates, xy, elev, srad, tmax, tmin, prec, tdew = NULL, 
                      wind = NULL, outdir, name) {
  yrs <- substr(dates, 3, 4)
  mos <- substr(dates, 6, 7)
  yyddd <- sprintf("%2s%03s", yrs, strftime(dates, format = "%j"))

  # Name vector
  if(nchar(name) == 4) {  # add year value if just 4 characters
    nyrs <- ifelse(length(unique(yrs)) > 99, 99, length(unique(yrs)))
    yrnyr <- sprintf("%2s%02s", unique(yrs)[1], nyrs)
    outname <- paste0(name, yrnyr, ".WTH")  # Output WTH name
  } else if(nchar(name) == 8) {  # add wth extension if 8
    outname <- paste0(name, ".WTH")  # Output WTH name
  } else {
    stop("name must be 4 or 8 characters long", call. = FALSE)
  }
  
  mo_mu <- t(sapply(sort(unique(mos)), function(x) {
     (mean(tmax[which(mos == x)]) + mean(tmin[which(mos == x)])) / 2 
  }))
  #if((length(tdew) == 1) & (tdew[1] == -99)) tdew <- rep(-99, length(srad))
  
  # print variables
  headvec <- c("@DATE", "SRAD", "TMAX", "TMIN", "RAIN", "DEWP", "WIND")
  spvec <- c("%5s", "%5.1f", "%5.1f", "%5.1f", "%5.1f", "%5.1f", "%5.1f")
  wlist <- c("yyddd", "srad", "tmax", "tmin", "tdew", "wind")
  
  # check if tdew, wind missing
  # if(is.null(tdew)) {
  #   dropind <- which(headvec == "DEWP")
  #   headvec <- headvec[-dropind]
  #   spvec <- spvec[-dropind]
  # } 
  # if(is.null(wind)) {
  #    dropind <- which(headvec == "WIND")
  #    headvec <- headvec[-dropind]
  #    spvec <- spvec[-dropind]
  # }

  # if tdew missing or wind missing, -99
  if(is.null(tdew)) tdew <- rep(-99, length(srad))
  if(is.null(wind)) wind <- rep(-99, length(srad))

  # combine output into sprintf  
  output <- rbind(sprintf("%s %s", "*WEATHER DATA :", substr(outname, 1, 4)),
                  c(""),
                  # Second tier
                  c('@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT'),
                  sprintf("%6s %8.3f %8.3f %5.0f %5.1f %5.1f %5.2f %5.2f", 
                          name, xy[2], xy[1], elev, mean(mo_mu), # Tav
                          (max(mo_mu) - min(mo_mu)) * 0.5,  # Amp
                          1.75, 2.00),  # REFHT and WNDHT  (hardcoded for now)
      
                  # Data tier
                  c('@DATE  SRAD  TMAX  TMIN  RAIN  DEWP  WIND'),
                  cbind(sprintf("%5s %5.1f %5.1f %5.1f %5.1f %5.1f %5.1f",
                                yyddd, srad, tmax, tmin, prec, tdew, wind)))

  write(output, fp(outdir, outname))   
}



