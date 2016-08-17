#' Creates weather data file for crop model
#' @description Creates certain key inputs for crop model weather data files and
#' write them to appropriate format
#' @param xy vector giving longitude then latitude (degrees)
#' @param srad Solar radiation (mj/m^2/day)
#' @param tmax Maximum temperature (deg C)
#' @param tmin Minimum temperature (deg C)
#' @param prec Daily rainfall (mm)
#' @param ea Vapor pressure (mb, optional)
#' @param rh Relative humidity (\%, optional)
#' @param tmu Average temperature (deg C, optional)
#' @param sh Specific humidity (kg/kg, optional)
#' @param pres Surface pressure (mb, optional)
#' @param tdew Dewpoint temperature (deg C, optional)
#' @param sdate Start date for range in WTH file
#' @param edate End date for range in WTH file
#' @param name Name prefix for output weather data file
#' @param outdir Output directory for weather file
#' @param model Crop model type, default = "DSSAT"
#' @return An output crop model weather file for a single location
#' @details This is currently only works for the DSSAT model, but is intended 
#' to be expanded to other models (e.g. APSIM). Note that this function assumes 
#' that daily data given to it will be the same date as UTC. This might be 
#' problematic in Australasia. 
#' @examples
#' sdate <- "19790701" 
#' edate <- "20100630"
#' xy <- c("y" = -16.2783358, "x" = 27.4738917) 
#' load("data/sp-clim-dat.rda")
#' elev <- 1120
#' srad <- swv
#' name <- "ZASP"
#' outdir <- "external/ext_data"
#' weather(xy, elev, srad, tmax, tmin, prec, wind = wind, sh = sh, 
#'         pres = pres, sdate = sdate, edate = edate, name = name, 
#'         outdir = outdir)
#' @export
weather <- function(xy, elev, srad, tmax, tmin, prec, wind = NULL, 
                    ea = NULL, rh = NULL,  tmu = NULL, sh = NULL, pres = NULL,
                    tdew = NULL, sdate, edate, name, outdir, model = "DSSAT") {   						
   
   # set up dates
   ymd <- seq(as.Date(sdate,"%Y%m%d"), as.Date(edate,"%Y%m%d"), by = 1)

   # some key variables
   if(is.null(tmu)) tmu <- (tmax + tmin) / 2  # calculate tmu
   if(is.null(tdew) & all(!is.null(c(ea, rh, sh)))) {
      tdew <- dew_point(ea = ea, rh = rh, tmax = tmax, tmin = tmin, 
                        tmu = tmu, sh = sh, pres = pres)
   }
   
   if(model == "DSSAT") {
      # New YYddd vector
      write_wth(dates = ymd, xy = xy, elev = elev, srad = srad, tmax = tmax,
                tmin = tmin, prec = prec, tdew = tdew, wind = wind, 
                outdir = outdir, name = name)
   }  # expand here in future as more models added...
}



   
