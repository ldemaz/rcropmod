#' Calculate dewpoint temperature
#' @param ea Vapor pressure (mb)
#' @param rh Relative humidity (\%)
#' @param tmax Maximum temperature (deg C)
#' @param tmin Minimum temperature (deg C)
#' @param tmu Average temperature (deg C)
#' @param sh Specific humidity (kg/kg)
#' @param pres Surface pressure (mb)
#' @return Dewpoint temperature (deg C)
#' @details This function was adapted from a python function provided by the 
#' The Parallel System for Integrating Impact Models and Sectors (pSIMS). 
#' https://github.com/RDCEP/psims 
#' @references Elliott, J., D. Kelly, J. Chryssanthacopoulos, M. Glotter, 
#' Kanika Jhunjhnuwala, N. Best, M. Wilde, and I. Foster, (2014). The Parallel 
#' System for Integrating Impact Models and Sectors (pSIMS). Environmental 
#' Modeling and Software. http://dx.doi.org/10.1016/j.envsoft.2014.04.008
#' @export
dew_point <- function(ea = NULL, rh = NULL, tmax = NULL, tmin = NULL, 
                      tmu = NULL, sh = NULL, pres = NULL) {
   if(!is.null(ea)) {
      tdew <- 4302.65 / (19.4803 - log(ea)) - 243.5
   } else if(!is.null(rh) & !is.null(tmu)) {
      N <- 243.5 * log(0.01 * rh * exp((17.67 * tmu) / (tmu + 243.5)))
      D <- 22.2752 - log(rh * exp((17.67 * tmu) / (tmu + 243.5)))
      tdew <- N / D
   } else if(!is.null(rh) & !is.null(tmax) & !is.null(tmin)) {
      tmu <- 0.5 * (tmax + tmin)
      N <- 243.5 * log(0.01 * rh * exp((17.67 * tmu) / (tmu + 243.5)))
      D <- 22.2752 - log(rh * exp((17.67 * tmu) / (tmu + 243.5)))
      tdew <- N / D
   } else if(!is.null(sh) & !is.null(pres)) {
      sh[sh == 0] <- 0.00001
      N <- -243.5 * log((2.31034 * sh + 3.80166) / (pres * sh))
      D <- log((sh + 1.6455) / (pres * sh)) + 18.5074
      tdew <- N / D
   }  else {
      print('Cannot compute dewpoint temperature from inputs')
      tdew <- -99
   }
   return(tdew)
}

# e0Tmin <- 0.6108 * exp((17.27 * temp[, "tmn"]) / 
#                           (temp[, "tmn"] + 237.3)) * (temp[, "rhmn"] / 100)
# e0Tmax <- 0.6108 * exp((17.27 * temp[, "tmx"]) / 
#                           (temp[, "tmx"] + 237.3)) * (temp[, "rhmx"] / 100)
# ea <- (e0Tmin + e0Tmax) / 2
# tdew <- (116.91 + 237.3 * log(ea)) / (16.78 - log(ea))

