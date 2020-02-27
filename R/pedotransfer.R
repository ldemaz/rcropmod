# Pedotransfer functions
#' Calculate wilting point
#' @param sand Fraction of sand
#' @param clay Fraction of clay
#' @param soc Soil organic matter percent
#' @return Wilting point at 1500 kpa
#' @keywords internal
#' @export 
wilt_point <- function(sand, clay, soc) {
  theta1500t <- -0.024 * sand + 0.487 * clay + 0.006 * soc + 
    (0.005 * sand * soc) - (0.013 * clay * soc) + (0.068 * sand * clay) + 
     0.031
  theta1500 <- theta1500t + (0.14 * theta1500t - 0.02)
  return(theta1500)  
}

#' Calculates field capacity
#' @param sand Fraction of sand
#' @param clay Fraction of clay
#' @param soc soil organic matter percent
#' @return Field capacity at 33 kpa
#' @keywords internal
#' @export 
field_cap <- function(sand, clay, soc) {
  theta33t <- -0.251 * sand + 0.195 * clay + 0.011 * soc + 
     (0.006 * sand * soc) - (0.027 * clay * soc) + 
     (0.452 * sand * clay) + 0.299
  theta33 <- theta33t + ((1.283 * theta33t^2) - 0.374 * theta33t - 0.015)
  return(theta33)
}  

#' Calculates saturated moisture content, requires function field_cap
#' @param sand Fraction of sand
#' @param clay Fraction of clay
#' @param soc soil organic matter percent
#' @return Field capacity at 33 kpa
#' @keywords internal
#' @export 
theta_s <- function(sand, clay, soc) {
  thetas_33t <-  0.278 * sand + 0.034 * clay + 0.022 * soc - 
     (0.018 * sand * soc) - (0.027 * clay * soc) - (0.584 * sand * clay) + 0.078
  thetas_33 <- thetas_33t + (0.636 * thetas_33t - 0.107)
  theta33 <- field_cap(sand, clay, soc)
  thetas <- theta33 + thetas_33 - 0.097 * sand + 0.043
  return(thetas)
}

#' Matric density accounting for compaction
#' @param thetas Saturation water content
#' @param DF Density factor between 0.9 and 1.3, normal (default) at 1
#' @return Matric density
#' @keywords internal
#' @export 
ro_df <- function(thetas, DF = 1) {
  rodf <- ((1 - thetas) * 2.65) * DF
  return(rodf)
}

#' Bulk density accounting for compaction plus gravel
#' @param thetas Saturation water content (without compaction)
#' @param DF Density factor between 0.9 and 1.3, normal (default) at 1
#' @param gravel Gravel percent by weight
#' @keywords internal
#' @export 
bdens <- function(thetas, DF = 1, gravel = 0) {
  rodf <- ro_df(thetas, DF)
  gravel_pctv <- ((rodf / 2.65 ) * gravel) / (1 - gravel * ( 1 - rodf / 2.65))
  ro_b  <- gravel_pctv * 2.65 + (1 - gravel_pctv) * rodf
  return(ro_b)
}

#' Calculates saturated water content, accounting for compaction
#' @param sand Fraction of sand
#' @param clay Fraction of clay
#' @param soc Soil organic matter percent
#' @param DF Density factor between 0.9 and 1.3, no effect if set to 1
#' @keywords internal
#' @export 
theta_sdf <- function(sand, clay, soc, DF) {
  thetas <- theta_s(sand, clay, soc)
  rodf <- ro_df(thetas, DF)
  thetasdf <- 1 - (rodf / 2.65)
  return(thetasdf)
}

#' Calculated field capacity accounting for compaction
#' @param sand Fraction of sand
#' @param clay Fraction of clay
#' @param soc Soil organic matter percent
#' @param DF Density factor between 0.9 and 1.3, normal (default) at 1
#' @keywords internal
#' @export
field_cap_df <- function(sand, clay, soc, DF) {
  thetas <- theta_sdf(sand, clay, soc, DF = 1)  # Normal theta_s
  thetasdf <- theta_sdf(sand, clay, soc, DF)  # theta_s with compaction
  fcdf <- field_cap(sand, clay, soc) - 0.2 * (thetas - thetasdf)
  return(fcdf)
}

#' Saturated hydraulic conductivity, including gravel effects. 
#' @param sand Fraction of sand
#' @param clay Fraction of clay
#' @param soc Soil organic matter percent
#' @param DF Density factor between 0.9 and 1.3, normal (default) at 1
#' @param gravel Gravel percent by weight (0 by default)
#' @keywords internal
#' @export
ksat <- function(sand, clay, soc, DF = 1, gravel = 0) {
  fcdf <- field_cap_df(sand, clay, soc, DF)
  wp <- wilt_point(sand, clay, soc)
  lambda <- (log(fcdf) - log(wp)) / (log(1500) - log(33))  # = 1/Beta
  thetas <- theta_s(sand, clay, soc)  # theta_sdf no density effects
  mdens <- bdens(thetas, DF, gravel = 0)  # BD no gravel to get matric density
  thetasdf <- theta_sdf(sand, clay, soc, DF = DF)  # ThetaSDF w/density effects
  theta_sdf_fcdf <- thetasdf - fcdf
  theta_sdf_fcdf <- ifelse(theta_sdf_fcdf < 0, 0, theta_sdf_fcdf) # FC ! > por.
  kbks <- (1 - gravel) / (1 - gravel * (1 - 1.5 * (mdens / 2.65)))  
  ks <- 1930 * (theta_sdf_fcdf)^(3 - lambda) * kbks
  return(ks)
}

#' Plant available water, adjusted for gravel and density effects. 
#' @param sand Fraction of sand
#' @param clay Fraction of clay
#' @param soc Soil organic matter percent
#' @param DF Density factor between 0.9 and 1.3, normal (default) at 1
#' @param gravel Gravel percent by weight (0 by default)
#' @keywords internal
#' @export
paw <- function(sand, clay, soc, DF = 1, gravel = 0) {
  thetas <- theta_sdf(sand, clay, soc, DF = 1)
  thetasdf <- theta_sdf(sand, clay, soc, DF = DF)
  rodf <- ro_df(thetas, DF)
  gravel_pctv <- ((rodf / 2.65 ) * gravel) / (1 - gravel * ( 1 - rodf / 2.65))
  fcdf <- field_cap(sand, clay, soc) - 0.2 * (thetas - thetasdf)
  wp <- wilt_point(sand, clay, soc)
  paw <- (fcdf - wp) * (1 - gravel_pctv)
  return(paw)
}

#' Calculates various soil hydraulic properties, following Saxton & Rawls, 2006
#' @param sand Fraction of sand
#' @param clay Fraction of clay
#' @param soc Soil organic matter percent
#' @param DF Density factor between 0.9 and 1.3, normal (default) at 1
#' @param gravel Gravel percent by weight (0 by default)
#' @param digits Number of significant digits (4 by default)
#' @param PAW Gravel and density adjusted plant available water (TRUE or FALSE)
#' @details A single function producing estimates of wilting point, 
#' field capacity, saturated water content, bulk density, and saturdated 
#' hydraulic conductivity, account for soil density and gravel effects, based on 
#' methods described by Saxton and Rawls (2006). Internal functions for each 
#' variables can be also used separately, as needed. Per Saxton & Rawls (2006), 
#' these functions are only valid for SOC <= 8% clay <= 60%. Functions were 
#' checked against equations available for download with SPAW model, 
#' downloadable at http://hrsl.arsusda.gov/SPAW/SPAWDownload.html.
#' @references 
#' Saxton, K.E. & Rawls, W.J. (2006) Soil water characteristic estimates by 
#' texture and organic matter for hydrologic solutions. Soil Sci Soc Am J, 70, 
#' 1569–1578.
#' @export
#' @examples 
#' soil_hydraulics(sand = 0.29, clay = 0.32, soc = 3.51, DF = 1, gravel = 0)
#' soil_hydraulics(sand = 0.29, clay = 0.32, soc = 3.51, DF = 0.8, gravel = 0)
#' soil_hydraulics(sand = 0.29, clay = 0.32, soc = 3.51, DF = 1, gravel = 0.2)
#' soil_hydraulics(sand = 0.29, clay = 0.32, soc = 3.51, DF = 0.8, gravel = 0.2)
soil_hydraulics <- function(sand, clay, soc, DF = 1, gravel = 0, digits = 4, 
                            PAW = TRUE) {
  if((sand > 1) | (clay > 1)) {
     stop("Sand & clay must be fractions, soc a percentage", call. = FALSE)
  }
  if((clay > 0.6) | soc > 8) {
      warning(paste("Validity of results questionable for sand fractions > 0.8", 
                    "or SOC percentage > 8"))
  }
   
  # pedotransfer functions
  wp <- wilt_point(sand, clay, soc) # Wilting point
  fc <- field_cap(sand, clay, soc) # Field capacity, no density effects
  fcdf <- field_cap_df(sand, clay, soc, DF) # Field capacity, w/density 
  thetas <- theta_s(sand, clay, soc) # Satured moisture content, no density
  thetasdf <- theta_sdf(sand, clay, soc, DF) # Satured moisture content, density 
  bd <- bdens(thetas, DF, gravel) # Bulk density
  ks <- ksat(sand, clay, soc, DF, gravel) # KSat, w/density and gravel
  
  # output
  out <- c("fc" = fcdf, "wp" = wp, "sat" = thetasdf, "bd" = bd, "ksat" = ks)
  if(PAW == TRUE) { 
    rodf <- ro_df(thetas, DF)
    gravel_pctv <- ((rodf / 2.65 ) * gravel) / (1 - gravel * ( 1 - rodf / 2.65))
    PAW <- (fcdf - wp) * (1 - gravel_pctv)
    out <- c(out, "PAW" = PAW)
  } 
  return(round(out, digits))
}  
