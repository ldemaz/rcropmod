#' A 30 year weather dataset for a single location in Zambia
#'
#' A dataset containing daily weather data for a location in southern Zambia. 
#' The variables are as follows:
#'
#' @format A 3 element list consisting of two 2-element vectors and a data.table
#' with 11684 rows and 6 variables:
#' \itemize{
#'   \item dates: start and end dates of the weather data.
#'   \item xyz: latitude, longitude, and elevation of the location.
#'   \item dat: weather dataset
#'   \itemize{
#'     \item prec: precipitation (mm)
#'     \item tmax: maximum temperature (degress Celsius)
#'     \item tmin: minimum temperature (degress Celsius)
#'     \item sw: shortwave radiation (MJ/m^2/day) 
#'     \item wind: windspeed (km/day)
#'     \item pres: air pressure (kPa)
#'     \item sh: specific humidity (kg/kg)
#'   }
#' }
#' @references 
#' Chaney, N.W., Sheffield, J., Villarini, G. & Wood, E.F. (2014)
#' Spatial Analysis of Trends in Climatic Extremes with a High Resolution 
#' Gridded Daily Meteorological Data Set over Sub-Saharan Africa. Journal of 
#' Climate, 27, 5815-5835.
#' 
#' Estes, L.D., Chaney, N.W., Herrera-Estrada, J., Sheffield, J., Caylor, K.K. 
#' & Wood, E.F. (2014) Changing water availability during the African 
#' maize-growing season, 1979–2010. Environmental Research Letters, 9, 075005.
"weathdat"

#' Weather data for gridding demonstration
#'
#' A dataset containing gridded daily weather data (2005-2010) over Namwala 
#' District in Zambia, as well as a shapefile and reference grid containing cell
#' IDs.  
#' The variables are as follows:
#'
#' @format A 22 element list consisting of 1 SpatialPolygonsDataFrame, 
#' a raster (20 cells), and 20 data.tables each containing 2191 rows and 4 
#' variables:
#' \itemize{
#'   \item namwala: SpatialPolygonsDataFrame showing district shape (EPSG 4326)
#'   \item wthgrid: RasterLayer subset from 0.25 degree grid, providing cell IDs
#'   \item C1960-1964, C2017-2021, C2074-2078, C2131-2135: data.tables of daily
#'   weather containing
#'   \itemize{
#'     \item sw: shortwave radiation (MJ/m^2/day) 
#'     \item tmax: maximum temperature (degress Celsius)
#'     \item tmin: minimum temperature (degress Celsius)
#'     \item prec: precipitation (mm)
#'   }
#' }
#' @references 
#' Chaney, N.W., Sheffield, J., Villarini, G. & Wood, E.F. (2014)
#' Spatial Analysis of Trends in Climatic Extremes with a High Resolution 
#' Gridded Daily Meteorological Data Set over Sub-Saharan Africa. Journal of 
#' Climate, 27, 5815-5835.
#' 
#' Estes, L.D., Chaney, N.W., Herrera-Estrada, J., Sheffield, J., Caylor, K.K. 
#' & Wood, E.F. (2014) Changing water availability during the African 
#' maize-growing season, 1979–2010. Environmental Research Letters, 9, 075005.
"weathergrid"