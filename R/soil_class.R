#' Returns soil texture class, based on USDA triangle
#' @param clay Vector of percent clay
#' @param sand Vector of percent silt
#' @param silt Vector of percent sand
#' @return USDA texture class
#' @details This function draws on tt_class, and adaptation of the 
#' TT.points.in.classes from the soiltexture package. That function can classify
#' borderline soiltexture cases. This function returns just the first of several
#' classifications made. 
#' @references Julien Moeys (2016). soiltexture: Functions for Soil Texture
#' Plot, Classification and Transformation. R package version 1.4.1. 
#' https://CRAN.R-project.org/package=soiltexture
#' @examples 
#' soil_class(clay = c(05,60,15,05,25,05,25,45,65,75,13,47),
#'            sand = c(90,32,70,70,20,10,10,10,20,10,70,10))
#' @export
soil_class <- function(clay, sand, silt = NULL) {
  if(is.null(clay)) stop("Clay percentage is required", call. = FALSE)
  if(is.null(sand) & is.null(silt)) {
    stop("You must provide at either sand or silt percentages (or both)", 
         call. = FALSE)
  }
  if(is.null(sand) & !is.null(silt)) sand <- 100 - (clay + silt)
  if(is.null(silt)) silt <- 100 - (clay + sand)
  tri.data <- cbind.data.frame("CLAY" = clay, "SILT" = silt, "SAND" = sand)   
  if(any(round(rowSums(tri.data), 1) != 100)) stop("Textures don't add up")
  texclass <- tt_class(tri.data = tri.data, PiC.type = "n")
  ind <- which(texclass > 0)
  tclass <- apply(texclass, 1, function(x) colnames(texclass)[which(x > 0)[1]])
  return(tclass)
}