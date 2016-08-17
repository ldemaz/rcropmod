#' Classify soil texture using USDA texture triangle
#' @param tri.data Data frame containing the CLAY, SILT and SAND 
#' 'coordinates' of the texture data points to be classified. The data 
#' frame can contain more column than needed (ignored). The data 
#' frame must have column named CLAY, SILT and SAND (uppercase, 
#' the order has no importance) or named after the 'css.names' 
#' argument (alternative names). The sum of CLAY, SILT and SAND 
#' must be equal to 'text.sum' 
#' ('text.tol' determines the error tolerance).
#' @param PiC.type Single character string. If equal to 'n', then a table of 0, 
#' 1, 2 or 3 is outputed (0 if the sample does not belong to a class, 
#' 1 if it does, 2 if it lies on an edge and 3 if it lies on a 
#' vertex). Notice that the accuracy of the classification is 
#' not garanteed for samples lying very close to an edge, or right 
#' on it. See <http://www.mail-archive.com/r-help@r-project.org/msg96180.html>
#' @param css.names Vector of 3 character strings. Name of the columns in 
#' 'tri.data' that contains the CLAY SILT and SAND values, respectively. 
#' If NULL, default c("CLAY","SILT","SAND") value is assumed. Not to be 
#' confused with 'css.lab' that defines the labels of the  CLAY SILT and 
#' SAND axes in the plot.
#' @param text.sum Single numerical. Sum of the 3 particle size classes for each 
#' texture value (fixed). The real sum of the 3 particle size classes in 
#' 'tri.data' should be >= text.sum * (1-text.tol) OR  <= text.sum * 
#' (1+text.tol),  where 'text.tol' is an argument that can be changed. If some 
#' of the texture values don't match this requirement, an error 
#' occur (function fails) and TT.points.in.classes returns a of bad values with 
#' their actual particle size classes sum. You can 'normalise' you data 
#' table () prior to the use of TT.points.in.classes, by using the function 
#' TT.normalise.sum(), so all values match the 'text.sum' criteria. 
#' See also 'tri.sum.tst' that can be set to FALSE to avoid 
#' sum of particle size classes tests.
#' @param collapse Single character string. If PiC.type = "t" and a 
#' sample lie on the edge of 2 texture classes, then both will be outputed 
#' in a single character string, separated by 'collapse'. 
#' @return Vector of USDA texture classes (PiC.type = "t") or integer or logical
#' matrix showing which of all possible classes it fits within. 
#' @details This function is an adaptation/simplification of the 
#' TT.points.in.classes function from the soiltexture package, confined to 
#' classification using the USDA soil texture triangle. We incorporate it here 
#' to avoid the overhead of an X11 load (in Mac) that listing soiltexture as an
#' import causes. 
#' @references   Julien Moeys (2016). soiltexture: Functions for Soil Texture
#' Plot, Classification and Transformation. R package version 1.4.1. 
#' https://CRAN.R-project.org/package=soiltexture
#' @export
tt_class <- function(tri.data, PiC.type = "n", css.names = NULL, text.sum=100, 
                     collapse = NULL) { 
  data("usda_tt")
  if(is.null(collapse)) collapse = ", " 
  if(is.null(css.names)) css.names <- c("SAND", "SILT", "CLAY")
  classes.points.xy <- data.frame(
    "xpos" = usda_tt$"tt.points"[,"SILT"] * text.sum, 
    "ypos" = usda_tt$"tt.points"[,"CLAY"] * text.sum)   #
  data.points.xy <- data.frame("xpos" = tri.data[, css.names[2]], 
                               "ypos" = tri.data[, css.names[1]])   #

  # Vectorisable and custom wrapper for point.in.polygon():
  # X = class name
  points.in.class <- function(X, polygons.list = usda_tt$"tt.polygons", 
                              classes.points.xy = classes.points.xy, 
                              data.points.xy = data.points.xy) {  #
    sel.vec <- (polygons.list[[X]])$"points"
    xpol    <- classes.points.xy[sel.vec, "xpos" ]
    ypol    <- classes.points.xy[sel.vec, "ypos" ]
    #
    PiP.res <- point.in.polygon(point.x = data.points.xy$"xpos",  
                                point.y = data.points.xy$"ypos",  
                                pol.x = xpol, pol.y = ypol)# 
    return(PiP.res) 
  }
  classes.names <- names(usda_tt$"tt.polygons")
   
  PiP.res2 <- do.call("cbind", 
                      lapply(X = classes.names, FUN = points.in.class,
                             polygons.list = usda_tt$"tt.polygons",  
                             classes.points.xy = classes.points.xy, 
                             data.points.xy = data.points.xy))

  if(PiC.type %in% c("l","t")) {   #
    PiP.res2 <- t(apply(X = PiP.res2, MARGIN = 1, FUN = function(X) {
      as.logical(X)
    }))   #
  }   
  colnames(PiP.res2) <- classes.names
  #
  if(PiC.type == "t") {   #
    PiP.res2 <- unlist(apply(X = PiP.res2, MARGIN = 1, FUN = function(X) { 
       paste(names(X)[X], collapse = collapse)
    }))
  }   
  return(PiP.res2) 
}