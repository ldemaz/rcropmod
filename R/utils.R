# general utilities

#' Move a single column within a data.table
#' @param colname Name or index of column to move
#' @param newpos Column name to left of new location, or index of new location.
#' @param DT data.table to operate on
#' @return data.table DT with column order changed by reference
#' @details This is a wrapper function for data.table's setcolorder function, 
#' written to avoid having to change an entire vector of column names. 
#' @examples 
#' DT <- data.table(a = 1:10, b = 1:10, c = 11:20, d = 26:35)
#' colinsert("c", "a", DT)
#' DT <- data.table(a = 1:10, b = 1:10, c = 11:20, d = 26:35)
#' colinsert(3, 1, DT)
#' DT <- data.table(a = 1:10, b = 1:10, c = 11:20, d = 26:35)
#' colinsert(4, 1, DT)
#' DT <- data.table(a = 1:10, b = 1:10, c = 11:20, d = 26:35)
#' colinsert(4, 3, DT)
#' DT <- data.table(a = 1:10, b = 1:10, c = 11:20, d = 26:35)
#' colinsert("d", "b", DT)
#' DT <- data.table(a = 1:10, b = 1:10, c = 11:20, d = 26:35)
#' colinsert("d", 3, DT)
#' @export
colinsert <- function(colname, newpos, DT) {
  nms <- names(DT)
  if(is.character(newpos)) newind <- which(nms == newpos) + 1
  if(is.numeric(newpos)) newind <- newpos
  if(is.character(colname)) oldind <- which(nms == colname)
  if(is.numeric(colname)) oldind <- colname
  
  # create swap index
  nmsind <- 1:length(nms)
  nmsind1 <- nmsind[0:(newind - 1)]
  if(oldind %in% nmsind1) nmsind1 <- nmsind1[!nmsind1 == oldind]
  nmsind2 <- nmsind[newind:length(nmsind)]
  if(oldind %in% nmsind2) nmsind2 <- nmsind2[!nmsind2 == oldind]
  
  # swap
  setcolorder(DT, nms[c(nmsind1, oldind, nmsind2)])
  return(DT)
}

#' Easier joining of file paths and file names for reading in files
#' 
#' @param path Path directory
#' @param fname File name
#' @return Complete file string
#' @details Function imported from lmisc, to avoid need for importing
#' @keywords internal
#' @export
fp <- function(path, fname) {
   pend <- substr(path, nchar(path), nchar(path))
   if(pend != .Platform$file.sep) {
      pout <- paste(path, .Platform$file.sep, fname, sep = "") 
   } else { 
      pout <- paste(path, fname, sep = "")
   }
   pout
}

#' Faster version of readLines
#' @param fname File name to read
#' @return Vector of strings from input file
#' @author mlt
#' @details Also appears in lmisc. Added here to minimize dependencies. 
#' @references http://www.r-bloggers.com/faster-files-in-r/
#' @keywords internal
#' @export
read_lines2 <- function(fname) {
   s <- file.info(fname)$size 
   buf <- readChar(fname, s, useBytes = TRUE)
   strsplit(buf,"\n", fixed = TRUE, useBytes = TRUE)[[1]]
}
