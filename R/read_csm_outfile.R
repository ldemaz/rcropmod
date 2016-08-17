#' Read in output file from DSSAT CSM
#' @param rundir CSM where model should execute (e.g. Maize)
#' @param type Specify which CSM output file type. Default is "summary"
#' @param vars A vector of specific variables from the file of interest 
#' @return Numeric matrix containing year, day, and specified variables
#' @details This is a read.table based variant of read_csm_outfile, designed for 
#' reading in DSSAT .OUT files, particularly Summary.OUT files, but 
#' theoretically including (although not yet tested on) other, e.g. ET.OUT.  
#' The vars variable should be provide the exact variable names of interest. 
#' E.g. "EOAC and EOAA" from ET.OUT. 
#' @note This is only set up to read in the Summary.OUT file at present 
#' @export
read_csm_outfile <- function(rundir, type = "summary", vars) {
  pat <- paste(vars, collapse = "|")
  drop.col  <- "@"  # To get rid of leading @ in @RUNNO
  if(type == "summary") {
    skip <- 3
    x <- lmisc::fp(rundir, "Summary.OUT")
  }
  sep <- ""  # Blank space separator
  headname <- scan(x, what = "character", sep = sep, skip = skip, nlines = 1, 
                   na.strings = drop.col, strip.white = TRUE, quiet = TRUE)  
  
  # Corrects potential mismatches between column headers and table body
  # Test for columns having defined drop.col variable
  test.na <- ifelse(is.na(headname) == "TRUE", 1, 0)

  # Resize header column if drop.col variable is present
  if(sum(test.na) > 0) {  
    headname <- headname[!is.na(headname)]  
  }    
  
  # Define a vector of NULL values with length of "header"
  header_vec <- rep("NULL", length(headname))
  header_vec[grep(pat, headname, perl = TRUE)] <- 1
  
  # Assign NA to positions where column names match search pattern
  header_vec[grep(pat, headname, perl = TRUE)] <- NA
  sk2 <- skip + 1  # Skip variable for table body

  # Read-in table
  dat <- read.table(x, header = FALSE, sep = sep, colClasses = header_vec,
                    skip = sk2, fill = TRUE)
  colnames(dat) <- sub("@", "", headname[is.na(header_vec)])

  # Create marker for end of season in new column
#   dat <- cbind(dat, "SEASON" = c(1:nrow(dat) * 0))
#   
#   # Labels the rows containing the last record for a given growing season with the year in which that season
#   # ended (finds the gap between years, and then puts the value in the row above that--if it's the last season
#   # it is determined by the total number of rows in the table)
#   dat[row(dat)[, 1] %in% c(grep("DSSAT", dat[, 1], perl = T) - 1, nrow(dat)), ncol(dat)] <- 
#     as.numeric(as.character(dat[c(grep("DSSAT", dat[, 1], perl = T) - 1, nrow(dat)), 1]))
#   
#   # Remove gaps between seasons
#   dat <- dat[!rownames(dat) %in% grep("RUN|!|@|DSSAT|MODEL|EXP|DATA|TREAT", dat[, 1], perl = T), ]
#   
#   dat <- apply(dat, 2, function(x) as.numeric(as.character(x)))
#   rownames(dat) <- 1:nrow(dat)
#   
#   un.yrs <- unique(dat[dat[, ncol(dat)] > 0, ncol(dat)]) 
#   for(i in 1:length(un.yrs)) {
#     if(i == 1) j <- 1
#     if(i > 1) {
#       yind <- row(dat)[dat[, ncol(dat)] %in% un.yrs[i - 1], 1]  # vector of rows w/previous season year
#       j <- yind[length(yind)] + 1  # Row index for current year
#     }
#     k <- match(un.yrs[i], dat[, ncol(dat)])
#     yr <- dat[match(un.yrs[i], dat[, ncol(dat)]), ncol(dat)]
#     dat[j:k, ncol(dat)] <- yr
#   }
  dat <- as.data.table(dat)
  return(dat)
}
