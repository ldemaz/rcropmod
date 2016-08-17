#' Read in output file from DSSAT CSM
#' @param rundir CSM where model should execute (e.g. Maize)
#' @param type Specify which CSM output file type. Default is "summary"
#' @param vars A vector of specific variables from the file of interest 
#' @return data.table containing year, day, and specified variables
#' @details This is an experimental readChar based variant of read_csm_outfile, 
#' designed for reading in DSSAT .OUT files, particularly Summary.OUT files, but 
#' theoretically including (although not yet tested on) other, e.g. ET.OUT.  
#' The vars variable should be provide the exact variable names of interest. 
#' E.g. "EOAC and EOAA" from ET.OUT. 
#' @export
read_csm_outfile2 <- function(rundir, type = "summary", vars) {
  pat <- paste(vars, collapse = "|")
  drop.col  <- "@"  # To get rid of leading @ in @RUNNO
  if(type == "summary") {
    skip <- 3
    x <- fp(rundir, "Summary.OUT")
  }
  sep <- ""  # Blank space separator
  headname <- scan(x, what = "character", sep = sep, skip = skip, nlines = 1, 
                   na.strings = drop.col, strip.white = TRUE, quiet = TRUE)  
  
  # Corrects potential mismatches between column headers and table body
  # Test for columns having defined drop.col variable
  test.na <- ifelse(is.na(headname) == "TRUE", 1, 0)

  # Define a vector of NULL values with length of "header"
  header_vec <- rep("NULL", length(headname))
  header_vec[grep(pat, headname, perl = TRUE)] <- NA
  
  # read in data lines, select columns, return data.table
  skip2 <- skip + 2
  dat <- lmisc::read_lines2(x)
  dat <- dat[skip2:length(dat)]
  colind <- which(is.na(header_vec))
  dat <- strsplit(dat, split = "\\s+")
  dat <- do.call(rbind.data.frame, lapply(dat, function(x) x[colind]))
  colnames(dat) <- headname[is.na(header_vec)]
  dat <- as.data.table(dat)
  return(dat)
}
