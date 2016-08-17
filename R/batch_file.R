#' Create a batch file for one or more CSM X files
#' @param xfiles Vector of x file names
#' @param outdir Output directory name
#' @param btype What sort of batch file, e.g. "MAIZE"
#' @param bname Optional batch file name--.v45 suffix is automatically appended 
#' @param RP default 1. A CSM parameter
#' @param SQ default 0. For sequential runs 
#' @param OP default 0 
#' @param CO default 0
#' @return .v45 batch file in selected directory
#' @note Batch files are hard-coded to spatial now.  Needs to change
#' @export
batch_file <- function(xl, xfiles, outdir, btype, bname = "R_DSSBatch", RP = 1, 
                       SQ = 0, OP = 0, CO = 0) {
  outname <- paste0(bname, ".v45")
  #i <- 1
  #xfiles <- unlist(xfiles)
  header <- rbind(paste0("$BATCH(", btype, ")"), "!", 
                  sprintf("%6s %92s %6s %6s %6s %6s", "@FILEX", "TRTNO",
                          "RP", "SQ", "OP", "CO"))
  guts <- do.call(rbind, lapply(1:length(xl), function(i) {
     trt <- xl[[i]]$N  # pull out treatment numbers
     cbind(sprintf("%6s %86s %6i %6i %6i %6i", xfiles[i], trt, RP, SQ, OP, CO))
  }))  
  outbatch <- rbind(header, guts)
  #outbatch <- rbind(rbind(paste0("$BATCH(", btype, ")"), "!", 
  #                       sprintf("%6s %92s %6s %6s %6s %6s", "@FILEX", "TRTNO",
  #                                "RP", "SQ", "OP", "CO")),
  #                          cbind(sprintf("%6s %86s %6i %6i %6i %6i", xfiles, 1, 
  #                                      RP, SQ, OP, CO))) 
  
  # Write the batch file to the selected folder  
  write(outbatch, lmisc::full_path(outdir, outname), append = FALSE)
  return(outname)
}