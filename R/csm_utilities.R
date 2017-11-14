#' DSSAT45 execution command
#' @param projdir Your working directory
#' @param csmdir Master directory for DSSAT model
#' @param rundir CSM where model should execute (e.g. Maize)
#' @param btype Batch mode code ("B" normal batch (default), "S" spatial, etc)
#' @param csm CSM executable name, defaults to DSCSM046.EXE
#' @param bname Batch file name, default is R_DSSBatch.v45
#' @return File_path for DSSAT
#' @export 
exec_csm <- function(projdir, csmdir, rundir, btype = "B", 
                     csm = "DSCSM046.EXE", 
                     bname = "R_DSSBatch.v45") {
   bcmd <- sprintf("%s%s %s %s", csmdir, "/", csm, btype, bname)
   setwd(rundir)
   system(bcmd, ignore.stdout = TRUE)
   setwd(projdir)
}

#' Create field ids
#' @param wth vector of weather file name (e.g. ZASP7932.WTH)
#' @keywords internal
#' @export
fid <- function(wth) {
  fid <- sprintf("%s%04d", substr(wth, 1, 4), 1:length(wth))
  return(fid)
}

#' X file unique identifiers
#' @param wth vector of weather file name (e.g. ZASP7932.WTH)
#' @param n length of vector
#' @details Designed for data.table with use of BY statements
#' @keywords internal
#' @export
xname <- function(wth, n) {
   combis <- expand.grid(data.frame(cbind(LETTERS, LETTERS, LETTERS, LETTERS)))
   combis <- paste0(combis[, 4], combis[, 3], combis[, 2], combis[, 1])
   xname <- combis[1:n]
   return(xname)
}

#' Create unique four character DSSAT WTH file identifier
#' @param n Number of names needed
#' @param i index number of first prefix in subset (e.g. 1 corresponds to AAAA) 
#' @keywords internal
#' @export
wthname <- function(n, i) {
   data(wthids)
   # letgrid <- do.call(cbind, lapply(1:4, function(x) LETTERS))
   # letgrid <- data.table(expand.grid(data.table(letgrid)))
   # wthids <- letgrid[, unique(paste0(V1, V2, V3, V4))]
   wthv <- wthids[i:(i + n - 1)]
   return(wthv)
}

#' Creates combination CSM X file treatment table 
#' @param tvars Vector of variable names in treatments, e.g. c("INGENO, "PDATE")
#' @param topts Vector of corresponding factor level names, e.g. c("CU", "MP") 
#' @param ttab Table of treatment permutations
#' @keywords internal
#' @export
t_tab <- function(tvars, topts, ttab) {
  tprs <- cbind(c("CU", "MP", "MF"), 
                c("CLNUM", "atP", "atF"))
  opt_df <- do.call(cbind, lapply(1:length(tvars), function(x) {
    cind <- which(colnames(ttab) == tvars[x])
    v <- unique(ttab[, cind])
    df <- do.call(rbind, lapply(1:length(v), function(y) {
      d <- cbind(which(ttab[, cind] == v[y]), y, y)
    }))
    df[order(df[, 1]), 2:3]
  }))
  cn <- unlist(lapply(topts, function(x) c(x, tprs[match(x, tprs[, 1]), 2]))) 
  colnames(opt_df) <- cn
  return(opt_df)
}
