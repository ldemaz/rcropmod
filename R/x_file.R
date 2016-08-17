#' Creates a sprintf statement
#' @param d input dataframe
#' @param tvar variable defining iterations
#' @param sprintf format string
#' @param vnames variable names in d (excluding tvar) to print
#' @keywords internal
#' @export
sprinter <- function(d, tvar, spvec, vnames) {
   do.call(rbind, lapply(unique(d[, tvar]), function(i) {
      ind <- which(d[, tvar] == i)[1]
      do.call(sprintf, c(list(spvec), d[ind, c(tvar, vnames)]))
   }))
}

#' Creates a DSSAT xfile
#'
#' @param xtab CSM parameter input table
#' @param outdir Output directory name
#' @param z Two digit code for end of X file, typically 01
#' @param xtype CSM xfile type (corresponding to particular crop)
#' @return A CSM experimental file, written to the location of choice
#' @details This allows multiple treatments to be tested, but only for a single
#' field. As such, it is more appropriate, for the time being, to run the model 
#' for individual crops, rather than in spatial mode (.GSX) with multiple crops.
#' @note As presently written (4/3/15), the simulation controls will not work
#' for multiple methods, as a you need a complete general and automatic section
#' replicated for each treatment
#' @export
x_file <- function(xtab, outdir, z, xtype) {
   
  # coerce to data.frame for now
  if(is.data.table(xtab)) xtab <- as.data.frame(xtab)  
   
  # Check if this is a multiple treatment table, establish single row input
  # to provide all unvarying parameters
  if(nrow(xtab) > 1) {
     xf <- xtab[1, ]
  } else {
     xf <- xtab
  }
  
  # Remove any soil horizons missing data
  hor <- colnames(xf)[grep("LLL|DUL|SLB", colnames(xf))]
  hornew <- hor[hor %in% hor[which(is.na(xf[, hor]))]]
  xtab <- xtab[, colnames(xtab)[!colnames(xtab) %in% hornew]]
  xf <- xf[, colnames(xf)[!colnames(xf) %in% hornew]]
  if(any(!ncol(xtab) %in% ncol(xf))) stop("input mismatch", call. = FALSE)

  # For soil initial conditions section
  ll_v <- colnames(xf)[grep("LLL", colnames(xf))]
  dul_v <- colnames(xf)[grep("DUL", colnames(xf))]
  slb_v <- colnames(xf)[grep("SLB", colnames(xf))]

  # sprint vectors
  spvecl <- list("TR" = paste0("%2s %s %s %s %-25s%3s%3s%3s%3s%3s%3s%3s%3s%3s",
                               "%3s%3s%3s%3s"),
                 "CU" = "%2s %s %s %s",
                 "F1" = "%2s %8s %8s %5s %5s %5s %5s %5s %5s %5s %5s %10s %s",
                 "F2" = "%2s %15s %15s %9s %17s %5s %5s %5s %5s %5s",
                 "IC" = paste0("%2s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s",
                               " %5s %5s %s"),
                 "PL" = paste0("%2s %5s %5s %5.1f %5.1f %5s %5s %5s %5s %5s",
                               " %5s %5s %5s %5s %5s"),
                 "FE" = "%2s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %7s",
                 "RE" = "%2s %5s %5s %5s %5.1f %5s %5s %5s %5s %5s %6s",
                 "EM" = "%2s %5s %5s %5s %5s %5s %5s %5s %5s %5s %-s",
                 "SC1" = "%2s %-11s %5s %5s %5s %5s %5s %-s",
                 "SC2" = "%2s %-11s %5s %5s %5s %5s %5s %5s %5s %5s %5s",
                 "SC3" = paste0("%2s %-11s %5s %5s %5s %5s %5s %5s %5s %5s %5s",
                                " %5s %5s"),
                 "SC4" = "%2s %-11s %5s %5s %5s %5s %5s",
                 "SC5" = paste0("%2s %-11s %5s %5s %5s %5s %5s %5s %5s %5s %5s",
                                " %5s %5s %5s %5s"),
                 "a1" = "%2s %-11s %5s %5s %5s %5s %5s %5s %5s",
                 "a2" = "%2s %-11s %5s %5s %5s %5s %5s %5s %5s",
                 "a3" = "%2s %-11s %5s %5s %5s %5s %5s",
                 "a4" = "%2s %-11s %5s %5s %5s",
                 "a5" = "%2s %-11s %5s %5s %5s %5s")

  vnamel <- list("TR" = c("N", "R", "O", "C", "TNAME", "CU", "atL", "SA",
                          "IC", "MP", "MI", "MF", "MR", "MC", "MT", "ME", "MH",
                          "SM"),
                 "CU" = c("CLNUM", "CR", "INGENO", "CNAME"),
                 "F1" = c("atL", "ID_FIELD", "WSTA", "FLSA", "FLOB", "FLDT",
                          "FLDD", "FLDS", "FLST", "SLTX", "SLDP", "prof",
                          "FLNAME"),
                 "F2" = c("N", "XCRD", "YCRD", "ELEV", "AREA", "SLEN", "FLWR",
                          "SLAS", "FLHST", "FHDUR"),
                 "IC" = c("atF", "PCR", "ICDAT", "ICRT", "ICND", "ICRN", "ICRE",
                          "ICWD", "ICRES", "ICREN", "ICREP", "ICRIP", "ICRID",
                          "prof"),
                 "PL" = c("atP", "PDATE", "EDATE", "PPOP", "PPOE", "PLME",
                          "PLDS", "PLRS", "PLRD","PLDP", "PLWT", "PAGE", "PENV",
                          "PLPH", "SPRL"),
                 "FE" = c("atF", "FDATE", "FMCD", "FACD", "FDEP", "FAMN",
                          "FAMP", "FAMK", "FAMC", "FAMO", "FOCD", "FERNAME"),
                 "RE" = c("atR", "RDATE", "RCOD", "RAMT", "RESN", "RESP",
                          "RESK", "RINP", "RDEP", "RMET", "RENAME"),
                 "EM" = c("atE", "ODATE", "EDAY", "ERAD", "EMAX", "EMIN",
                          "ERAIN", "ECO2", "EDEW", "EWIND", "ENVNAME"),
                 "SC1" = c("atN", "GENERAL", "NYERS", "NREPS", "START", "SDATE",
                           "RSEED", "SNAME"),
                 "SC2" = c("atN", "OPTIONS", "WATER", "NITRO", "SYMBI", "PHOSP",
                           "POTAS", "DISES", "CHEM", "TILL", "CO2"),
                 "SC3" = c("atN", "METHODS", "WTHER", "INCON", "LIGHT", "EVAPO",
                           "INFIL", "PHOTO", "HYDRO", "NSWIT", "MESOM", "MESEV",
                           "MESOL"),
                 "SC4" = c("atN", "MANAGEMENT", "PLANT", "IRRIG", "FERTI",
                           "RESID", "HARVS"),
                 "SC5" = c("atN", "OUTPUTS", "FNAME", "OVVEW", "SUMRY", "FROPT",
                           "GROUT", "CAOUT", "WAOUT", "NIOUT", "MIOUT", "N",
                           "VBOSE", "CHOUT", "OPOUT"),
                 "a1" = c("atN", "PLANTING", "PFRST", "PLAST", "PH20L", "PH20U",
                          "PH20D", "PSTMX", "PSTMIN"),
                 "a2" = c("atN", "IRRIGATION", "IMDEP", "ITHRL", "ITHRU",
                          "IROFF", "IMETH", "IRAMT", "IREFF"),
                 "a3" = c("atN", "NITROGEN", "NMDEP", "NMTHR", "NAMNT", "NCODE",
                          "NAOFF"),
                 "a4" = c("atN", "RESIDUES", "RIPCN", "RTIME", "RIDEP"),
                 "a5" = c("atN", "HARVEST", "HFRST", "HLAST", "HPCNP", "HPCNR"))

  f_tab_out <- rbind(

    # Xfile header
    paste0("*EXP.DETAILs:", xf$CR, ":", xf$XNAME, ":", substr(xf$SDATE, 1, 2),
          ":", z),  # might be replaced by variable if making more than 1 vers
    c(""),
    paste0("*TREATMENTS",
           "                        -------------FACTOR LEVELS------------"),
    paste0("@N R O C TNAME....................",
           " CU FL SA IC MP MI MF MR MC MT ME MH SM"),
    sprinter(xtab, vnamel$TR[1], spvecl$TR, vnamel$TR[-1]),

    # Cultivar section
    c(""),
    c("*CULTIVARS"),
    c("@C CR INGENO CNAME"),
    sprinter(xtab, vnamel$CU[1], spvecl$CU, vnamel$CU[-1]),

    # Fields section
    c(""),
    c("*FIELDS"),
    #c("@L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST  SLTX  SLDP ID_SOIL    FLNAME"),
    paste0("@L ID_FIELD WSTA....  FLSA  FLOB",
           "  FLDT  FLDD  FLDS  FLST  SLTX  SLDP ID_SOIL    FLNAME"),
    sprinter(xf, vnamel$F1[1], spvecl$F1, vnamel$F1[-1]),

    # Field additional information section
    #c("@L ...........XCRD ...........YCRD .....ELEV .............AREA .SLEN .FLWR .SLAS FLHST FHDUR"),
    paste0("@L ...........XCRD ...........YCRD .....ELEV .............AREA",
           " .SLEN .FLWR .SLAS FLHST FHDUR"),
    sprinter(xf, vnamel$F2[1], spvecl$F2, vnamel$F2[-1]),

    #Initial conditions section - new to this version
    c(""),
    c("*INITIAL CONDITIONS"),
    #c("@C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID ICNAME"),
    paste0("@C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP",
           " ICRIP ICRID ICNAME"),
    #sprintf("%2s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %s",
    #        xf$atF, xf$PCR, xf$ICDAT, xf$ICRT, xf$ICND, xf$ICRN, xf$ICRE,
    #        xf$ICWD, xf$ICRES, xf$ICREN, xf$ICREP, xf$ICRIP, xf$ICRID, xf$prof),
    sprinter(xf, vnamel$IC[1], spvecl$IC, vnamel$IC[-1]),
    c("@C  ICBL  SH2O  SNH4  SNO3"),
    do.call(rbind, lapply(1:length(slb_v), function(q) {  # q <- 1
      sprintf("%2s %5i %5.3f %5.1f %5.1f",
              xf$atF, xf[, slb_v[q]],
              (xf[, dul_v[q]] - xf[, ll_v[q]]) * xf$H20_s + xf[, ll_v[q]],
              xf$SNH4, xf$SNO3)
    })),

    # Planting section
    c(""),
    c("*PLANTING DETAILS"),
    #c("@P PDATE EDATE  PPOP  PPOE  PLME  PLDS  PLRS  PLRD  PLDP  PLWT  PAGE  PENV  PLPH  SPRL"),
    paste0("@P PDATE EDATE  PPOP  PPOE  PLME  PLDS  PLRS  PLRD  PLDP  PLWT",
           "  PAGE  PENV  PLPH  SPRL"),
    sprinter(xtab, vnamel$PL[1], spvecl$PL, vnamel$PL[-1]),

    # Fertilizer section
    c(""),
    c("*FERTILIZERS (INORGANIC)"),
    c("@F FDATE  FMCD  FACD  FDEP  FAMN  FAMP  FAMK  FAMC  FAMO  FOCD FERNAME"),
    sprinter(xtab, vnamel$FE[1], spvecl$FE, vnamel$FE[-1]),

    # Residues section
    c(""),
    c("*RESIDUES AND ORGANIC FERTILIZER"),
    c("@R RDATE  RCOD  RAMT  RESN  RESP  RESK  RINP  RDEP  RMET RENAME"),
    sprinter(xtab, vnamel$RE[1], spvecl$RE, vnamel$RE[-1]),

    # Environmental modifications section
    c(""),
    c("*ENVIRONMENT MODIFICATIONS"),
    c("@E ODATE EDAY  ERAD  EMAX  EMIN  ERAIN ECO2  EDEW  EWIND ENVNAME"),
    sprinter(xtab, vnamel$EM[1], spvecl$EM, vnamel$EM[-1]),

    ############ Simulation controls section ################
    c(""),
    c("*SIMULATION CONTROLS"),

    # SC 1
    #c("@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL"),
    paste0("@N GENERAL     NYERS NREPS START SDATE RSEED",
           " SNAME.................... SMODEL"),
    sprinter(xtab, vnamel$SC1[1], spvecl$SC1, vnamel$SC1[-1]),

    # SC2.
    c("@N OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES  CHEM  TILL   CO2"),
    sprinter(xtab, vnamel$SC2[1], spvecl$SC2, vnamel$SC2[-1]),

    # SC3.
    #c("@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT MESOM MESEV MESOL"),
    paste0("@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT",
           " MESOM MESEV MESOL"),
    sprinter(xtab, vnamel$SC3[1], spvecl$SC3, vnamel$SC3[-1]),

    # SC4
    c("@N MANAGEMENT  PLANT IRRIG FERTI RESID HARVS"),
    sprinter(xtab, vnamel$SC4[1], spvecl$SC4, vnamel$SC4[-1]),

    # SC5. 5/5/11 - new switches added in from csm.xTab.R.
    #c("@N OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT MIOUT DIOUT VBOSE CHOUT OPOUT"),
    paste0("@N OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT",
           " MIOUT DIOUT VBOSE CHOUT OPOUT"),
    sprinter(xtab, vnamel$SC5[1], spvecl$SC5, vnamel$SC5[-1]),

    ################Automatic management section##################
    # The only section that currently can vary is the planting section, for which the switch in
    # SC4 for planting has to be thrown to 'A' for these values to apply.
    c(""),
    c("@  AUTOMATIC MANAGEMENT"),
    c("@N PLANTING    PFRST PLAST PH2OL PH2OU PH2OD PSTMX PSTMN"),
    sprinter(xtab, vnamel$a1[1], spvecl$a1, vnamel$a1[-1]),

    c("@N IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT IREFF"),
    sprinter(xtab, vnamel$a2[1], spvecl$a2, vnamel$a2[-1]),

    c("@N NITROGEN    NMDEP NMTHR NAMNT NCODE NAOFF"),
    sprinter(xtab, vnamel$a3[1], spvecl$a3, vnamel$a3[-1]),

    c("@N RESIDUES    RIPCN RTIME RIDEP"),
    sprinter(xtab, vnamel$a4[1], spvecl$a4, vnamel$a4[-1]),

    c("@N HARVEST     HFRST HLAST HPCNP HPCNR"),
    sprinter(xtab, vnamel$a5[1], spvecl$a5, vnamel$a5[-1]))

  # X File output name
  xname <- sprintf("%4s%2s%02s%4s", xf$XNAME, substr(xf$SDATE, 1, 2), z, xtype)

  # Write out X file
  write(f_tab_out, fp(outdir, xname), append = FALSE)

  # Create and write out batch file - deleted this version 25/5/11
  return(xname)
}  # End function
