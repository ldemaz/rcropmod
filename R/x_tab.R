#' Creates input table of DSSAT CSM model parameters
#' @description Creates input table of variables to be passed to CSM xfile
#' writing function, joining them on to master input table
#' @param fields data.frame/data.table with minimum variable set (see Details).
#' @return Table of management variables joined to master table defining fields
#' @details Function sets up a table containing all variables necessary to 
#' create a DSSAT X file. The minimum variables needed are: 
#' \itemize{
#'   \item ID_FIELD: field id (created by \code{\link{fid}})
#'   \item WTH: weather station id
#'   \item prof: soil profile id
#'   \item PDATE: Planting date (in YYDOY).
#'   \item XNAME: 4-digit X file identifier (created by \code{\link{xname}})
#'   \item SLB1-10: 10 columns for soil depths 
#'   \item SLLL1-10: 10 columns for wilting point values
#'   \item SDUL1-10: 10 columns for drained upper limit values
#' }
#' The last three sets of variables usually only have about 4-5 values, the rest
#' are NA. 10 columns are provided for the rare soil profile that has this many 
#' layers. These values are obtained by running \code{\link{read_sol_hor}} or 
#' \code{\link{read_sol_prof}} functions. Default values are provided for any 
#' variable beyond those required that are not specified. Note that the default
#' crop is maize, so if you don't specify a crop type in inputs that is what the
#' model will run. Additional variables specified in \emph{fields} will 
#' over-ride default values. Variable names must be exact or function will fail.
#' Other default settings worth knowing about: 
#' \itemize{
#'   \item SDATE, ICDAT, ODATE, RDATE are all assumed to equal PDATE, unless 
#'   otherwise specified.
#'   \item PFRST, PLAST For automatic planting, the planting window defaults to 
#'   opening 14 days before PDATE and closes 21 days afterwards. If variable
#'   planting dates is one of the treatments, this option should not be used. 
#'   For this reason, automatic planting is turned off by default. 
#'   \item FDATE (fertilization date) is assumed to be on day of planting. 
#' }
#' @export
x_tab <- function(fields) {

    # Define static CSM variables
    cnames <- c("VALUE", "SA", "ME",   
                "CR", "CLNUM", "INGENO", "CNAME", "FLHST", "FHDUR",    
                "PCR", "ICRT", "ICND", "ICRN", "ICRE", "ICWD", "ICRES", "ICREN",
                "ICREP", "ICRIP", "ICRID", "SNH4", "SNO3", "H20_s",     
                "PLDP", "FDATE", "FDEP", "FAMN", "FAMP", "FAMK", "ERAD", "EMAX",     
                "EMIN", "ERAIN", "ECO2", "RCOD", "RAMT", "RESN", "RDEP", 
                "PHOSP", "TILL", "CO2", "PLANT", "SUMRY", "FROPT",  "PH20L",
                "PH20U", "PH20D", "PSTMX", "PSTMIN", "rf50", "ICDAT", "PDATE",
                "PPOP", "PPOE", "PLRS", "RDATE", "ODATE", "NYERS", "SDATE", 
                "PFRST", "PLAST")    
    
    # name check
    minnames <- c("ID_FIELD", "WTH", "prof", "XNAME", "PDATE", 
                  paste0("SLB", 1:10), paste0("SLLL", 1:10), 
                  paste0("SDUL", 1:10))
    if(any(!minnames %in% names(fields))) {
       stop("Missing minimum values needed to create X files", call. = FALSE)
    }
    
    # data.table check
    if(!is.data.table(fields)) fields <- as.data.table(fields)

    ctab <- data.frame(matrix(nrow = nrow(fields), ncol = length(cnames)))    
    colnames(ctab) <- cnames         
  
    ###

    # Treatment table options
    ctab$N       <- 1               # @N - Treatment number 
    ctab$R       <- 1               # R - Something to do with sequence 
    ctab$O       <- 1               # O - As above
    ctab$C       <- 0               # C  
    ctab$TNAME   <- fields$ID_FIELD # Treatment name
    ctab$CU      <- 1               # Cultivar treatment switch
    ctab$FL      <- 1               # Field treatment switch
    ctab$SA      <- 0               # Soil analysis treatment switch (1 or 0)
    ctab$IC      <- 1               # IC - Initial conditions
    ctab$MP      <- 1               # MP - planting method
    ctab$MI      <- 0               # MI - irrigation
    ctab$MF      <- 1               # MF - Fertilization method
    ctab$MR      <- 0               # MR - Residue method
    ctab$MC      <- 0               # MC - Chemical applications
    ctab$MT      <- 0               # MT - Tillage
    ctab$ME      <- 0               # ME - Environmental modification 
    ctab$MH      <- 0               # MH - Harvest methods 
    ctab$SM      <- 1               # SM - simulation method  
    
    # Cultivar section
    ctab$CLNUM   <- 1         # Change cultivar number to 1 for now     
    ctab$CR      <- "MZ"       # Crop type 
    ctab$CNAME   <- "CULT"
    ctab$INGENO  <- "990002"   # Cultivars - generic DSSAT medium season 
    
    # Fields
    ctab$atL     <- 1          # @L 
    ctab$ID_FIELD <- fields$ID_FIELD  # Field id
    ctab$WSTA    <- fields$WTH
    ctab$FLSA    <- -99 
    ctab$FLOB    <- -99   
    ctab$FLDT    <- -99   
    ctab$FLDD    <- -99   
    ctab$FLDS    <- -99   
    ctab$FLST    <- -99   
    ctab$SLTX    <- -99   
    ctab$SLDP    <- -99  
    ctab$ID_SOIL <- fields$prof    
    ctab$FLNAME  <- sprintf("%s%04d", "F", ctab$N)
    
    # Fields additional information section
    ctab$FLHST   <- "FH202"   
    # !!! 1. FH10[1-2], 2. FH20[1-2], 3. FH301
    #   1. Assumes cultivated, good vs bad mgmnt (1 or 2 suffix)
    #   2. Initial grass or forest, good or bad management
    #   3. Assumes prior degraded, good management
    ctab$FHDUR   <- "20"      # !!! Field history duration, 0, 5, 10, 20, 60
    ctab$XCRD    <- -99
    ctab$YCRD    <- -99
    ctab$ELEV    <- -99
    ctab$AREA    <- -99 
    ctab$SLEN    <- -99
    ctab$FLWR    <- -99 
    ctab$SLAS    <- -99 
       
    # Initial conditions section    
    # Residues    
    ctab$PCR     <- "MZ"      # Previous crop (set to maize for now)
    ctab$ICDAT   <- fields$PDATE  #  Date
    ctab$ICRT    <- "-99"     #  Root weight previous crop    
    ctab$ICND    <- "-99"     #  Nodule weight previous crop    
    ctab$ICRN    <- 1         #  Rhizobia number    
    ctab$ICRE    <- 1         #  Rhizobia effectiveness    
    ctab$ICWD    <- "-99"     #  Water table depth    
    ctab$ICRES   <- "-99"     #  Crop residue kg/ha    
    ctab$ICREN   <- "-99"     #  % N residue    
    ctab$ICREP   <- "-99"     #  % P residue    
    ctab$ICRIP   <- "-99"     #  % Incorporation    
    ctab$ICRID   <- "-99"     #  Incorporation depth (cm)    
    # Profile section    
    ctab$SNH4    <- 0.09      #  NH4 g Mg^-1    
    ctab$SNO3    <- 0.9       #  N03 g Mg^-1
    ctab$H20_s   <- 0.2       # !!! Starting soil moisture

    # Planting section 
    ctab$atP     <- 1         # planting section treatment number 
    ctab$PDATE   <- fields$PDATE  # Planting date
    ctab$EDATE   <- -99
    ctab$PPOP    <- 4.0
    ctab$PPOE    <- -99
    ctab$PLME    <- "S"
    ctab$PLDS    <- "R"
    ctab$PLRS    <- 90  # Row-spacing
    ctab$PLRD    <- -99
    ctab$PLDP    <- 5          # Planting depth (cm) - MZ = 5, WH = 4  
    ctab$PLWT    <- -99
    ctab$PAGE    <- -99
    ctab$PENV    <- -99
    ctab$PLPH    <- -99
    ctab$SPRL    <- -99

    # Fertilization    
    ctab$atF     <- 1 
    ctab$FDATE   <- 0   # Fertilization date (0 days after planting here)
    ctab$FMCD    <- "FE001"    
    ctab$FACD    <- "AP002"
    ctab$FDEP    <- 10  # Fertilization depth    
    ctab$FAMN    <- 5  # N kg
    ctab$FAMP    <- 5  # P kg    
    ctab$FAMK    <- 5  # K kg
    ctab$FAMC    <- -99
    ctab$FAMO    <- -99  
    ctab$FOCD    <- -99
    ctab$FERNAME <- -99
    
    # Residue controls
    ctab$atR     <- 1
    ctab$RDATE   <- fields$PDATE
    ctab$RCOD    <- "RE001"    # Residue code    
    ctab$RAMT    <- 1000       # Residue amount (kg)    
    ctab$RESN    <- 1.5        # Residue N %    
    ctab$RESP    <- -99
    ctab$RESK    <- -99 
    ctab$RINP    <- -99  
    ctab$RDEP    <- 5          # Residue incorporation depth
    ctab$RMET    <- -99 
    ctab$RENAME  <- -99
        
    # Environmental modification
    # modification variables
    # mod A = add, M = multiply, R = replace, S = subtract          
    ctab$atE     <- 1
    ctab$ODATE   <- fields$PDATE
    ctab$EDAY    <- "A   0"
    ctab$ERAD    <- "A   0"  # Solar radiation modification
    
    ctab$EMAX    <- "A   0"  # Tmax 
    ctab$EMIN    <- "A   0"  # Tmin modification                  
    ctab$ERAIN   <- "A   0"    # Rainfall modification     
    ctab$ECO2    <- "A   0"    # CO2 adjust: e.g. "R 548"
    ctab$EDEW    <- "A   0"
    ctab$EWIND   <- "A   0"
    ctab$ENVNAME <- "MOD"
    
    # Simulation controls section
    # SC1 - General
    ctab$atN     <- 1
    ctab$GENERAL <- "GE"
    ctab$NYERS   <- 1 # Number of years in simulation
    ctab$NREPS   <- 1
    ctab$START   <- "S"
    ctab$SDATE   <- fields$PDATE # Simulation start date
    ctab$RSEED   <- 2150
    ctab$SNAME   <- "Whatever"
    
    # SC2
    ctab$OPTIONS <- "OP"
    ctab$WATER   <- "Y"  # Water balance (Y or N))            
    ctab$NITRO   <- "Y"  # N balance (Y or N)
    ctab$SYMBI   <- "N" 
    ctab$PHOSP   <- "N"  # Phosphorous module switch
    ctab$POTAS   <- "N" 
    ctab$DISES   <- "N"   
    ctab$CHEM    <- "N" 
    ctab$TILL    <- "N"  # Tillage module switch    
    ctab$CO2     <- "M"  # "W" from WTH, "M" Mauna Loa, "D" default 380 ppm   
    
    # SC3 - Methods
    ctab$METHODS <- "ME"
    ctab$WTHER   <- "M" 
    ctab$INCON   <- "M" # "M" for measured or "S" from previous sim
    ctab$LIGHT   <- "E"
    ctab$EVAPO   <- "F" # ETmethod ("F" FAO-56, "R" P-T/Ritchie)
    ctab$INFIL   <- "R" # infilt. ("R" Ritchie, "S" SCS, "N" No mulch effects) 
    ctab$PHOTO   <- "L" # Photosyn ("C" daily, "R" RUE, "L" leaf photosynth)
    ctab$HYDRO   <- "R"
    ctab$NSWIT   <- 1 # N switch: diff. N options buried in code
    ctab$MESOM   <- "P" # !!! SOM method ("G" CERES/Godwin, "P" Parton/Century)
    ctab$MESEV   <- "S" # Soil evaporation ("R" Ritchie, "S" Suleiman-Ritchie)
    ctab$MESOL   <- "1" # Soil layer ("1" Original, "2" modified, "3" User def.)    
    
    # SC4 - Management
    ctab$MANAGEMENT <- "MA"
    ctab$PLANT   <- "R"  # automatic or date specified planting (A or R)
    ctab$IRRIG   <- "R" 
    ctab$FERTI   <- "D"
    ctab$RESID   <- "R"
    ctab$HARVS   <- "M"

    # SC5 - Outputs
    ctab$OUTPUTS <- "OU"
    ctab$FNAME   <- "N"
    ctab$OVVEW   <- "N" # Overview.OUT desired ("Y"/"N")
    ctab$SUMRY   <- "Y" # output--for large runs, probably only output set to Y    
    ctab$FROPT   <- 1  # Frequency in days of outputs
    ctab$GROUT   <- "N"  # Growth output ("Y"/"N")
    ctab$CAOUT   <- "N"  # Carbon output ("Y"/"N")
    ctab$WAOUT   <- "N"  # Water outputs ("Y"/"N")
    ctab$NIOUT   <- "N"  # N outputs ("Y"/"N")
    ctab$MIOUT   <- "N"  # P outputs ("Y"/"N")
    ctab$DIOUT   <- "N"
    ctab$VBOSE   <- "0"  # Verbose ("0" Summary, "N" Minimal, "Y" Normal, 
    # "D" Detail, "A" All)
    ctab$CHOUT   <- "N" 
    ctab$OPOUT   <- "N" 
		                                   
    
    # Auto-planting section
    ctab$PLANTING  <- "PL"
    pwindow <- as.Date(as.character(fields$PDATE[1]), "%y%j")
    pwindow <- strftime(c(pwindow - 15, pwindow + 21), "%y%j")
    ctab$PFRST   <- pwindow[1]  # Auto-planting window begin date
    ctab$PLAST   <- pwindow[2]  # Auto-planting window end date
    ctab$PH20L   <- 70  # Lowest PAW
    ctab$PSTMX   <- 40  # Upper T threshold for auto-planting
    ctab$PH20U   <- 100  # Upper PAW threshold    
    ctab$PH20D   <- 25   # Management depth for water (cm)
    ctab$PSTMIN  <- 10   # Lower temperature threshold for auto-planting    
    
    # Auto irrigation
    ctab$IRRIGATION  <- "IR"             
    ctab$IMDEP   <- 30
    ctab$ITHRL   <- 50   
    ctab$ITHRU   <- 100 
    ctab$IROFF   <- "GS000" 
    ctab$IMETH   <- "IR001" 
    ctab$IRAMT   <- 10 
    ctab$IREFF   <- 1
    
    # Auto nitrogen
    ctab$NITROGEN <- 30
    ctab$NMDEP   <- 30
    ctab$NMTHR   <- 50
    ctab$NAMNT   <- 25
    ctab$NCODE   <- "FE001"
    ctab$NAOFF   <- "GS000"
    
    # Auto residues
    ctab$RESIDUES <- "RE"     
    ctab$RIPCN   <- 100
    ctab$RTIME   <- 1
    ctab$RIDEP   <- 20 
    
    # Auto harvest
    ctab$HARVEST <- "HA"     
    ctab$HFRST   <- 0
    ctab$HLAST   <- "001"
    ctab$HPCNP   <- 100 
    ctab$HPCNR   <- 0
    
    # replace default ctab values with values from fid, combine tables
    upd_col <- colnames(ctab)[which(colnames(ctab) %in% colnames(fields))]
    ctab[, upd_col] <- fields[, upd_col, with = FALSE]
    xtab <- cbind(fields, ctab[, !colnames(ctab) %in% upd_col])
    return(xtab)
}

