#' Delete consecutive zeros
#'
#' Consecutive zeros (of at least 'period' minutes) 
#' are replaced by NA
#'
#' @param x the file to load
#' @param period minimum length
#' @return x
#' @export
delete.zeros <- structure(function(x, period = 20) {
    ##################################################
    #  checks for period+ mins of consecutive
    #  zeros 
    ##################################################

    Epoch <- Header(x)$Epoch
    DATA <- Data(x)
    D  <- get.counts(x)
    # nb! siin on ikka probleem 3dnx andmetega! Nii Data() kui get.counts()
    # peaks seda arvesse v6tma
    # kas nii, et Data ja get.counts oleks generic ja 3dnx objektidel omaette klass
    #### kontrolli seda!!!!!
    act <- grep("Activity", names(DATA))
    
    # siia asemele accessor function, kui see korda saab!
    Header(x, "Preprocess") <- paste("delete.zeros(x, period = ", period, ")", sep="") 
    # siia tuleks midagi loetavamat panna!

    if(!is.null(period)) {    
       conze <- function(x, value=0, limit=5) {
         rle.x<-rle(x)
         rle.x$values[rle.x$lengths>=limit & rle.x$values==0]<-NA
         is.na(inverse.rle(rle.x))
         }
       periodlength <- period*60 / Epoch
       foo<-conze(D, 0, periodlength)
       for(ii in act) DATA[foo,ii] <- NA
       }
    x$X  <- DATA   # this to Data<-
    x
    }, class="function")


#' Nofilter
#'
#' Filler function when no filter is used 
#'
#' @param X acc object
#' @param ... discarded
#' @return X
#' @export
nofilter <- function(X, ...) X

#' Filter out implausible values
#'
#' Filter out implausible values
#'
#' @param X data part of an acc object
#' @param HD header data
#' @param ERR env to send err...
#' @param maxsteps maxsteps
#' @param maxcnts maxcnts default 15000
#' @param minhr minhr default 60
#' @param maxhr maxhr default 220
#' @return filtered X
#' @export
FILTER <- function(X, HD, ERR, 
             maxsteps = 500, maxcnts = 15000, minhr = 60, maxhr=220){
    Epochrate <- 60/HD$Epoch
    def <- names(X)
    MAXSTEPS <- maxsteps/Epochrate
    MAXCNTS <- maxcnts/Epochrate
    act <- grep("Activity", def)
    # as per Cliff et al JSciMedSport 2008, p 561 counts over 15000 biol implausible
      ERR$Activity <- sum(X[,act]>MAXCNTS | X[,act]<0)
      ERR$ActMax <- max(X[,act])
      ERR$ActMin <- min(X[,act])
    X[,act][X[,act]>MAXCNTS | X[,act] < 0 ] <- NA
    # step counts over 500 per minute implausible
    if("Steps" %in% def) {
             ERR$Steps <- sum(X$Steps>MAXSTEPS | X$Steps < 0)
             ERR$StepMax <- max(X$Steps)
             ERR$StepMin <- min(X$Steps)
             ERR$Stepsgta <- sum(X$Steps>X$Activity1)
             X$Steps[X$Steps > MAXSTEPS | X$Steps < 0] <- NA
             }
    # heart rate 
    # 1. check if HR is recorded as bpm or bpe as per John Schneider
    # 2. convert bpe to bpm
    # 3. delete stupid values 
    if("HR" %in% def){
       if(is.new.ver(HD$Firmware)) X[,"HR"] <- X[,"HR"]*Epochrate
       # for >10s epochs this removes <MIN & >MAX values
       # for shorter epochs, this has to be resolved yet
       # maybe reintegrate HR to 60s epoch for all data?
       # or filter using per-minute moving average???
       HRMIN <- minhr; HRMAX <- maxhr
       if(HD$Epoch >= 10) {
             ERR$HR <- sum(X$HR < HRMIN | X$HR > HRMAX)
             ERR$HRMax <- max(X$HR)
             ERR$HRMin <- min(X$HR)
             X$HR[X$HR < HRMIN | X$HR > HRMAX] <- NA
             }
       
       }
    X
    }

#' Reintegrate
#'
#' Reintegrate to another (longer) epoch
#'
#' @param x acc object
#' @param TO target epoch
#' @param FACTOR factor
#' @param after function to be applied afterwards
#' @param fractional function to be called when fractional epoch occurs: normally either warning or error, possibly print or cat
#' @param ... passed to 'after'
#' @return x
#' @export
reintegrate <- function(x, TO = NULL, FACTOR = NULL, after = delete.zeros, 
    fractional = warning, ...){
    # need either TO or FACTOR but TO gets priority
    # 14.02.2011 - vanad versioonid asendatud!
    X <- Data(x)
    HD <- Header(x)
    Epoch <- HD$Epoch
    if(!is.null(TO)) FACTOR <- NULL
    if(is.null(TO) && is.null(FACTOR)) stop("Need either TO or FACTOR arg to be specified for reintegrate1")
    if(is.null(FACTOR)) FACTOR <- TO / Epoch
    if(FACTOR %% 1 != 0) {
         fractional("Can't reintegrate ", HD$File, " fractional factor:", FACTOR, "\n")
         return(after(x, ...))
         }
    if(FACTOR!=1){   # v6i: if Factor>=1 ??
      foldSums <- function(x, F=FACTOR, na.rm=FALSE) {
          saba <- length(x)%%F
          if(saba!=0) x <- c(x, rep(NA, F-saba))
          colSums(matrix(x, nrow=F), na.rm=na.rm)
          }
      X <- as.data.frame(lapply(X, foldSums))
      if("HR" %in% names(X)) X$HR <- X$HR/FACTOR
      HD $ Epoch <- HD $ Epoch * FACTOR
      } else return(after(x, ...))

    after(structure(list(HD=HD, X=X), class="acc"), ...)
    }
class(reintegrate)<-"function"

 
#######################################################################################
 
#' print.acc
#'
#' print.acc
#'
#' @param x acc object
#' @param FORMAT date format
#' @param ... dots
#' @method print acc
#' @return nothing useful
#' @export
print.acc <- function(x, FORMAT="%A, %d. %B %Y %H:%M:%S", ...) {
    HD <- Header(x)
    cat(length(HD$Axes), "-axial accelerometer data from ", HD$Model, ", Serial no. ", HD$Serial, "\n")
    cat("Source file:\t", HD$File, "\n")
    cat("Epoch: \t\t", HD$Epoch, " seconds.\n")

    cat("Start time: \t", format(HD$Start, FORMAT), "\n")
    cat("Download time:\t", format(HD$Down, FORMAT), "\n")
    if(!is.null(HD$End)) cat("Data ends at: \t", format(HD$End, FORMAT), "\n")

    cat("Preprocessing:\t", if(is.null(HD$Preprocess)) "none" else as.character(HD$Preprocess), "\n")
    cat("Data table has ", nrow(Data(x)), " rows and ", ncol(Data(x)), " columns.\n")
    cat("\nStart of data:\n")
    print(utils::head(Data(x)), ...)
    cat("---------------------------\n")
    }
 


#' Bouts
#'
#' Bouts
#'
#' @param Counts counts
#' @param Epoch epoch
#' @param BoutLength bout length
#' @param CUT cut
#' @param Threshold .8
#' @return bouts
#' @export
BOUTS <- function (Counts, Epoch, BoutLength=5, CUT, Threshold = .8 ) {
    # this is only for "at least" bouts
    Win <- BoutLength*60 / Epoch

    if(length(Counts)<Win) return(list(TiB = NA, NoB = NA))
    Counts[is.na(Counts)] <- 0
    Roll <- zoo::rollmean(Counts >= CUT, Win)
    Starts <- which(Roll>=Threshold)
    Counts[1:length(Counts)] <- 0
    for(i in Starts) Counts[i:(i+Win-1)] <- 1
    BoutedX <- sum(Counts) / (60/Epoch)
    NumberOfBouts <- sum(rle(Counts)$values)
    list(TiB = BoutedX, NoB = NumberOfBouts)
}


#' Cuttable
#'
#' Cuttable
#'
#' @param x x
#' @param breaks breaks
#' @param include.lowerst TRUE
#' @param right FALSE
#' @param ... dots
#' @return bincount
#' @keywords internal
cuttable <- function (x, breaks, include.lowest = TRUE, right = FALSE, ...) {
    n <- length(x <- x[is.finite(x)])
    nB <- length(breaks)
    storage.mode(x) <- "double"
    storage.mode(breaks) <- "double"
    # "bincode" for coding 
    #.C("bincount", x, as.integer(n), breaks, as.integer(nB), 
    #    counts = integer(nB - 1), right = as.logical(right), 
    #    include = as.logical(include.lowest), naok = FALSE, NAOK = FALSE, 
    #    DUP = FALSE, PACKAGE = "base")$counts
    C_BinCount <- environment(graphics::hist)$C_BinCount
    if(!is.null(C_BinCount)) .Call(C_BinCount, x, breaks, right = as.logical(right), include.lowest = as.logical(include.lowest)) else
      graphics::hist(x, breaks, right=right, include.lowest=include.lowest, plot=FALSE)$counts
}

 
do_cutoffs <- function(x, Epoch, cutoffs=Cutoffs, return.MVPA = TRUE, 
    return.bouts = TRUE, return.rel=TRUE, return.raw=TRUE, rel.factor=1){
    INF <- 99999999L
    EPOC <- 60/Epoch
    EPOS <- lapply(cutoffs, "[", "Epoch")
    coeffs <- lapply(EPOS, "/", Epoch)
    CUTS <- lapply(cutoffs, function(x) c(x[-1], INF))
    CUTS <- mapply("/", CUTS, coeffs, SIMPLIFY = FALSE)
    fina <- function(i) names(i)[-length(i)]
    COtables <- lapply(CUTS, function(i) structure(cuttable(x,i), names=fina(i))/EPOC)
    if (return.MVPA) 
        COtables <- lapply(COtables, function(x) {
            if ("Moderate" %in% names(x) && "Vigorous" %in% names(x)) 
                c(x, MVPA = unname(x["Moderate"] + x["Vigorous"]))
            else x
        })
    cuttos <- sapply(CUTS, function(x) x[if ("Moderate" %in% 
        names(x)) 
        "Moderate"
    else utils::tail(names(x)[names(x) != ""], 1)])
    boutstable <- unlist(lapply(cuttos, function(z) unlist(BOUTS(x, 
        Epoch, 5, z, 0.8))))
    val.time <- sum(!is.na(x))/EPOC
    if (return.rel) 
        COtables2 <- lapply(lapply(COtables, "/", val.time), 
            "*", rel.factor)
    # list(COtables = COtables, COtables2 = COtables2, boutzz = boutzz)
    data.frame(if (return.raw) 
            as.data.frame(lapply(COtables, as.list), stringsAsFactors = FALSE)
        else data.frame(row.names = 1), if (return.rel) 
            as.data.frame(lapply(COtables2, as.list))
        else data.frame(row.names = 1), if (return.bouts) 
            as.data.frame(as.list(boutstable))
        else data.frame(row.names = 1))
    }


#' 3dnx cntstats
#'
#' 3dnx cntstats
#'
#' @param x x
#' @param Epoch epoch
#' @param cutoffs cutoffs
#' @param return.MVPA TRUE
#' @param return.bouts TRUE
#' @param return.rel TRUE
#' @param return.raw TRUE
#' @param rel.factor 1
#' @param reintegrate.to NULL
#' @return 3dnx cntstats
#' @export
CNTSTATS.3dnx <- function (x, Epoch, cutoffs = Cutoffs, return.MVPA = TRUE, 
    return.bouts = TRUE, return.rel = TRUE, return.raw = TRUE, rel.factor = 1,  
    reintegrate.to = NULL) 
{
    sums <- get.counts(x, "sum")
    EPOC <- 60/Epoch                     # doubled
    val.time <- sum(!is.na(sums))/EPOC      # doubled  

    ctz <- do_cutoffs(sums, Epoch, cutoffs=cutoffs, return.MVPA = return.MVPA,
         return.bouts = return.bouts, return.rel=return.rel, return.raw=return.raw, rel.factor=rel.factor) 

    simplestats <- function(x){
        data.frame(avg.cpm = iCurry(mean, x, na.rm = TRUE) * 
        EPOC, tot.cnts = iCurry(sum, x, na.rm = TRUE), 
        permax = iCurry(max, x, na.rm = TRUE))
        }
    xyz <- data.frame(
        X = simplestats(get.counts(x, "Activity1")),
        Y = simplestats(get.counts(x, "Activity2")),
        Z = simplestats(get.counts(x, "Activity3")))

    RES <- data.frame(avg.cpm = iCurry(mean, sums, na.rm = TRUE) * 
        EPOC, tot.cnts = iCurry(sum, sums, na.rm = TRUE), val.time = val.time, 
        permax = iCurry(max, sums, na.rm = TRUE), ctz, xyz)
    RES
}


#' CNTSTATS
#'
#' CNTSTATS
#'
#' @param x x
#' @param Epoch epoch
#' @param cutoffs cutoffs
#' @param return.MVPA TRUE
#' @param return.bouts TRUE
#' @param return.rel TRUE
#' @param return.raw TRUE
#' @param rel.factor 1
#' @param reintegrate.to NULL
#' @param short FALSE
#' @return 3dnx cntstats
#' @export
CNTSTATS <- function (x, Epoch, cutoffs = Cutoffs, return.MVPA = TRUE, 
    return.raw = TRUE, return.rel = !short, rel.factor = 1, 
    return.bouts = !short, reintegrate.to = NULL, short = FALSE) 
{
    if(short) cutoffs <- cutoffs[1]
    what <- "first"   # kas see v6iks olla arg.?
    x <- get.counts(x, what)
    EPOC <- 60/Epoch                     # doubled
    val.time <- sum(!is.na(x))/EPOC      # doubled
    ctz <- do_cutoffs(x, Epoch, cutoffs=cutoffs, return.MVPA = return.MVPA,
         return.bouts = return.bouts, return.rel=return.rel, return.raw=return.raw, rel.factor=rel.factor) 
    RES <- data.frame(avg.cpm = iCurry(mean, x, na.rm = TRUE) * 
        EPOC, tot.cnts = iCurry(sum, x, na.rm = TRUE), val.time = val.time, 
        permax = iCurry(max, x, na.rm = TRUE), ctz)
    RES
}



#' stepstats
#'
#' stepstats
#'
#' @param x x
#' @param ... dots 
#' @return stepstats
#' @export
STEPSTATS <- function(x, ...){
     x <- get.steps(x)
     if(is.null(x)) STEPS <- NA else STEPS <- iCurry(sum, x, na.rm=TRUE)
     data.frame(STEPS= STEPS)
     }

#' hrstats
#'
#' hrstats
#'
#' @param x x
#' @param Epoch epoch
#' @param ... dots 
#' @return hrstats
#' @export
HRSTATS <- function(x, Epoch, ...){
     HR <- get.hr(x)
     geomean <- function(x, ...) exp(mean(log(x[x>0]), ...))
     RES <- within(data.frame(
            datalength = length(HR),
            validEpochs = length(stats::na.omit(HR)),
            min = iCurry(min, HR, na.rm=TRUE), 
            max = iCurry(max, HR, na.rm=TRUE),
            mean = iCurry(mean, HR, na.rm=TRUE), 
            median = iCurry(stats::median, HR, na.rm=TRUE),
            geomean = iCurry(geomean, HR, na.rm=TRUE),
            sd = iCurry(stats::sd, HR, na.rm=TRUE),
            dur140 = sum(HR>140, na.rm=TRUE)/(60/Epoch)), 
          durTot <- validEpochs/(60/Epoch)
          )
     names(RES) <- paste("HR", names(RES), sep=".")
     RES
     }

#' hrstats2
#'
#' hrstats2
#'
#' @param x x
#' @param Epoch epoch
#' @param cutoffs evenson
#' @param ... dots 
#' @return hrstats
#' @export
HRSTATS2 <- function(x, Epoch, cutoffs=Cutoffs.Evenson, ...){
     if(length(x$HR)==0) x$HR <- NA   # kickfix
     brx <- c(cutoffs[[1]][-1], 999999)/(60/Epoch)
     cutz <- cut(c(get.counts(x), brx[-1]), brx, labels=utils::head(names(brx),-1))
     HR <- c(rep(NA, length(brx)-1), get.hr(x))
     tapas <- tapply(HR, cutz, stats::quantile, na.rm=T)
     names(tapas) <- paste(substr(names(tapas),1,3), "HR", sep="")
     for(iii in 1:length(tapas)) names(tapas[[iii]]) <- 0:4*25
     tapas <- as.data.frame(as.list(unlist(tapas)))
     tapas $ HROK <- sum(!is.na(HR)) / (60/Epoch)
     tapas
     }


#' cntstats2
#'
#' cntstats2
#'
#' @param x x
#' @param Epoch epoch
#' @param cutoffs evenson
#' @param ... dots 
#' @return cntstats
#' @export
CNTSTATS2 <- function(x, Epoch, cutoffs=Cutoffs.Evenson, ...){
  INF <- 9e+9
  CNTS <- get.counts(x)
  EPOC <- 60/Epoch
  EPOS <- lapply(cutoffs, "[", "Epoch")
  coeffs <- lapply(EPOS, "/", Epoch)
  CUTS <- lapply(cutoffs, function(x) c(x[-1], INF))
  CUTS <- mapply("/", CUTS, coeffs, SIMPLIFY = FALSE)
  
  cufas <- lapply(seq_along(cutoffs), function(iii){
    cut(CNTS, CUTS[[iii]], labels = names(cutoffs[[iii]])[-1], include.lowest=TRUE, right=FALSE)
  })
  
  nc <- names(cutoffs)
  names(cufas) <- nc
  r1 <- lapply(cufas, table)
  r1 <- lapply(r1, function(x) x/EPOC)
  
  r2 <- lapply(cufas, function(x) tapply(CNTS, x, sum))
  
  tdf <- function(x) as.data.frame(as.list(unlist(x)))
  
  data.frame(A=tdf(r1), B=tdf(r2))  
}



#' round2nhh
#'
#' round to nearest half hour
#'
#' @param td a time vector
#' @return ROUNDED TD
#' @keywords internal
round2nhh <- function(td){
       # ymarda lxhima (eelmise) 30 min- perioodini!
       # no type check here
       nfh <- trunc(td, "hours")
       tif <- difftime(td, nfh, units="mins")
       if(tif > 30) tif <- tif - 30
       td - tif
       }


#' Convert an "acc" object to zoo
#'
#' as.zoo.acc
#'
#' @param x x
#' @param ... extra dots passed to as.zoo
#' @return zoo obj
#' @method as.zoo acc
#' @export
as.zoo.acc <- function(x, ...){
    l <- list(...)
    Extract <- get.counts
    if("Extract" %in% names(l)) Extract <- l$Extract
    HD <- Header(x)
    x <- Extract(x)
    Epoch <- HD$Epoch
    LENX <- if(is.null(nrow(x))) length(x) else nrow(x)
    timeseq <- seq(HD$Start, by=HD$Epoch, length= LENX)
    zoo(x, timeseq, ...)
    }


#' chbrks2mins
#'
#' chbrks2mins 
#'
#' @param breaks brx
#' @return nothing useful
#' @keywords internal
chbrks2mins <- function (breaks) {
    by2 <- strsplit(breaks, " ", fixed = TRUE)[[1L]]
    valid <- pmatch(by2[length(by2)], c("hours", "days", "weeks", 
        "months", "years"))
    tambov <- c(1, 24, 24*7, 24*30, 24*365)
    num <- if(length(by2)==1) 1 else as.numeric(by2[1])
    tambov[valid] *  num
    }
 


#' load.acc
#'
#' load.acc
#'
#' @param file file
#' @param DIR dir
#' @param get.id get.id
#' @param ... dots
#' @return acc obj
#' @export
load.acc <- function(file, DIR, get.id = identity, ...){
  filename <- if (missing(DIR)) 
    file
  else paste(DIR, file, sep = "/")
  if (!file.exists(filename)) 
    stop("File ", filename, " does not exist")
  ACC <- Load(filename)
  process.acc(ACC, get.id= get.id, ...)
}
   

#' dontsave
#'
#' Filler function when nothing needs to be done :P
#'
#' @param ... discarded
#' @return invisible NULL
#' @export
dontsave <- function(...) invisible(NULL)


#' write.csv
#'
#' write.csv
#'
#' @param ... dots
#' @return nothing useful 
#' @export
write.csv <- structure(write.csv, class="function")

#' write.csv2
#'
#' write.csv2
#'
#' @param ... dots
#' @return nothing useful 
#' @export
write.csv2 <- structure(write.csv2, class="function")
