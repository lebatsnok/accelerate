#' Get file extension
#'
#' file.ext
#'
#' @param FN file name
#' @return file extension (the part after the last "." in the file name)
#' @export
file.ext <- function(FN){
    L <- strsplit(FN, "\\.")
    K <- sapply(L, length)
    J <- mapply("[", L, K)
    J[K==1] <- ""
    J
    }


#' Apply a set of procedures to all files in a folder
#' 
#' 
#' process.folder 
#' 
#' @param  FOLDER (character) name of a folder 
#' @param  FUN a function
#' @param  FUN2 a funciton
#' @param  SAVE1 a function
#' @param  SAVE2 a function
#' @param  FINALLY a function 
#' @param  LIST a vector (numeric or logical) of files to include
#' @param  EXT file extension(s) to include
#' @param  PATTERN (character) pattern of file names - passed to list.files
#' @param  ALL (boolean) include all files in the folder?
#' @param  FULL (boolean) use full names
#' @param  VERBOSE (boolean) report all the superfluous details?
#' @param  FILES (character vector) a set of files possibly from different folders; if not NULL then the FOLDER argument is ignored
#' @param  TIME (boolean) sets timing on/off
#' @param  ... ... 
#' @details This applies a set of rules to all (or some: as per LIST) files in a folder (with the same extensions, specified in EXT). FUN is first applied to each file (using try), and then FUN2. Errors are collected and reported as the component $errors in the result. Finally, the function given in FINALLY is applied to combine the results (using do.call). For saving the results, specify a function in SAVE. Finally, the results are timed if TIME==TRUE. See read.actigraph.folder for an example.
#' @return  process.folder 
#' @export 
process.folder <- function(FOLDER, FUN, FUN2 = NULL, SAVE1 = NULL, SAVE2 = NULL, 
                           FINALLY = NULL, LIST=NULL, EXT=NULL, PATTERN="", 
                           ALL = FALSE, FULL = TRUE, VERBOSE = TRUE, 
                           FILES = NULL, TIME = TRUE, ...){
    # LIST can be numeric | logical or character vector
    # numeric or logical LIST are used to subset the list of files
    # if LIST is a character vector then only files with names included in the LIST are processed
    # with FILES you can give it a list of files possibly form different folders
    # then FOLDER is ignored (but should be specified, e.g., ".") but LIST is better be left as default unless you're trying to be clever
    if(TIME) {
       gc(FALSE)
       time <- proc.time()
       }
    files <- dir(FOLDER, full.names=FULL, all.files=ALL, pattern=PATTERN)
    if(!is.null(FILES)) files <- FILES
    if(!is.null(EXT)) files <- files[file.ext(files) %in% EXT]
    if(length(files)==0) stop("No files to process")
    if(length(LIST)>0) {
        if(!is.vector(LIST)) LIST <- as.vector(LIST)
        if(length(LIST) > length(files)) {
            warning("In process.folder: more elements in LIST than valid files in FOLDER")
            }
        if(is.numeric(LIST)|is.logical(LIST)) {
            LIST <- LIST[LIST <= length(files)]
            files <- files[LIST]
            }
        if(is.character(LIST)) files <- basename(files)[basename(files) %in% basename(LIST)]
        if(any(is.na(LIST))) {
            LIST <- stats::na.omit(LIST)
            warning("Missing elements in LIST were deleted")
            }
        }
    ERR <- RES <- list()
    cnt <- 0
    if(VERBOSE) cat("[1]\t")
    for(FILE in files){
        if(VERBOSE) cat(FILE, " ... ")
         if((cnt<-cnt+1)%%5==0) cat("\n[", cnt+1, "]\t")
         X <- try(FUN(FILE, ...))
         X2 <- if(is.null(FUN2)) X else try(FUN2(X))
         IXE <- c(inherits(X, "try-error"), inherits(X2, "try-error"))
         if(any(IXE)){
              ERR[[FILE]] <- X
              if(VERBOSE) cat("\n\nFile ", FILE, "caused the following error:\n", if(IXE[1]) X else X2, "\n\n")
              next
              }
         RES[[FILE]] <- if(is.null(FUN2)) X else FUN2(X)
         if(is.null(SAVE1)) {} else SAVE1(X, FILE)
         }
    if(VERBOSE) cat("\n")
    if(is.null(FINALLY)) {} else RES <- do.call(FINALLY,RES)
    if(is.null(SAVE2)) {} else SAVE2(RES, FOLDER)
    if(TIME){
        new.time <- proc.time()
        difftime <- new.time - time
        if(VERBOSE) print(difftime)
        }
    invisible(structure(RES, errors = ERR, timing = difftime))
    }


#' summary.acc 
#' 
#' 
#' summary.acc 
#' 
#' @param  object x 
#' @param  BY an interval specification that can be interpreted by cut.POSIXt, e.g., "DSTday" or "30 mins" or "2 hours"
#' @param  STATS STATS 
#' @param  copy the columns to be copied from the header part of the acc file, defaults to c("File", "ID", "Epoch", "Model", "Serial")
#' @param  report.errors report.errors 
#' @param  roundto only used if BY=="30 mins": any value that is not NULL will cause the time stamps to be rounded to nearest half-hour (this was quick hack for synchronizing data with another device -- let me know if you need something like this)
#' @param  short use short output (no HR or error reporting, and just one set of cutoffs) 
#' @param  ... ... 
#' @return  summary.acc 
#' @method summary acc
#' @export 
summary.acc <- function(object, BY="DSTday", STATS = if(short) list(function(...) CNTSTATS(..., short=TRUE)) else c(CNTSTATS, STEPSTATS, HRSTATS), copy = c("File", "ID", "Epoch", "Model", "Serial"), report.errors = !short, roundto = NULL, short = FALSE, ...) {
     ### accmeetri mudel -- summarysse
     if(is.function(STATS)) STATS <- list(STATS)
     x <- object
     wkdy <- function(x) as.integer( format(x, "%w"))  #get weekday as a number, 1=Monday, 0=Sunday
     
     HD <- Header(x)
     X <- Data(x)

     Epoch <- HD$Epoch
     timeseq <- seq(HD$Start, by=Epoch, length= nrow(X))

     # algus! : HD$Start+Epoch
     if(!is.null(roundto) && BY=="30 mins") timeseq <- c(round2nhh(timeseq[1]), timeseq)

     periods <- cut(timeseq, BY)
     if(!is.null(roundto) && BY=="30 mins") periods <- periods[-1]

     SPLITT <- split(X, periods)
     megafun <- function(XX, ...) {
       RES <- list()
       for(i in 1:length(STATS)) RES <- c(RES, STATS[[i]](XX, ...))
       as.data.frame(RES, stringsAsFactors=FALSE)
       }

     RES <- do.call(rbind, lapply(SPLITT, megafun, Epoch=Epoch, ...))

     Length <- as.data.frame(table(periods) / (60/Epoch), stringsAsFactors=FALSE)[2]
     Period <- as.POSIXct(unique(periods))
     names(Length) <- "Length"
# browser()
     df <- function(...) data.frame(..., stringsAsFactors = FALSE)
     RES <- df( HD[copy], 
            df( Period=Period,
                        Length=Length,
                        Wkdy = wkdy(Period) 
                       ), 
                         RES, row.names=NULL)
     if(report.errors) RES <- df(RES, get.errors(x))
     RES
     }  


#'  summarize
#' 
#' 
#'  summarize 
#' 
#' @param  A A data frame with accelerometer results (such as the result of summary.acc or read.actigraph.folder)
#' @param  condition inclusion condition (column names in table A can be used, e.g., "val.time >= 480") 
#' @param  no.days number of days
#' @param  keep not implemented yet
#' @param  head names of variables to include in the result
#' @param  subset.only subset.only 
#' @return  summarize 
#' @export 
summarize <- function (A, 
                       condition = val.time >= 360, 
                       no.days = 3, keep = "all",
                       head = c("File", "ID", "Epoch", "Period"),
                       subset.only=FALSE) {
    # proovi siin kasutada rowsum'i - teeb kiiremaks kuid vaja palju katsetada
    cond <- substitute(condition)
    A$DAYS <- 1
    A <- subset(A, eval(cond))
    k <- with(A, table(ID))
    k <- which(k >= no.days)
    A <- A[A$ID %in% names(k),]

    if(keep!="all") {
        if(!is.function(keep)) stop("Keep must be either 'all' or a function taking no. days and a data frame as arguments")
        ## but it's not yet implemented
        ## A <- split(A, A$ID)
        ## A <- do.call(rbind,(lapply(A, function(..) ..[1:no.days,])))
        }

    if(subset.only) return(A)

    FUN <- function(x, nimi=NULL){
        MAX <- "permax"
        SUM <- c("Length", "DAYS", "tot.cnts")
        PASTE <- c("Wkdy")
        if(!is.numeric(x)) return(x[1]) else {
            if(nimi %in% MAX) return(max(x, na.rm=TRUE))
            if(nimi %in% SUM) return(sum(x, na.rm=TRUE))
            if(nimi %in% PASTE) return(paste(x, collapse=","))
            return(mean(x, na.rm=TRUE))
            }
        "No way"
        }
    # browser()
    asd <- function(...) as.data.frame(..., stringsAsFactors=FALSE)
    splat <- split(A, list(A$ID))
    summm <- lapply(splat, function(.) mapply(FUN, ., names(A), SIMPLIFY=FALSE))
    do.call(rbind, lapply(summm, asd))
}

#'  keepfirst 
#' 
#' 
#'  keepfirst 
#' 
#' @param  x x 
#' @param  howmany howmany 
#' @return  keepfirst 
#' @export 
keepfirst <- function(x, howmany=3){
  # see on asi mida tuleb txiendada -- & yldistada
  # check if it has at least `howmany` days
  # then : exclude 1st or last day
  # then: include random days
  # then: incl at least N weekdays and M weekend days
  x <- split(x, x$ID)
  x <- lapply(x, function(Z) Z[1:howmany,])
  x <- do.call(rbind, x)
  x
  }


#'  load.acc.dir 
#' 
#' 
#'  load.acc.dir 
#' 
#' @param  folder folder 
#' @param  ... ... 
#' @return  load.acc.dir 
#' @export 
load.acc.dir <- function(folder, ...)
    process.folder(folder, 
            PATTERN = "rda", 
            FUN = load.acc, 
            FUN2 = function(x) summary.acc(x, ...),
            FINALLY = rbind
            )



#'  read.actigraph.folder 
#' 
#' 
#'  read.actigraph.folder 
#' 
#' @param  FOLDER name of the folder
#' @param  LIST which files from the folder to read in (could be numeric or logical index)
#' @param  RIT reintegrate to common epoch (default is 15)
#' @param  CUTOFFS which cutoffs to use (default to a long list used in Idefics study: Cutoffs.T1)
#' @param  BY. period to use as a unit of analysis: defults to "DSTday" (calendar day but using daylight saving time changes when relevant i.e period is usually 24 hours but can be 23 or 25); could be anything like "30 mins" or "2 days"
#' @param  STATS. defaults to c(CNTSTATS, STEPSTATS, HRSTATS) - that is - calculate statistics for activity counts, steps, and heart rate
#' @param  EXT. which file extensions to use; defaults to c("dat", "csv", "agd")
#' @param  ... ... 
#' @return  read.actigraph.folder 
#' @export 
read.actigraph.folder <- function(FOLDER, LIST=NULL, RIT = 15, CUTOFFS = Cutoffs.T1, BY. = "DSTday", STATS.= c(CNTSTATS, STEPSTATS, HRSTATS), EXT. = c("dat", "csv", "agd"), ...)  
    process.folder(FOLDER, 
        FUN = function(..,...) read.actigraph(.., preprocess=reintegrate[TO=RIT, fractional=stop], ...),
        FUN2 = function(..,...) summary.acc(.., cutoffs=CUTOFFS, report.errors=FALSE, BY = BY., STATS = STATS., ...),
        EXT = EXT.,
        LIST = LIST,
        FINALLY = rbind)



########################################################################
########################################################################
########################################################################


#'  Time to winter solstice 
#' 
#' 
#'  tty 
#' 
#' @param  d d 
#' @param  ... ... 
#' @return  tty 
#' @export 
tty <- function(d,  ...){
    d <- as.Date(d)
    y <- as.integer(format(d, "%Y"))
    yule <- data.frame( a=ISOdate(y-1, 12, 22), b=ISOdate(y, 12, 22), c=ISOdate(y+1, 12, 22))
    yule <- as.data.frame(lapply(yule, as.Date))
    ttcy <- rep(NA, length(d))
    for(iii in 1:length(d)){
         yo <- yule[iii,]
         do <- d[iii]
         difs <- sapply(yo, difftime, do)
         ttcy[iii] <- (function(x) x[which.min(abs(x))])(difs)
         }
    ttcy
    }

#' Time to winter solstice
#' 
#' 
#'  tty2 
#' 
#' @param  d d 
#' @param  ... ... 
#' @return  tty2 
#' @export 
tty2 <- function(d,  ...){
    d <- as.Date(d)
    y <- as.integer(format(d, "%Y"))
    yule <- as.Date(c( a=ISOdate(y-1, 12, 22), b=ISOdate(y, 12, 22), c=ISOdate(y+1, 12, 22)))
    difs <- yule - d
    (function(x) x[which.min(abs(x))])(difs)
    }


#' Plot time activity categories by 30-min segments
#' 
#' 
#' aciPlot 
#' 
#' @param  D summarized data (using 30 mins as period with summary.acc) 
#' @param  cutname Name of cut points 
#' @param  cats Cutoff categories as a vector (eg., sedentary, light, moderate) 
#' @param  plot if TRUE, the results will be plotted using plotAci 
#' @param  select select 
#' @param  ... ... 
#' @return  cdplot.acc 
#' @export 
aciPlot <- function(D, cutname = "Evenson", cats = c("Sedentary", "Light", "Moderate", "Vigorous"), plot=TRUE, select = Wkdy %in% 0:6, ...){
       if(is.acc(D)) {
         CO <- get(paste0("Cutoffs.", cutname), .GlobalEnv)
         D <- summary(D, BY="30 mins", cutoffs = CO)
       }
       nrm <- 360
       cutoffnames <- paste(cutname, cats, sep=".")
       rateoc <- with(D, difftime(Period[2], Period[1], units="mins"))
       nrowpd <- 1440 / as.numeric(rateoc)
       D$day <- as.character(trunc(D $ Period, "days"))
       dt <- table(D$day)
       wrongdays <- dt[dt!=nrowpd]
       throwaway <- which(D$day %in% names(wrongdays))
       if(length(throwaway)) D <- D[-throwaway,]

       if(nrow(D) == 0) stop("zero rows left after deleting incomplete days")

       hrs <- as.numeric(format(D$Period, "%H"))
       mns <- as.numeric(format(D$Period, "%M"))
       hrfs <- mns / 60
       taim <- hrs + hrfs

       splat <- lapply(split(D[,cutoffnames], D$day), as.matrix)

       splat <- splat[sapply(splat, sum)>=nrm] 
       if(!length(splat)) stop("zero days left after deleting <360min days")

       avgd <- Reduce("+", splat) / length(splat)
       res <- structure(apply(avgd, 1, cumsum), class="aci")

       if(plot) plotAci(res, maxy=as.numeric(rateoc), ... )
       res
      # browser()
       }

#' plotAci
#' 
#' 
#' plotAci 
#' 
#' @param  x x 
#' @param  maxy maxy 
#' @param  ... ... 
#' @return  nothing useful
#' @export 
plotAci <- function(x, maxy = NULL, ...){          
       maxx <- ncol(x)
       nrx <- nrow(x)
       cols <- rev(grDevices::heat.colors(nrx))
       graphics::plot(0,0, xlim=c(1,maxx), ylim=c(0, maxy),  
              type="n", axes=FALSE, xlab="Time", ylab="", ...)
       for(iii in nrx:1)
               graphics::polygon(c(1, 1:maxx, maxx), c(0, x[iii,], 0), col=cols[iii])
       graphics::axis(1, at=0:24*2, labels=0:24)
       graphics::axis(2)
       graphics::box()
       }


