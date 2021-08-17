#' \tabular{ll}{
#' Package: \tab accelerate\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1\cr
#' Date: \tab 2012-12-08\cr
#' License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' Read in and summarize accelerometer data
#'
#'
#' @name accelerate-package
#' @aliases accelerate
#' @docType package
#' @title accelerate-package
#' @author Kenn Konstabel \email{kenn.konstabel AT tai.ee}
#' @references
#' \url{http://en.wikipedia.org/wiki/accelerometry}
#' @keywords package
#' @import zoo RSQLite DBI
#' @seealso \code{\link{read.actigraph}}
#' @examples NULL
#' 
#'
NULL

###########################################################################
#### functions to read in data
###########################################################################

#' Read in actigraph *.dat file  
#'
#' Reads in an Actigraph (GT1M, ActiTrainer) dat file and
#' returns an object of class "acc" that can be further
#' processed with summary.acc etc.
#'
#' @param file the file to read 
#' @param DIR folder - added to 'file' but ignored if 'file' contains '/'
#' @param id.prefix - prefix to be added to ID (this argument will be removed in future versions as the same functionality can be achieved with writing acustom get.id function)
#' @param preprocess - function to preprocess data - either delete.zeros or reintegrate - or a custom function
#' @param dateformat - c("month", "day", "year") - order in which  each element occurs in the date string
#' @param get.id - function to extract ID from file name, eg. getID
#' @param .filter - function to filter out implausible values
#' @return an object (list) of class "acc"
#' @seealso \code{\link{read.actigraph.csv}}
#' 
#' @details For reading in older 'Actigraph' files in "dat" format.
#' The main shortcoming of this format is ambiguity of date format -
#' it is dependent on computer used to download the data. Therefore
#' the newer format (agd) should be used when possible.
#'
#'
#' @export
read.actigraph.dat <- function(file, DIR, id.prefix="", 
    preprocess = delete.zeros, dateformat=c("month", "day", "year"), 
    get.id= getID, .filter=FILTER) {
    #############################################
    #  reading and preprocessing data
    #############################################
     filename <- if(missing(DIR)) file else paste(DIR, file, sep="/") 
     if(!file.exists(filename)) stop("File ", filename, " does not exist")
#    filename <- if(regexpr("/", file)== -1) paste(dir, file, sep="/") else file
    HD <- read.actigraph.header(filename, dateformat, id.prefix) 
    def <- actigraphmode(HD$Mode, HD$Model)
    # ActiLife 5.5.2 puhul
    # loodetavasti siis parandatud versioonil on uus number!!!
    if(HD$ActiLife %in% "v5.5.2" & HD$Model %in% "ActiTrainer"){
           def $ cols <- def $ used
           }
    x <- try(scan(file=filename, skip=HD$HL, what=integer(1), quiet=TRUE))
    Errors <- new.env(hash=FALSE, parent=emptyenv())
    if(any(x<0)) {
        Errors$Negcounts <- sum(x<0)
        warning("Negative counts found in ", filename, 
                "\nPlease check that file (if possible, try downloading it again using ActiLife).")
        }
    # pad NA's if length(x) shorter than expected #
    pad <- function(X, K){
        li <- length(X) %% K
        Errors$Pad <- li
        if(li == 0) X else c(X, rep(NA, K-li))
        }
    X <- matrix(pad(x, def$cols), ncol=def$cols, byrow=TRUE)[,1:def$used]
    X <- as.data.frame(X)
    names(X) <- def$names
    HD$Axes <- act <- def$names[grep("Activity", def$names)]

    ####  Filter out implausible (and impossible) values
    X <- .filter(X, HD, Errors) # changes arg.!!

    if(nrow(X)==0) stop("Zero data rows in ", file, "--- skipping this file :(((")
    if(HD$Epoch == 0) stop("Fatal error in file ", file, ": epoch == 0")
    HD$End <- HD$Start + (nrow(X)-1) * HD$Epoch
    HD$ID <- get.id(filename)
    HD$Errors <- Errors
    resu <- list(HD=HD, X=X)
    resu <- preprocess(resu)
    structure(resu, class="acc") 
    }

#' Read in a CSA (older Actigraph models ) file  
#'
#' Reads in a CSA file and returns an object of class "acc" that can be 
#' further processed with summary.acc etc.
#'
#' @param file the file to read 
#' @param DIR folder - added to 'file' but ignored if 'file' contains '/'
#' @param get.id - function to extract ID from file name, eg. getID
#' @param ... any extra parameters are silently ignored (check if this can be deleted)
#' @return an object (list) of class "acc"
#' @seealso \code{\link{read.actigraph.csv}}
#' 
#' @details For reading in older CSA actigraph files. The 
#' format is very similar to the 'dat' format used with GT1M and ActiTrainer
#' and has the same shortcomings.
#'
#'
#' @export
read.csa <- function (file, DIR, get.id = identity, ...){
     filename <- if(missing(DIR)) file else paste(DIR, file, sep="/") 
     if(!file.exists(filename)) stop("File ", filename, " does not exist")
   HD <- read.actigraph.header(filename, Way="CSA")
   HD$Axes <- "Activity1"
   HD$ID <- get.id(file)
   HD$Errors <- new.env(hash=FALSE, parent=emptyenv())
   x <- try(scan(file=filename, skip=HD$HL, what=integer(1), quiet=TRUE))
   X <- FILTER(data.frame(Activity1=x), HD, HD$Errors)
   # was:  X <- FILTER(data.frame(Activity1=x), 60/HD$Epoch, "Activity1", HD, HD$Errors)
   RES <- list(HD=HD, X = X)
   structure(RES, class="acc")
   }


#' Read in actigraph *.csv file  
#'
#' Reads in an Actigraph (GT1M, ActiTrainer) csv file and
#' returns an object of class "acc" that can be further
#' processed with summary.acc etc.
#'
#' @param file the file to read 
#' @param DIR folder - added to 'file' but can be missing
#' @param preprocess - function to preprocess data - either delete.zeros or reintegrate - or a custom function
#' @param get.id - function to extract ID from file name, eg. getID
#' @param .filter - function to filter out implausible values
#' @param ... extra parameters are silently ignored
#' @return an object (list) of class "acc"
#' @seealso \code{\link{read.actigraph.csv}}
#' 
#' @details For reading in older actigraph files in "dat" format.
#' The main shortcoming of this format is ambiguity of date format -
#' it is dependent on computer used to download the data. Therefore
#' the newer format (agd) should be used when possible.
#'
#'
#' @export
read.actigraph.csv <- function(file, DIR="data", get.id = getID, 
   preprocess = delete.zeros, .filter=FILTER, ...){
     filename <- if(missing(DIR)) file else paste(DIR, file, sep="/") 
     if(!file.exists(filename)) stop("File ", filename, " does not exist")
   HD <- read.actigraph.header(filename, Way="csv")
   HD$Errors <- new.env(hash=FALSE, parent=emptyenv())
   Model <- HD$Model
   Mode <- HD$Mode
   agm <- actigraphmode(Mode, Model)
   SKIP <- HD$HL
   # uuemas versioonis on csv faili esimeses reas
   # tunnuste nimed ... need algavad Axis1, ...
   first <- readLines(filename, HD$HL+1)[HD$HL+1]
   first <- strsplit(first, ",")[[1]]
   if(all(is.na(suppressWarnings(as.numeric(first))))) SKIP <- SKIP + 1
   X <- utils::read.csv(filename, skip=SKIP, header=FALSE)
   names(X) <- agm$names
   
   HD$Axes <- agm$names[grep("Activity", agm$names)]
   HD$ID <- get.id(file)
   X <- .filter(X, HD, HD$Errors) # changes arg.!!
   resu <- list(HD=HD, X=X)
   resu <- preprocess(resu)
   class(resu) <- c("acc")
   resu
   }

########################################################################
#' Process an actigraph file header
#'
#' Reads in the header part of an actigraph 'dat' file and 
#' does the necessary text processing. Used in read.actigraph.dat.
#'
#' @param filename the file to read 
#' @param dateformat - the order of "month" "day" and "year" in date string given as a character vector e.g c("month", "date", "year")
#' @param id.prefix - prefix to be added to the id 
#' @param format TRUE
#' @param h FALSE
#' @param Way "ActiGraph" or "CSA"
#' @param return.l FALSE
#' @return a list of processed header data
#' @seealso \code{\link{read.actigraph.dat}}
#' 
#' @details For processing the headers of older actigraph files in "dat" format.
#'
#'
#' @export
read.actigraph.header <- function(filename, dateformat=c("month", "day", "year"), 
    id.prefix="", format=TRUE, h=FALSE, Way="ActiGraph", return.l = FALSE){
    # auxiliary functions
    SPLIT <- function(..., BY=" ") unlist(strsplit(..., BY))
    CTD   <- function(x) as.numeric(gsub("[[:punct:]]|[[:space:]]", "\\.", x))
    STD <- function(x) gsub("[[:punct:]]", "\\.", x)
    get.time <- function(x, f=c("hour", "min", "sec")) structure(as.integer(SPLIT(x, "[[:punct:]]")), names=f)
    get.date <- function(x, f=dateformat) {
            vec <- SPLIT(STD(x), "\\.")
            month.abb <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
            if(vec[2] %in% month.abb){
                    vec[2] <- which(month.abb %in% vec[2])
                    f <- c("day", "month", "year")
                    if(nchar(vec[3])==2) vec[3] <- paste(if(as.integer(vec[3])<80) "20" else "19", vec[3], sep="")
                    }
            structure(as.integer(vec), names=f)
            }
    my.dt <- function(t,d) do.call(ISOdatetime, as.list(c(d,t)))
 
    fc <- file(filename)
    open(fc)
    l <- character(50)
    l[1] <- readLines(fc,1)
    i <- 1
 
    repeat {
       l[i<-i+1] <- readLines(fc,1)
       if(substr(l[i],1,10)=="----------") break
       }
 
    close(fc)
    l <- l[1:i]
    LH <- i

    ### the following for reading headers of ActiGraph csv files
    ### 
    if(Way %in% "csv") l<-gsub(",","",l)
 
    if(h) cat("\n-- ## 1 ##  --\n")
    if(h) print(l)
 
    l <- paste(l, collapse="\n")
    l <- strsplit( sub("(Mode[[:space:]]+)", "\n\\1", l), "\n")[[1]]
 
    l <- lapply(strsplit(l, " "), function(x) x[x!=""])
    l <- lapply(l,  function(x) sub("-{2,}", "", x))
    l <- lapply(l, function(x) x[x!=""])
    l <- l[sapply(l, length)!=0]
    l1<- l[[1]]

          wm <- which(l1%in% "By")
          if(length(wm)>0) {
                  MODEL <- if(l1[wm+1] %in% "ActiGraph") l1[wm+2] else l1[wm+1]
                  } else MODEL <- NA
          wa <- which(l1%in%"ActiLife")
          ACTILIFE <- if(length(wa)>0) l1[wa+1] else NA
          wf <- which(l1%in%"Firmware")
          FIRMWARE <- if(length(wf)>0) l1[wf+1] else NA
    #    MODEL <- l[[1]][5]
 
    if(Way %in% "CSA") {
            l[[8]]<-l[[8]][1:4]
            l1 <- l[[1]]
            l2 <- l[-1]
            l1 <- list(c("Serial", "Number", sub("SN:", "", l1[1])), c("", l1[-1]))
            MODEL <- "CSA"
            l <- c(l1, l2, list(c("Mode", "", "=")))
            FIRMWARE <- l[[2]][3]
            }
 
    l <- sapply(l, function(x)  structure(x[length(x)], names=gsub("\\W", "", paste(x[1:2], collapse=""))))
    if(l["Mode"]=="=") is.na(l["Mode"]) <- TRUE
    l[length(l)+1] <- LH
 
    if(h) cat("\n-- ## 2 ## --\n")
    if(h) print(l)
    if(h) cat("\n-- ## 3 ## --\n")

    if(return.l) return(l)

    HD <- list( File = basename(filename),
        Serial  = unname(l["SerialNumber"]),
        Model   = MODEL,
        ActiLife= ACTILIFE,
        Firmware= FIRMWARE,
        Start   = my.dt( get.time(l["StartTime"]), get.date(l["StartDate"]) ),
        Epoch   = sum(get.time(l["EpochPeriod"])*c(24*60,60,1)),
        Down    = my.dt( get.time(l["DownloadTime"]), get.date(l["DownloadDate"]) ),
        Address = as.integer(l["CurrentMemory"]),
        Voltage = CTD(l["CurrentBattery"]),
        Mode    = as.integer(l["Mode"]),
        HL      = LH   
        )
    toobad <- FALSE
    anysna <- any(is.na( c(HD$Start, HD$Down)))
    if(!anysna) toobad <- difftime(HD$Down, HD$Start, units="days") > 35
    if(!anysna && !toobad) toobad <- HD$Down < HD$Start
#    & ( | HD$Down < HD$Start)) HD[c("Start", "Down")] <- guess.dates( get.date(l["StartDate"]), get.time(l["StartTime"]), get.date(l["DownloadDate"]), get.time(l["DownloadTime"])) 
    if(anysna || toobad) HD[c("Start", "Down")] <- guess.dates( get.date(l["StartDate"]), get.time(l["StartTime"]), get.date(l["DownloadDate"]), get.time(l["DownloadTime"])) 
    if(format||h) HD else l
 }
 
 
#' Guess dates 
#'
#' Date format guessing used in read.actigraph.dat and read.csa
#'
#' @param d1 date 1
#' @param t1 time 1
#' @param d2 date 2
#' @param t2 time 2
#' @param START start date of study 
#' @param END end date of study
#' @return an object (list) of class "acc"
#' @seealso \code{\link{read.actigraph.csv}}
#' 
#' @details For reading in older actigraph files in "dat" format.
#' The main shortcoming of this format is ambiguity of date format -
#' it is dependent on computer used to download the data. Therefore
#' the newer format (agd) should be used when possible.
#'
#'
#' @export
guess.dates <- function (d1, t1, d2, t2, 
    START = ISOdate(2007,8,31), END = ISOdate(2008,7,1)) {
    my.dt <- function(t,d) do.call(ISOdatetime, as.list(c(d,t)))
    d1 <- unname(d1)
    d2 <- unname(d2)
    YR <- which(d1>1000 & d2 > 1000)
    yr1<- d1[YR]
    yr2<- d2[YR]
    d1 <- d1[-YR]
    d2 <- d2[-YR]
 
    g1 <- list(
       my.dt( t1, c(year=yr1, month=d1[1], day=d1[2])),
       my.dt( t2, c(year=yr2, month=d2[1], day=d2[2]))      )
    g2 <- list(
       my.dt( t1, c(year=yr1, month=d1[2], day=d1[1])),
       my.dt( t2, c(year=yr2, month=d2[2], day=d2[1]))      )
 

    # if one is incorrect return the other
    if(any(is.na(g1))) return(g2)
    if(any(is.na(g2))) return(g1)

    # download date must be later than start date; check if this is violated
    if(g1[[2]]< g1[[1]]) return(g2)
    if(g2[[2]]< g2[[1]]) return(g1)
 
    # check now the length of periods -- prefer a shorter one
    MAGICK <- 50
    g1L <- -do.call(difftime, c(g1, units="days"))
    g2L <- -do.call(difftime, c(g2, units="days"))
    if(g1L> MAGICK) return(g2)
    if(g2L> MAGICK) return(g1)

    # possibly add length of data as an additional criterion
    # and maybe g1L > g2L something?
 
    # if year is first, it'll be YYYY-MM-DD
    if(YR==1) return(g1)
 
    # the default ones are IDEFICS-specific
    cat("\n**** WARNING!!! WARNING!!! date guessing reached level 3 ***\n")
    if(g1[[2]]>END) return(g2)
    if(g2[[2]]>END) return(g1)
 
    if(g1[[1]]<START) return(g2)
    if(g2[[1]]<START) return(g1)
 
    if(g1[[2]]-g1[[1]] > g2[[2]]-g2[[1]]) return(g2)
    if(g1[[2]]-g1[[1]] < g2[[2]]-g2[[1]]) return(g1)
 
    g1
 }

###########################################################################
###########################################################################

#' Read in a 3DNX (TM) file  
#'
#' Reads in a 3DNX file and
#' returns an object of class "acc" that can be further
#' processed with summary.acc etc.
#'
#' @param file the file to read 
#' @param DIR folder - added to 'file' but ignored if 'file' contains '/'
#' @param METHOD rowSums 
#' @param default.epoch what to do if epoch is missing from file 
#' @param ask.if.no.epoch FALSE
#' @param get.id getID
#' @param preprocess delete.zeros
#' @param method "sum"
#' @param ... args 2b passed
#' @return an object (list) of class "acc"
#' @seealso \code{\link{read.actigraph.csv}}
#' 
#' @details For reading in 3dnx files 
#'
#'
#' @export
read.3dnx <- function (file, DIR, METHOD = rowSums, 
    default.epoch=5, ask.if.no.epoch=FALSE, get.id=getID, 
    preprocess=delete.zeros, method="sum", ...) {
    ########## MAR-10 ... added to argslist to make summary work that gave the fwg error
    ##### Error in FUN(file = file, dir = dir, id.prefix = id.prefix, preprocess = preprocess,  : 
    ####  unused argument(s) (id.prefix = id.prefix, preprocess = preprocess, dateformat = dateformat)
    ### > summary(read.3dnx("IV17.txt", "gla3"), STATS=list(.CNTSTATS), preprocess=delete.zeros)
    ### Read 958491 items
    ### Error in seq.int(from, by = by, length.out = length.out) : 
    ###  'from' must be finite
 
#   filename <- if(regexpr("/", file)== -1) paste(dir, file, sep="/") else file
     filename <- if(missing(DIR)) file else paste(DIR, file, sep="/") 
     if(!file.exists(filename)) stop("File ", filename, " does not exist")
   RAW <- scan(filename, what = character(), quiet=TRUE)
    if (length(RAW) <= 150) 
        stop("Seems to be an empty file: ", filename)
    HeaderStart <- which(RAW[1:150] %in% "START")
    if (length(HeaderStart) == 0) 
        stop("Strange file, can't read it! ", filename)
    if (HeaderStart[1] > 1) {
        HS1 <- HeaderStart - 1
        RAW <- RAW[-(1:HS1)]
        HeaderStart <- which(RAW[1:150] %in% "START")
    }
    while (length(HeaderStart > 1)) {
        HS1 <- utils::tail(HeaderStart, 1) - 1
        if (HS1 > 0) {
            RAW <- RAW[-(1:HS1)]
            HeaderStart <- which(RAW[1:150] %in% "START")
        }
        else break
    }
    HeaderEnd <- which(RAW[1:150] == "[dVolts]")[1]
    CONT <- match(c("Session", "DMY", "DMY", "DMY", "HMS", "HMS", 
        "HMS", "Vbat", "Snr.", "Interval"), RAW[1:50])
    ADDO <- c(1, 1, 2, 3, 1, 2, 3, 1, 1, 1)
    NAMZ <- c("Session", "SDay", "SMonth", "SYear", "SHour", 
        "SMin", "SSec", "SVoltage", "Serial", "Epoch")
    HD1 <- structure(RAW[CONT + ADDO], .Names = NAMZ)
    lr <- length(RAW)
    FooterEnd <- utils::tail(which(RAW[(lr - 151):lr] %in% "[dVolts]"), 
        1)
    NH <- length(FooterEnd) > 0
    if (length(FooterEnd) > 0) 
        if (FooterEnd != 150) {
            RAW <- RAW[-((lr - 151 + FooterEnd):lr)]
            lr <- length(RAW)
        }
    SDS <- which(RAW[(lr - 150):lr] %in% "STOP")
    FooterStart <- if (length(SDS) == 0) 
        lr + 1
    else lr - 151 + SDS
    HD2 <- RAW[FooterStart:lr]
    CONT <- match(c("DMY", "DMY", "DMY", "HMS", "HMS", "HMS", 
        "Vbat"), HD2)
    ADDO <- c(1, 2, 3, 1, 2, 3, 1)
    NAMZ <- c("EDay", "EMonth", "EYear", "EHour", "EMin", "ESec", 
        "EVoltage")
    HD2 <- structure(HD2[CONT + ADDO], .Names = NAMZ)
    HD1 <- c(HD1, HD2)
    x <- as.integer(RAW[(HeaderEnd + 1L):(FooterStart - 1L)])
    if (is.na(HD1["Serial"])) {
#       warning("The file ", filename, " has no serial number in it.")
        HD1["Serial"] <- 0
        }
    if (is.na(HD1["Epoch"])) {
#       warning("The file ", filename, " has no epoch in it.")
        HD1["Epoch"] <- default.epoch
        if(ask.if.no.epoch) {
             foo <- readline(paste("Enter epoch for ", filename, " >> "))
             HD1["Epoch"] <- as.integer(foo)
             }
        }
    get.dt <- function(vec) {
        l <- as.list(vec)
        names(l) <- c("year", "month", "day", "hour", "min", 
            "sec")
           # correct "zero days" and months as was in gla3/IV17.txt
           if(l$day %in% "00") l$day<-1
           if(l$month %in% "00") l$month<-1
        l$year <- paste("20", l$year, sep = "")
        do.call(ISOdatetime, l)
        }
    HD <- list(File = basename(filename), ID = get.id(file), Serial = unname(HD1["Serial"]), 
        Model = "3DNX", Start = get.dt(HD1[c("SYear", "SMonth", 
            "SDay", "SHour", "SMin", "SSec")]), Epoch = as.integer(HD1["Epoch"]), 
        Down = get.dt(HD1[c("EYear", "EMonth", "EDay", "EHour", 
            "EMin", "ESec")]), Address = "", Voltage = c(as.numeric(HD1["SVoltage"]), 
            as.numeric(HD1["EVoltage"])), Session = unname(HD1["Session"]), 
        Axes = 1:3)
    if(length(x)%%4>0) warning("Something suspicious in ", filename)
    MAT <- matrix(x, ncol=4, byrow = TRUE)
       bar <- abs(diff(abs(MAT[,1])))
       baa <- which(bar>50)
       if(length(baa)!=0) {
              cat("\nNow these lines in ", filename, "are a bit funny:\n")
              leba <- if(length(baa)>10) 10 else length(baa)
              if(length(baa)>10) print(baa[1:10]) else print(baa)
              for(i in 1:leba) print(MAT[(baa[i]-1):(baa[i]+1),])
              if(length(baa)>10) cat("... and some more ...\n")
              cat("\nPlease find these lines in the original file and try to correct them manually.\n")
              }
    Activity <- MAT[, -1]
    colnames(Activity) <- c("X", "Y", "Z")
    X <- as.data.frame(Activity)
    ## now follows a hack to make the summary method work
    RES <- structure(list(RH = HD1, HD = HD, X = X), class=c("acc", "acc3d"))
    names(RES$X) <- paste("Activity", 1:3, sep="")
    RES <- preprocess(RES)
    RES$X$Activity1 <- get.counts(x, method)
    RES
}

############################################################################
############################################################################
############################################################################

#' Get numberic ID from file name
#'
#' Get ID from file name. All numbers (digits) in file name
#' will be treated as ID; letters are ignored.
#' For example, '42abcd.dat' becomes 42.
#'
#' @param x file name (character) 
#' @return ID (as integer) 
#' @export
getID <- function(x) 
  if(is.acc(x)) Recall(get.filename(x)) else 
    as.numeric(gsub("[^0-9]", "", basename(x))) 

#' Get character ID from file name
#'
#' Get ID from file name. File extension will be cut and 
#' everything else written with leading capital letter.
#' For example, 'Elijah Bailey.dat' becomes 'ElijahBailey'
#'
#' @param x file name (character) 
#' @param noblanks TRUE (blanks will be deleted)
#' @return ID (as character) 
#' @export
getNAME <- function(x, noblanks = TRUE) {
  if(is.acc(x)) x <- get.filename(x)
     .simpleCap <- function(x) {
         s <- strsplit(x, " ")[[1]]
         paste(toupper(substring(s, 1,1)), substring(s, 2),
               sep="", collapse=" ")
     }

    x <- gsub("\\.dat$", "", basename(x))
    x <- gsub("[0-9]", "", x)
    x <- sub("[[:blank:]]$", "", x)
    x <- tolower(x)
    x <- sapply(x, .simpleCap)
    if(noblanks) x <- gsub("[[:blank:]]", "", x)
    unname(x)
    }

#' Get ID from file name (2)
#'
#' Get ID from file name: cut off the extension 
#' and treat the rest as ID. 
#' For example, 'Elijah Bailey.dat' becomes 'Elijah Bailey'
#'
#' @param x file name (character) 
#' @return ID (as character) 
#' @export
getRex <- function(x) if(is.acc(x)) Recall(get.filename(x)) else split(basename(x), "\\.")[[1]][1]


#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################

#' 
#'  read.agd 
#' 
#' @param  file file 
#' @param  DIR folder name (but you can include this in file name too) 
#' @param  get.id get.id 
#' @param  getinside getinside
#' @param  query either NULL or a valid SQL query
#' @param limit limit to the number of lines to be read from the file
#' @param  ... ... 
#' @return  read.agd 
#' @export 
read.agd <- function(file, DIR, get.id=getID, getinside = FALSE, limit = NULL, query = NULL,  ...){
  filename <- if(missing(DIR)) file else paste(DIR, file, sep="/") 
  if(!file.exists(filename)) stop("File ", filename, " does not exist")
  
  con <- dbConnect(SQLite(), filename) 
  # Tables <- dbListTables(con)
  Settings <- OSettings <- dbReadTable(con, "settings")
  ro1 <- dbGetQuery(con, "select * from data limit 1")
  nro1 <- names(ro1)
  nro1 <- nro1[-grep("dataTimestamp", nro1)]
  ee1 <- paste(nro1, collapse=", ")
  if(is.null(query)){
    query <- paste("select", ee1, "from data")
    if(!is.null(limit)) query <- paste(query, "limit", limit)
  }
  if(!is.character(query)) stop("Query must be either NULL or character")
  X <- dbGetQuery(con, query)
  if(getinside) browser()
  dbDisconnect(con)
  
  ORIG <- structure(-62135596800, class = c("POSIXct", "POSIXt"), tzone = "GMT")
  # ORIG <- as.POSIXct("1-1-1 0:0:0", "GMT")
  # date mt in agd files http://dl.theactigraph.com/SFT12DOC13%20-%20ActiLife%206%20Users%20Manual%20%28Rev%20A%29.pdf
  # and http://msdn.microsoft.com/en-us/library/system.datetime.ticks.aspx
  todate <- function(x) as.POSIXct(as.numeric(substr(x,1,11)), origin=ORIG, tz ="GMT")
  
  Settings <- structure(as.list(Settings$settingValue), .Names=Settings$settingName)
  critical <- c("startdatetime", "downloaddatetime", "stopdatetime")
  fosc <- Settings[critical]
  fosc[which(fosc %in% "0")] <- NA
  Settings[critical] <- fosc
  Settings[critical] <- lapply(Settings[critical], todate)
  
  names(X) <- sub("axis", "Activity", names(X))
  HD <- list( File = basename(filename),
              Serial  = Settings$deviceserial, 
              Model   = Settings$devicename,
              ActiLife= Settings$softwareversion,
              Firmware= Settings$deviceversion,
              Start   = Settings$startdatetime,
              Epoch   = as.integer(Settings$epochlength),
              Down    = Settings$downloaddatetime,
              Address = as.integer(Settings$addresspointer),
              Voltage = as.numeric(sub(",", "\\.", Settings$batteryvoltage)),
              Mode    = as.integer(Settings$modenumber),
              HL      = NA,
              ID      = filename,
              errors  = new.env(hash=TRUE, parent=emptyenv()),
              Axes    = grep("Activity", names(X)),
              OH      = with(OSettings, structure(settingValue, names=settingName))
  )
  process.acc(structure(list(HD=HD, X=X), class="acc"), get.id=get.id, ...)
}



#'  read.agd_old
#' 
#' 
#'  read.agd_old 
#' 
#' @param  file file 
#' @param  DIR folder name (but you can include this in file name too) 
#' @param  get.id get.id 
#' @param  justso justso 
#' @param  ... ... 
#' @return  read.agd 
#' @export 
read.agd_old <- function(file, DIR, get.id=getID, justso = FALSE, ...){
  filename <- if(missing(DIR)) file else paste(DIR, file, sep="/") 
  if(!file.exists(filename)) stop("File ", filename, " does not exist")
  
  # if(!require(RSQLite)) cat("need package RSQLite, otherwise can't work\n")
  con <- dbConnect(SQLite(), filename) 
  Tables <- dbListTables(con)
  FetchAll <- lapply(Tables, function(x) dbReadTable(con,x))
  names(FetchAll) <- Tables
  
  ORIG <- structure(-62135596800, class = c("POSIXct", "POSIXt"), tzone = "GMT")
  # ORIG <- as.POSIXct("1-1-1 0:0:0", "GMT")
  # date mt in agd files http://dl.theactigraph.com/SFT12DOC13%20-%20ActiLife%206%20Users%20Manual%20%28Rev%20A%29.pdf
  # and http://msdn.microsoft.com/en-us/library/system.datetime.ticks.aspx
  todate <- function(x) as.POSIXct(as.numeric(substr(x,1,11)), origin=ORIG, tz ="GMT")
  
  dbDisconnect(con)
  Settings <- FetchAll$settings
  Settings <- structure(as.list(Settings$settingValue), .Names=Settings$settingName)
  critical <- c("startdatetime", "downloaddatetime", "stopdatetime")
  fosc <- Settings[critical]
  fosc[which(fosc %in% "0")] <- NA
  Settings[critical] <- fosc
  Settings[critical] <- lapply(Settings[critical], todate)
  FetchAll$settings2 <- Settings
  if(justso) return(FetchAll)
  
  X <- FetchAll$data
  X$dataTimestamp <- NULL
  
  names(X) <- sub("axis", "Activity", names(X))
  HD <- list( File = basename(filename),
              Serial  = Settings$deviceserial, 
              Model   = Settings$devicename,
              ActiLife= Settings$softwareversion,
              Firmware= Settings$deviceversion,
              Start   = Settings$startdatetime,
              Epoch   = as.integer(Settings$epochlength),
              Down    = Settings$downloaddatetime,
              Address = as.integer(Settings$addresspointer),
              Voltage = as.numeric(sub(",", "\\.", Settings$batteryvoltage)),
              Mode    = as.integer(Settings$modenumber),
              HL      = NA,
              ID      = filename,
              errors  = new.env(hash=TRUE, parent=emptyenv()),
              Axes    = grep("Activity", names(X)),
              OH      = with(FetchAll$settings, structure(settingValue, names=settingName))
  )
  process.acc(structure(list(HD=HD, X=X), class="acc"), get.id=get.id, ...)
}



#'  process.acc 
#' 
#' 
#'  process.acc 
#' 
#' @param  x x 
#' @param  .filter .filter 
#' @param  preprocess preprocess 
#' @param  get.id get.id 
#' @return  process.acc 
#' @export 
process.acc <- function(x, .filter=FILTER, preprocess=delete.zeros, get.id=NULL){
    # idee: t6stame siia k6ik actigraphi eeltootlused, veaotsimised jne
    # samuti get.id
    # et seda saab siis kasutada iga tyypi failidega
    # praegu kasutab seda read.agd
    X <- x$X
    HD <- x$HD
    Errors <- new.env(hash = FALSE, parent = emptyenv())

    if(NROW(X) > 5) X <- .filter(X, HD, Errors)
    HD$End <- HD$Start + (nrow(X) - 1) * HD$Epoch
    HD$ID <- unname(if(is.function(get.id)) get.id(HD$File) else if(!is.null(HD$OH)) HD$OH["subjectname"] else HD$File)
    HD$Errors <- Errors
    resu <- list(HD = HD, X = X)
    if(NROW(X) > 5) resu <- preprocess(resu)
    class(resu) <- c("acc")
    resu
}

#####################################################################################
#####################################################################################
#'  read.actigraph 
#' 
#' 
#'  read.actigraph 
#' 
#' @param  .. a file
#' @param  ... args to be passed
#' @return  an object of class 'acc'
#' @export 
read.actigraph <- function(.., ...) {
    ext <- file.ext(..)
    if(ext=="dat") read.actigraph.dat(..,...) else if(ext=="csv") read.actigraph.csv(..,...) else if(ext=="agd") read.agd(..,...) else "unexpected file format"
    }
