# A few infernal variables and functions
# The following to avoid being NOTE'd on no visible binding (i.e to be able to use subset in fns)

#' Internal accelerate functions
#'
#' Infernal variables
#' @aliases validEpochs Wkdy val.time
validEpochs <- Wkdy <- val.time <- Epoch <- "Boo boo"

#'Curry with brackets
#'
#'Curry with brackets
#'
#'
#'@param FUN function
#'@param ... dots 
#'@return function
#'@rdname Curry
#'@export
Curry <- function(FUN,...) { 
   # by Byron Ellis, https://stat.ethz.ch/pipermail/r-devel/2007-November/047318.html
   .orig <- list(...)
   function(...) do.call(FUN,c(.orig,list(...))) 
   }

#'@rdname Curry
#'@method [ function
#'@export
`[.function` <- Curry



#'Extract header
#'
#'Extract header 
#'
#'
#'@param x 'acc object'
#'@return x$HD
#'@keywords internal
Header <- function(x) x$HD
`Header<-` <- function(x, attr, value) { if(attr=="ALL") x$HD <- value else x$HD[[attr]] <- value; x}

#'Extract data
#'
#'Extract data
#'
#'
#'@param x 'acc object'
#'@return x$X
#'@keywords internal
Data <- function(x) if(is.data.frame(x)) x else x$X 
`Data<-` <- function(x, attr, value) { if(attr=="ALL") x$X <- value else x$X[[attr]] <- value; x}



#'Get errors
#'
#'Get errors
#'
#'
#'@param x 'acc object'
#'@return errors
#'@keywords internal
get.errors <- function(x){
    Errs <- x$HD$Errors
    ErrNames <- c("HRMin", "HRMax", "HR", "Stepsgta", "StepMin", "StepMax", "Steps", 
                  "ActMin", "ActMax", "Activity", "Pad", "Negcounts")
    R1 <- sapply(ErrNames, function(x) Errs[[x]])
    R2 <- as.data.frame(lapply(R1, function(x) if(!length(x)) NA else x))
    TotErr <- any(c(R2$HRMin < 0, R2$StepMin<0, R2$Steps>0, R2$HR>0, R2$Activity>0, R2$Negcounts>0, R2$ActMin<0), na.rm=TRUE)
    names(R2) <- paste("Err", names(R2), sep="")
    R2$AnyErr <- TotErr
    R2
    }

#'Get counts
#'
#'Get counts
#'
#'
#'@param x 'acc object'
#'@param method method to get counts
#'@return counts
#'@keywords internal
get.counts <- function(x, method="first"){
    # this is meant to get VERTICAL counts from acc object
    # ... or sumcounts of a 3d acc object
    X <- Data(x)
    act <- grep("Activity", names(X))
    counts <- if(method=="first") X$Activity1 else 
            if(method=="sum") rowSums(X[,act]) else 
            if(method=="sumsq") sqrt(rowSums(X[,act]^2)) else X[[method]]
    counts
    }

#'Get HR
#'
#'Get HR
#'
#'
#'@param x 'acc object'
#'@return heart rate
#'@keywords internal
get.hr <- function(x){
    # get heart rate from an acc object
    X <- Data(x)
    HR <- if("HR" %in% colnames(X)) X[,"HR"] else numeric(0)
    HR
    }

#'Get steps
#'
#'Get steps
#'
#'
#'@param x 'acc object'
#'@return steps
#'@keywords internal
get.steps <- function(x){
    X <- Data(x)
    colnames(X) <- tolower(colnames(X))
    steps <- if("steps" %in% colnames(X)) X[,"steps"] else numeric(0)
    steps
    }

#'Get filename
#'
#'Get filename
#'
#'
#'@param x 'acc object'
#'@return filename
#'@keywords internal
get.filename <- function(x) Header(x)$File


#'Is acc?
#'
#'Is acc? 
#'
#'
#'@param x an object
#'@return logical
#'@keywords inter
#'@export
is.acc <- function(x) inherits(x, "acc")

#'Round upwards
#'
#'Round upwards
#'
#'
#'@param x numeric 
#'@return rounded value
#'@keywords internal
roundup <- function(x){
   foo <- max(which(x/10^(1:10)>1))
   ceiling(x/10^foo)*(10^foo)
   }

#'iCurry
#'
#'iCurry
#'
#'
#'@param FUN function
#'@param x x
#'@param ... dots
#'@return function with na.rm=T if possible
#'@keywords internal
iCurry <- function(FUN, x, ...)  if(!length(x)) NA else if(all(is.na(x))) NA else FUN(x, ...)


#' Load save'd objects and return them as a list 
#'
#' Obects in `file` are loaded and returned as a value (instead of 
#' creating these objects in the global environment). The value can be
#' a list (default) or an environment (if to.list is FALSE); if there
#' is just one object in the file to be read, it is by default unlisted
#' and just the objects' value is returned.
#'
#' @param file the file to load
#' @param to.list if TRUE, a list is returned, otherwise an environment
#' @param Unlist whether to "unlist" length 1 lists (default is TRUE)
#' @param spray whether to create these objects in the global environment (default is FALSE)
#' @param delete.parent if an environment is returned, whether to set its parent env to an empty environment (to make a smaller object)
#' @return a list or environment (depending on to.list) or other object (if there is just one object saved and Unlist equals TRUE)
#' @seealso \code{\link{load}}
#' 
#' @details This is useful to avoid overwriting objects in your workspace, 
#' for using only parts of a file, and inspecting the contents of files 
#' before you load them. You can set your own names to the objects instead
#' of using the ones from the file. E.g., 
#'
#' \code{new.name <- Load("manyobjects.rda")$old.name }
#' \code{ls(Load("manyobjects.rda"))}
#'
#' @export
Load <- function(file, to.list=TRUE, Unlist = TRUE, spray = FALSE, delete.parent = FALSE){
     NE <- if(spray) .GlobalEnv else new.env()
     load(file, NE)
     if(to.list){
         NE <- as.list(NE)
         if(Unlist & length(NE) == 1) NE <- NE[[1]]
         }
     if(delete.parent) parent.env(NE)<-emptyenv()
     NE
     }


#'loadIn
#'
#'loadIn 
#'
#'
#'@param .FILE a file 
#'@return list
#'@keywords internal
loadIn <- function(.FILE){
   load(.FILE)
   .RES <- lapply(ls(), get, envir=environment())
   if(length(.RES)==1) .RES <- .RES[[1]] else names(.RES)<-ls()
   .RES
   }


#'actigraphmode
#'
#'actigraphmode
#'
#'
#'@param MODE mode
#'@param MODEL model
#'@return actigraphmode
#'@keywords internal
actigraphmode <- function(MODE, MODEL){
    if(is.na(MODE)) return(list(names="Activity1", cols=1, used=1))
    Graph <- MODEL %in% c("GT1M", "GT3X", NA)
    Trainer <- MODEL %in% c("ActiTrainer")
    foo <- do.call(rbind,strsplit(sort(levels(do.call(function(...) interaction(..., sep=""), replicate(6,0:1, simplify=FALSE)))), ""))
    # foo <- cbind(0:63, foo)
    bar <- c("Activity2", "Activity3", "Steps", "HR", "Lux", "Incline")[order(c(4, 3, 6, 5, 2, 1))]
    zaz <- c(4, 3, 6, 5, 2, 1)
    res <- c("Activity1", bar[zaz][foo[MODE+1,zaz]==1])
    usedcols <- length(res)
    cols <- if(Trainer) usedcols+2 else usedcols
    list(names=res, cols=cols, used = usedcols)
    }


#'version
#'
#'version
#'
#'
#'@param x x 
#'@return ver 
#'@keywords internal
ver <- function (x) {
   if(substr(x,1,1) %in% "v") x <- substr(x,2,nchar(x))
   x <- if(!is.na(x)) as.integer(strsplit(x, ".", fixed=TRUE)[[1]]) else x
   structure(x, class="ver")
   }

#'is.new.ver
#'
#'is.new.ver
#'
#'
#'@param Firmware firmware
#'@return is.new ver
#'@keywords internal
is.new.ver <- function(Firmware) {
   if(is.null(Firmware)) return(FALSE)
   v <- ver(Firmware)
   if(is.na(v) || length(v)==0) return(FALSE)
   if(length(v)!=3) stop("Srange firmware version, can't make sense of it: ", Firmware)
   if(v[1]>6) return(TRUE)
   if(v[1]<6) return(FALSE)
   if(v[2]>1) return(TRUE)
   if(v[2]<1) return(FALSE)
   TRUE
   }
   

