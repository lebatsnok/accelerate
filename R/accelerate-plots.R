#' Plot an "acc" object
#'
#' plot.acc
#'
#' @param x x
#' @param BY BY
#' @param cutoffs cutoffs
#' @param c.factor 3
#' @param threshold 360
#' @param fileintitle TRUE
#' @param ... dots
#' @return nothing useful
#' @method plot acc
#' @export
plot.acc <- function(x, BY = NULL, cutoffs = NULL, 
                     c.factor=3, threshold= 60*6, fileintitle=TRUE, ...){
  imapply <- function(...) invisible(mapply(...))
  FORMAT <- "%A, %d. %B %Y"
  
  Epoch <- Header(x)$Epoch
  File <- Header(x)$File
  ID <- Header(x)$ID
  
  if(!is.null(cutoffs)){
    Cutn <- names(cutoffs)
    Cute <- cutoffs[[1]][1]
    Cutoffs <- cutoffs[[1]][-1]
    if(Cute != Epoch) Cutoffs <- Cutoffs / (Cute/Epoch)
    # maybe an aux. function adjust.cutoffs would be usefulll...
    YLIM <- c(0, roundup(utils::tail(Cutoffs,1)*c.factor))
  } else YLIM <- NULL
  
  x <- as.zoo(x)
  anyBY <- !is.null(BY)
  
  if(anyBY) {
    CUTS <- cut(index(x), BY)
    SPLT <- split(x, CUTS)
  } else SPLT <- list(x)
  VT <- sapply(SPLT, function(x) sum(!is.na(x))/(60/Epoch))
  
  SPLT[VT<threshold] <- NULL
  if(length(SPLT)==0) stop("Nothing to plot this time")
  
  TIXc <- "hours"
  if(!anyBY) TIXc <- "6 hours"
  TIXf <- "%H:%M"
  
  my.panel <- function(x, ...) {
    graphics::lines(x, ...)
    graphics::Axis(side = 1, at = TIXa, labels = TIXl)
  }
  
  
  for(i in 1:length(SPLT)){
    Xi <- SPLT[[i]]
    MAIN <- format(index(Xi)[1]+60*60, format=FORMAT)
    # tegelikult on vaja seda, et kui SPLT[i] ja [i-1] on erinevas
    # ajatsoonis, siis liidetakse v6i lahutatakse 1 tund
    # - juhul kui BY == "days"
    MAIN <- paste(if(fileintitle) ID else "", MAIN)
    TIXa <- as.POSIXct(levels(cut(index(Xi), TIXc)))
    TIXl <- format(TIXa, TIXf)
    
    graphics::plot(Xi, ylim = YLIM, main= MAIN, xaxt="n", panel=my.panel, ...)
    if(!is.null(cutoffs)) graphics::abline(h = Cutoffs[-1], col="red")
  }
}


#' pdfplot
#'
#' pdfplot
#'
#' @param x x
#' @param filename filename
#' @param MFROW mfrow
#' @param PAPER a4
#' @param ... dots
#' @return nothing useful
#' @export
pdfplot<- function(x, filename, MFROW = c(3,2), PAPER="a4", ...){
  File <- Header(x)$File
  if(missing(filename)) filename <- sub("\\.dat$", ".pdf", File)
  grDevices::pdf(file=filename, paper=PAPER)
  graphics::par(mfrow= MFROW, oma=c(0,0,2,0), mar=c(3,4,3,2))
  graphics::plot(x, xlab="", fileintitle=FALSE, ...)
  graphics::title(File, outer=TRUE)
  grDevices::dev.off()
}
