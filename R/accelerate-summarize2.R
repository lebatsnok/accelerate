#' Summarize2 - by weekdays and weekends
#' 
#'  
#' Cf summarize
#'
#' @param x a data frame, result from read.actigraph.folder or similar 
#' @param reqdays vector or list with elements named wd and we - required days
#' @param cutoffs character (name of the set of cutoffs to be used)
#' @param excl.rel exclude "relative" times in activity categories
#' @param epoch epoch 
#' @param minhrs minimum required hours
#' @param include2 include2
#' @return a df 
#' @seealso \code{\link{summarize}}
#
#  summarize2(foo, c(we=1, wd=1), "Evenson|Ghent")#' 
#' @details Like summarize but see ortega et al in PLOS!
#'
#'
#' @export


summarize2 <- function(x, reqdays = c(wd=2, we=1),  
                       cutoffs, excl.rel=TRUE, epoch, minhrs,
                       include2 =c("Period", "Length", "Wkdy", "avg.cpm", "tot.cnts", "val.time", "permax")
                       ){
  
  reqdays <- as.list(reqdays)
  # leave out the ones with wrong epoch
  if(!missing(epoch)) x <- subset(x, Epoch==epoch)
  
  # leave out the ones with less than XXX hours of recording time
  if(!missing(minhrs)) x <- subset(x, val.time>=(minhrs * 60) )
  
  if(length(cutoffs)>1) cutoffs <- paste(cutoffs, collapse="|")
  cudo <- names(x)[grep(cutoffs, names(x))]
  if (excl.rel & length(grep("\\.1$", cudo)>0)) 
    cudo <- cudo[-grep("\\.1$", cudo)]

  
  start = c("ID", "Epoch", "Model", "Serial")
  vars <- c(include2, cudo, "STEPS")
  vars <- vars[which(vars %in% names(x))]
  
  x <- x[,c(start,vars)]
  
  
  xWD <- summarize(x, Wkdy %in% 1:5, no.days=reqdays$wd)
  xWE <- summarize(x, !(Wkdy %in% 1:5), no.days=reqdays$we)[-(2:4)]
  xa <- merge(xWD, xWE, by="ID", all=FALSE, suffixes=c("wd", "we"))
  
  for(iii in vars){
    nwd <- paste0(iii, "wd")
    nwe <- paste0(iii, "we")
    if(!is.numeric(xa[[nwd]]) | !is.numeric(xa[[nwe]])) next
    isok <- xa$DAYSwd>=reqdays$wd & xa$DAYSwe>=reqdays$we 
    xa[[iii]] <- ifelse(isok , (xa[[nwd]]* 5 + xa[[nwe]]*2)/7, NA)
    }
    xa$DAYS <- xa$DAYSwe + xa$DAYSwd

  xa

}


