#' Choi wearing marking
#'
#' Choi wearing marking 
#'
#' @param ACC object of class "acc"
#' @param COL column name in ACC$X that contains counts (defaults to "Activity1")
#' @param set.na whether to set nonwear time as NA
#' @param add.wmcol whether to add an extra column to ACC$X for wearing marking
#' @param ... extra params passed to choi
#' @return X
#' @details The algorithm implemented in this function is described in 100. Choi L, Liu Z, Matthews CE, Buchowski MS. Validation of accelerometer wear and nonwear time classification algorithm. Med Sci Sports Exerc. 2011 Feb;43(2):357-64. doi: 10.1249/MSS.0b013e3181ed61a3. This function does the preparatory job; the actual algoritm is in `pa_choi`. 
#' @export
wmChoi <- function(ACC, COL = "Activity1", set.na = TRUE, add.wmcol = FALSE,  ...){
  # if set.na == TRUE then "nonwear" time is set to missing
  # otherwise ACC is returned with an extra column called wmChoi
  Epoch <- ACC$HD$Epoch
  if(60%%Epoch!=0) stop("Can't reintegrate to 60s, so can't use Choi algorithm. Sorry.")
  X <- ACC$X[,COL]
  Factor <- 60/Epoch
  if(Factor != 1) {
    foldSums <- function(x, F = Factor, na.rm = FALSE) {
      saba <- length(x)%%F
      if (saba != 0) x <- c(x, rep(NA, F - saba))
      fold <- matrix(x, nrow = F)
      colSums(fold, na.rm = na.rm)
    }
    riiX <- foldSums(X)
  } else riiX <- X
  wm <- pa_choi(riiX, ...)
  if(Factor != 1) wm <- rep(wm, each=Factor)[1:length(X)]
  if(set.na) ACC$X[[COL]][which(wm=="nw")] <- NA 
  if(add.wmcol) ACC$X <- data.frame(ACC$X, wmChoi = wm)
  ACC$HD$Preprocess <- paste("Choi")
  ACC
}


#' Choi algorithm (detection of nonwear time)
#'
#' Choi algorithm (detection of nonwear time)
#'
#' @param ct A vector of counts
#' @param frame 90 
#' @param streamFrame 30
#' @param allowanceFrame 2
#' @param talkie FALSE
#' @details This is the "workhorse" function for wmChoi and is borrowed from PhysicalActivity:::marking (not exported from PhysicalActivity, thus included here as a copy rather than as a dependency)
#' @author Leena Choi <leena.choi@Vanderbilt.Edu>, Zhouwen Liu <zhouwen.liu@Vanderbilt.Edu>, Charles E. Matthews <Charles.Matthews2@nih.gov>, and Maciej S. Buchowski <maciej.buchowski@Vanderbilt.Edu>
#' @return X
#' @export
pa_choi <- function (ct, frame = 90, streamFrame = 30, 
                  allowanceFrame = 2, talkie = FALSE) 
{
  if(talkie) cat("hi there!\nframe is ", frame, "\n", "streamFrame is ", streamFrame, "\n", "allowanceFrame is ", allowanceFrame, "\n")
  if (is.null(streamFrame)) {
    streamFrame = round(0.5 * frame)
  }
  ct1 = ct
  ct[is.na(ct)] = 0
  size = length(ct)
  wearing = rep("nw", size)
  ct_bool = ct > 0
  rowPos <- which(ct_bool)
  # rowPos = Nth(dataVct = ct_bool, value = TRUE)   # PhysicalActivity:::NthOccurance is here as Nth but not necessary to use 
  startpos = rowPos[1]
  endpos = c()
  prev = TRUE
  for (q in 2:(length(rowPos))) {
    if (prev) {
      if (rowPos[q] - rowPos[q - 1] > 1) {
        endpos = c(endpos, rowPos[q - 1])
        startpos = c(startpos, rowPos[q])
      }
    }
    else {
      startpos = c(startpos, rowPos[q])
      prev = TRUE
    }
    if (q == length(rowPos)) {
      endpos = c(endpos, rowPos[q])
    }
  }
  allowancewin = endpos - startpos
  for (r in 1:length(allowancewin)) {
    if (allowancewin[r] < allowanceFrame) {
      usStart = startpos[r] - streamFrame
      usEnd = startpos[r] - 1
      if (usStart <= 0) {
        usStart = 1
      }
      if (usEnd <= 0) {
        usStart = 1
      }
      if (usEnd - usStart == 0) {
        usSignal = "nowearing"
      }
      else {
        if (sum(ct_bool[usStart:usEnd]) > 0) {
          usSignal = "wearing"
        }
        else {
          usSignal = "nowearing"
        }
      }
      dsEnd = endpos[r] + streamFrame
      dsStart = endpos[r] + 1
      if (dsEnd > size) {
        dsEnd = size
      }
      if (dsStart > size) {
        dsStart = size
      }
      if (dsEnd - dsStart == 0) {
        dsSignal = "nowearing"
      }
      else {
        if (sum(ct_bool[dsStart:dsEnd]) > 0) {
          dsSignal = "wearing"
        }
        else {
          dsSignal = "nowearing"
        }
      }
      if (usSignal == "nowearing" & dsSignal == "nowearing") {
        startpos[r] = -1
        endpos[r] = -1
      }
    }
  }
  startpos = startpos[startpos != -1]
  endpos = endpos[endpos != -1]
  gap = startpos[-1] - endpos[1:length(endpos) - 1]
  endgap = endpos[1:length(gap)]
  startgap = startpos[-1]
  endgap[gap <= frame] = NA
  startgap[gap <= frame] = NA
  startgap = c(startpos[1], startgap)
  endgap = c(endgap, endpos[length(gap) + 1])
  newstartpos = startgap[!is.na(startgap)]
  newendpos = endgap[!is.na(endgap)]
  for (w in 1:length(newendpos)) {
    wearing[newstartpos[w]:newendpos[w]] = "w"
  }
  tlen = length(wearing)
  wearing[tlen] <- wearing[tlen - 1]
  wearing[is.na(ct1)] <- NA
  structure(wearing, params=paste0("frame: ", frame, " | streamFrame: ", streamFrame, " | allowanceFrame: ", allowanceFrame, sep=""))
}
