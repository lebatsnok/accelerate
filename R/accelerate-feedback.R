#' do.feedback
#'
#' do feedback
#'
#' @param FILE filename
#' @param DIR dir
#' @param TSDIR a folder where to put the resulting pdf files
#' @param PDF TRUE
#' @param browse FALSE
#' @param PARAMS params
#' @param chg.locale chg.locale
#' @param FUN NULL
#' @export
do.feedback <- function (FILE, DIR, TSDIR, PDF = TRUE, browse = FALSE, PARAMS = "Estonian", 
    chg.locale = FALSE, FUN = NULL) 
{
    PARAMSLIST <- list(Estonian = list(req.time = 360, cutoffs = Cutoffs.Evenson, 
        datatablenames = c("P\u00e4ev", "Istuv", "Kerge", "M\u00f5\u00f5dukas", 
            "Tugev", "MTKA", "Sammud"), locale = "Estonian"), 
        English = list(req.time = 360, cutoffs = Cutoffs.Evenson, 
            datatablenames = c("Day", "Sedentary", "Light", "Moderate", 
                "Vigorous", "MVPA", "Steps"), locale = "English_UK"))
    if (is.character(PARAMS)) 
        PARAMS <- PARAMSLIST[[PARAMS]]
    else if (!is.list(PARAMS)) 
        stop("PARAMS must be either character string or list having 4 prespecified components")
    if (chg.locale) {
         ol <- Sys.getlocale("LC_COLLATE")
         Sys.setlocale("LC_ALL", PARAMS$locale)
         on.exit(Sys.setlocale("LC_ALL", ol))
         }
    textplot <- gplotsTextplotMatrix
    filename <- if (missing(DIR)) 
        FILE
    else paste(DIR, FILE, sep = "/")
    if (!file.exists(filename)) 
        stop("File ", filename, " does not exist")
    acc <- read.actigraph(filename)
    smry <- subset(summary(acc, cutoffs = PARAMS$cutoffs, report.errors = FALSE, 
        STATS = c(CNTSTATS, STEPSTATS), return.rel = FALSE, return.bouts = FALSE), 
        val.time > PARAMS$req.time)
    if (NROW(smry) > 7) 
        fof <- smry[1:7, ]
    if (NROW(smry) == 0) {
        cat(FILE, ": not enough data\n")
        return(invisible(NULL))
    }
    if (!file.exists(sub("/", "", TSDIR))) 
        dir.create(TSDIR)
    if (PDF) {
        grDevices::pdf(paste0(TSDIR, if (file.ext(FILE) != "") 
            sub(file.ext(FILE), "pdf", FILE)
        else paste0(FILE, ".pdf")), paper = "a4")
        on.exit(grDevices::dev.off(), add=TRUE)
        }
    smry.fmtd <- t(smry[, 13:16])
    colnames(smry.fmtd) <- format(smry$Period, "%x")
    old.par <- graphics::par(mfrow = 2:1, mar = c(3, 3, 2, 1), cex.axis = 0.8)
    bars <- graphics::barplot(smry.fmtd, beside = TRUE, col = "black", 
        density = c(0, 10, 30, 100), ylim = c(0, max(smry.fmtd) + 
            120))
    graphics::legend(1, max(smry.fmtd) + 100, legend = PARAMS$datatablenames[2:5], 
        horiz = TRUE, density = c(0, 10, 30, 100), bty = "n")
    graphics::title(FILE)
    usecols <- c("Period", paste(names(PARAMS$cutoffs)[1], c(names(PARAMS$cutoffs[[1]])[-1], 
        "MVPA"), sep = "."), "STEPS")
    smry.fmtd2 <- smry[, usecols]
    smry.fmtd2[, -1] <- lapply(smry.fmtd2[, -1], round)
    names(smry.fmtd2) <- PARAMS$datatablenames
    smry.fmtd2[, 1] <- format(smry.fmtd2[, 1], format = "%a %x")
    textplot(smry.fmtd2, show.rownames = FALSE)
    if (browse) browser()
    if (!is.null(FUN)) 
        FUN(acc, smry)
}


#' gplotsTextplotMatrix
#'
#' gplotsTextplotMatrix
#'
#' @param object object
#' @param halign halign
#' @param valign valign
#' @param cex cex
#' @param cmar 2
#' @param rmar 0.5
#' @param show.rownames TRUE
#' @param show.colnames TRUE
#' @param hadj 1
#' @param vadj 1
#' @param mar 1 1 4 1
#' @param col.data par('col')
#' @param col.rownames par('col')
#' @param col.colnames par('col')
#' @param ... ...
#' @export
gplotsTextplotMatrix <- function (object, halign = c("center", "left", "right"), valign = c("center", 
    "top", "bottom"), cex, cmar = 2, rmar = 0.5, show.rownames = TRUE, 
    show.colnames = TRUE, hadj = 1, vadj = 1, mar = c(1, 1, 4, 
        1) + 0.1, col.data = graphics::par("col"), col.rownames = graphics::par("col"), 
    col.colnames = graphics::par("col"), ...) 
{
    if (is.vector(object)) 
        object <- t(as.matrix(object))
    else object <- as.matrix(object)
    if (length(col.data) == 1) 
        col.data <- matrix(col.data, nrow = nrow(object), ncol = ncol(object))
    else if (nrow(col.data) != nrow(object) || ncol(col.data) != 
        ncol(object)) 
        stop("Dimensions of 'col.data' do not match dimensions of 'object'.")
    if (length(col.rownames) == 1) 
        col.rownames <- rep(col.rownames, nrow(object))
    if (length(col.colnames) == 1) 
        if (show.rownames) 
            col.colnames <- rep(col.colnames, ncol(object) + 
                1)
        else col.colnames <- rep(col.colnames, ncol(object))
    halign = match.arg(halign)
    valign = match.arg(valign)
    opar <- graphics::par()[c("mar", "xpd", "cex")]
    on.exit(graphics::par(opar))
    graphics::par(mar = mar, xpd = FALSE)
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), log = "", asp = NA)
    if (is.null(colnames(object))) 
        colnames(object) <- paste("[,", 1:ncol(object), "]", 
            sep = "")
    if (is.null(rownames(object))) 
        rownames(object) <- paste("[", 1:nrow(object), ",]", 
            sep = "")
    if (show.rownames) {
        object <- cbind(rownames(object), object)
        col.data <- cbind(col.rownames, col.data)
    }
    if (show.colnames) {
        object <- rbind(colnames(object), object)
        col.data <- rbind(col.colnames, col.data)
    }
    if (missing(cex)) {
        cex <- 1
        lastloop <- FALSE
    }
    else {
        lastloop <- TRUE
    }
    for (i in 1:20) {
        oldcex <- cex
        width <- sum(apply(object, 2, function(x) max(graphics::strwidth(x, 
            cex = cex)))) + graphics::strwidth("M", cex = cex) * cmar * 
            ncol(object)
        height <- graphics::strheight("M", cex = cex) * nrow(object) * 
            (1 + rmar)
        if (lastloop) 
            break
        cex <- cex/max(width, height)
        if (abs(oldcex - cex) < 0.001) {
            lastloop <- TRUE
        }
    }
    rowheight <- graphics::strheight("W", cex = cex) * (1 + rmar)
    colwidth <- apply(object, 2, function(XX) max(graphics::strwidth(XX, 
        cex = cex))) + graphics::strwidth("W") * cmar
    width <- sum(colwidth)
    height <- rowheight * nrow(object)
    if (halign == "left") 
        xpos <- 0
    else if (halign == "center") 
        xpos <- 0 + (1 - width)/2
    else xpos <- 0 + (1 - width)
    if (valign == "top") 
        ypos <- 1
    else if (valign == "center") 
        ypos <- 1 - (1 - height)/2
    else ypos <- 0 + height
    x <- xpos
    y <- ypos
    xpos <- x
    for (i in 1:ncol(object)) {
        xpos <- xpos + colwidth[i]
        for (j in 1:nrow(object)) {
            ypos <- y - (j - 1) * rowheight
            if ((show.rownames && i == 1) || (show.colnames && 
                j == 1)) 
                graphics::text(xpos, ypos, object[j, i], adj = c(hadj, 
                  vadj), cex = cex, font = 2, col = col.data[j, 
                  i], ...)
            else graphics::text(xpos, ypos, object[j, i], adj = c(hadj, 
                vadj), cex = cex, font = 1, col = col.data[j, 
                i], ...)
        }
    }
    graphics::par(opar)
}
