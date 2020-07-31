
#' Calculate Daily Cumulative Distance Traveled
#'
#' @param moveData R object of class moveData from \code{moveHMM}
#'
#' @return Data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' telcsv <- import_telcsv(file = "700516A Complete.csv", nskip = 23)
#' att <- telcsv2ATT(telcsv = telcsv, animal_ids = data.frame(collar_id = "700516A", animal_id = "lynx_1"))
#' moveData <- make.moveData(att)
#' calc.cdist(moveData)}

calc.cdist <- function(moveData){
    date1 = min(as.Date(moveData$fixtime))
    date2 = max(as.Date(moveData$fixtime))
    daterange = as.character(seq(date1, date2, by="day"))
    moveData$date_factor = as.Date(as.factor(moveData$fixtime))
    cd = data.frame(animal_id = moveData$ID[1], date = daterange, step = NA, cdist = NA)
    dayind = match(as.character(unique(moveData$date_factor)), daterange)
    cd$step[dayind] = stats::aggregate(moveData$step, by = list(date = moveData$date_factor), FUN = sum)$x
    cd$cdist[!is.na(cd$step)] = cumsum(cd$step[!is.na(cd$step)])
    cd$date = as.Date(cd$date)
    return(cd)
}

#' Plot Daily Cumulative Distance Traveled
#'
#' @param cd Data.frame containing daily cumulative distance traveled generated from \code{calc.cdist} function.
#' @param ymax Max limit for y axis. Scale is km.
#'
#' @return plot
#' @export
#'
#' @examples
#' \dontrun{
#' gps_df <- import_telcsv(file = "700516A Complete.csv", nskip = 23)
#' att_df <- telcsv2ATT(telcsv = gps_df, animal_ids = data.frame(collar_id = "700516A", animal_id = "lynx_1"))
#' moveData <- make.moveData(att_df)
#' cd <- calc.cdist(moveData)
#' cdist.plot(cd, ymax = 4500)}

cdist.plot <- function(cd, ymax=3000){
    graphics::plot.default(cd$date, cd$cdist, type = "p", pch = 21, cex = 0.5, xaxt = "n",
                 ylab = "Distance (km)", xlab = "Date",
                 ylim = c(0,ymax),
                 main = "Cumulative daily distance traveled (km)")
    ndays <- nrow(cd)
    mytickindz = c(seq(1, ndays, by = round(ndays / 8,0)), ndays)
    myticklabs = cd$date[mytickindz]
    graphics::axis(1, at = myticklabs, labels = FALSE)
    graphics::text(x = myticklabs, y = graphics::par("usr")[3] - 100, adj = 1, labels = myticklabs,
         xpd = TRUE, srt = 45, cex = 0.5)
}

#' Write plot of daily cumulative distance traveled to PNG file
#'
#' @param cd Data.frame containing daily cumulative distance traveled generated from \code{calc.cdist} function.
#' @param ymax Max limit for y axis. Scale is km.
#' @param figdir.path Directory path to where figure is written. Default is current working directory.
#'
#' @return PNG file written to working directory or user-specified directory path
#' @export
#'
#' @examples
#' \dontrun{
#' gps_df <- import_telcsv(file = "700516A Complete.csv", nskip = 23)
#' att_df <- telcsv2ATT(telcsv = gps_df, animal_ids = data.frame(collar_id = "700516A", animal_id = "lynx_1"))
#' moveData <- make.moveData(att_df)
#' cd <- calc.cdist(moveData)
#' cdist.fig(cd, ymax = 4500)}

cdist.fig <- function(cd, ymax, figdir.path = getwd()){
	animal_id <- as.character(cd$animal_id[1])
	grDevices::png(paste0(figdir.path, "/Animal_", animal_id, "_CumulStep.png"),
        width = 6.5, height = 6.5, units = "in", res = 192)
    telonicsGPS::cdist.plot(cd = cd, ymax = ymax)
    grDevices::dev.off()
}
