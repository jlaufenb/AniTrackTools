
#' Convert GPS Data to moveData Class from moveHMM Package
#'
#' @param gps_df Data.frame containing GPS data following output format of csv2df function
#'
#' @return R object of class moveData
#' @export
#'
#' @examples
#' \dontrun{
#' gps_df <- import_telcsv(file = "700516A Complete.csv", nskip = 23)
#' make.moveData(gps_df)}

make.moveData <- function(gps_df){
    moveHMMdata = moveHMM::prepData(data.frame(ID = gps_df$animal_id,
                                      x = gps_df$lon,
                                      y = gps_df$lat,
                                      collar_id = gps_df$collar_id,
                                      fixtime = gps_df$fixtime,
                                      fixtype = gps_df$fixtype,
                                      fixsched = gps_df$fixsched), type = "LL")
    attributes(moveHMMdata$step) = list(units = "km")
    attributes(moveHMMdata$angle) = list(units = "radians")
    return(moveHMMdata)
}
