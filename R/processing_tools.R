


#' Convert Telonics CSV to AniTrackTools Format
#'
#' @param telcsv Value from \code{import_telcsv} or data.frame with column names matching those from Telonics CSV file produced by the Telonics Data Converter software.
#' @param animal_ids A data.frame matching CTNs (\code{animal_ids$collar_id}) with animal IDs (\code{animal_ids$animal_id})
#'
#' @return A data.frame formatted for use by other functions in the \code{AniTrackTools} package
#' @export
#'
#' @examples
#' \dontrun{
#' gps_df <- import_telcsv(file = "700516A Complete.csv", nskip = 23)
#' telcsv2ATT(telcsv = gps_df, animal_ids = data.frame(collar_id = "700516A", animal_id = "lynx_1"))}

telcsv2ATT <- function(telcsv = NULL, animal_ids = NULL){
    ti_names = c("Collar_CTN","GPS_Fix_Time","GPS_Fix_Attempt","GPS_Latitude","GPS_Longitude",
                 "GPS_Horizontal_Dilution","GPS_Satellite_Count","Schedule_Set")
    att_names = c("collar_id","fixtime","fixtype","lat","lon","hdop","nsats","fixsched")
    if(!is.null(telcsv)){
        telcsv = telcsv[,names(telcsv) %in% ti_names]
        indz = match(names(telcsv),ti_names)
        names(telcsv) = att_names[indz]
        if(length(att_names[-indz])!=0)
            warning(paste0("The following columns were missing from input: ",
                           paste0(att_names[-indz], collapse = ", ")))
    }
    if(!is.null(animal_ids)){
        telcsv$animal_id = animal_ids$animal_id[match(telcsv$collar_id, animal_ids$collar_id)]
    }else{
        telcsv$animal_id = NA
        warning("Animal IDs were not provided, animal_id column filled with NA values.")
    }
    telcsv = telcsv[!is.na(telcsv$animal_id),c(ncol(telcsv),1:(ncol(telcsv)-1))]
    return(telcsv)
}





#' Convert GPS Data to moveData Class from moveHMM Package
#'
#' @param att Data.frame containing GPS data following output format of \code{import_telcsv} function
#' @param by Character string containing name of column to be used to partition moveHMM data. Default is set to \code{animal_id}
#'
#' @return R object of class moveData
#' @export
#'
#' @examples
#' \dontrun{
#' gps_df <- import_telcsv(file = "700516A Complete.csv", nskip = 23)
#' att_df <- telcsv2ATT(telcsv = gps_df, animal_ids = data.frame(collar_id = "700516A", animal_id = "lynx_1"))
#' make.moveData(att)}

make.moveData <- function(att, by = "animal_id"){
    mins = c(by,"lon","lat")
    indz = mins %in% names(att)
    if(any(!indz))stop(paste0("Warning: Input missing the following required columns: ",
                              paste0(mins[!indz], collapse = ", ")))
    moveHMMdata = moveHMM::prepData(data.frame(ID = att[,which(names(att) %in% by)],
                                               x = att$lon,
                                               y = att$lat,
                                               att), type = "LL")
    attributes(moveHMMdata$step) = list(units = "km")
    attributes(moveHMMdata$angle) = list(units = "radians")
    return(moveHMMdata)
}
