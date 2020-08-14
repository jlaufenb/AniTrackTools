
#' Internal Function to Import Individual Telonics Iridium CSV File
#'
#' @param file Character string containing path and file name of Telonics Iridium CSV file.
#' @param nskip Number of rows to remove from top of Iridium CSV file. This number may vary.
#' @param fix_attempt_keep Character vector containing location quality categories to retain.
#' @param time_units Character string specyfing units used to round GPS_Fix_Time variable. Follows round.POSIXt usage. Default set to "hours".
#'
#' @return Formatted data.frame. NOTE: Time variables are rounded to nearest hour by default.
#'

import_telirid <- function(file,
                   nskip = 23,
                   fix_attempt_keep = c("Resolved QFP", "Resolved QFP (Uncertain)")
                   ){
    ## load file
    x = readLines(file)
    ctn = x[which(grepl("CTN",x))]
    ctn = substr(ctn, regexpr(",", ctn) + 1, nchar(ctn))
    x = utils::read.csv(textConnection(paste0(x[-(1:nskip)],collapse="\n")), stringsAsFactors = FALSE)
    ## reformat column names
    names(x) = gsub("\\.", "\\_", names(x))
    ## add CTN variable
    x$Collar_CTN = ctn
    ## reorder columns
    x = x[,c(ncol(x),1:(ncol(x)-1))]
    ## Convert Schedule_Set variable to factor
    scheds = c("Primary","Auxiliary 1","Auxiliary 2","Auxiliary 3")
    x$Schedule_Set = factor(x$Schedule_Set, levels = scheds) # convert GPS fix type to factor
    ## Convert time variables to POSIXct
    x$Acquisition_Time = as.POSIXlt(x$Acquisition_Time, tz = "UTC", format = "%Y.%m.%d %H:%M:%S")
    x$Acquisition_Start_Time = as.POSIXlt(x$Acquisition_Start_Time, tz = "UTC", format = "%Y.%m.%d %H:%M:%S")
    x$GPS_Fix_Time = as.POSIXlt(x$GPS_Fix_Time, tz = "UTC", format = "%Y.%m.%d %H:%M:%S")
    x$Receive_Time = as.POSIXlt(x$Receive_Time, tz = "UTC", format = "%Y.%m.%d %H:%M:%S")
    ## subset data by GPS_Fix_Attempt and convert GPS_Fix_Attempt to factor
    x = x[x$GPS_Fix_Attempt %in% fix_attempt_keep, ]
    x$GPS_Fix_Attempt = factor(x$GPS_Fix_Attempt, levels = fix_attempt_keep)
    ## Remove duplicate fix times
    x = x[!duplicated(x$GPS_Fix_Time),]
    if(nrow(x)==0){
        warning(paste0("No valid location data for CTN ", ctn, "."))
        x = list(NULL)
    }
    return(x)
}


#' Import Telonics Iridium CSV File(s)
#'
#' @param path Character vector containing path(s) and file name(s) of Telonics Iridium CSV file(s),
#' character string specifying path to folder containing Telonics Iridium CSV file(s), or
#' character string specifying path to root folder containing folders with Telonics Iridium CSV file(s).\cr\cr
#' NOTE: When specifying a root folder within which a search of subfolders is conducted, user must pass \code{recursive = TRUE} as an additional argument.
#' @param csv_pattern Character string specifying text pattern used to select specific CSV files.
#' @param ... Additional arguments to pass on
#'
#' @return Formatted data.frame. NOTE: Time variables are rounded to nearest hour by default.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ## import only using file name based current working directory
#' import_TelCSVfiles(path = "700516A Complete.csv")
#'
#' ## import using character vector specifying directory paths and file names
#' files <- list.files("../directory_path", pattern = "Complete", full = TRUE)
#' import_TelCSVfiles(path = files)
#'
#' ## import using character string specifying directory path to folder storing files
#' import_TelCSVs(path = "../directory_path", csv_pattern = "Complete")}

import_TelCSVfiles <- function(path, csv_pattern = NULL, ...){
    ## message
    if(all(grepl(path, ".csv"))){
        nfiles = length(path)
        csv_list = vector("list", nfiles)
        for(i in 1:nfiles){
            csv_list[[i]] = import_telirid(path[i])
            cat("------------------------------------------------------------------------------------------\n\n",
                "Processing file ", i, "of ", nfiles, "\n\n",
                "File path: ", path[i], "\n\n")
        }
        df = do.call("rbind", csv_list)
    }
    if(length(path) == 1 & !grepl(path, ".csv")){
        files = list.files(path, pattern = csv_pattern, full = TRUE, ...)
        nfiles = length(files)
        csv_list = vector("list", nfiles)
        for(i in 1:nfiles){
            csv_list[[i]] = import_telirid(files[i])
            cat("------------------------------------------------------------------------------------------\n\n",
                "Processing file ", i, "of ", nfiles, "\n\n",
                "File path: ", files[i], "\n\n")
        }
        cat("Compiling files ...\n\n")
        df = do.call("rbind", csv_list)
    }
    return(df)
}



#' Import Telonics TPF File and Extract CTNs, IMEIs, and Fix-Rate Schedules
#'
#' @param file Character string containing path and file name of Telonics TPF file
#'
#' @return Formatted data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' tpf2df("180906015A_1.tpf")}

import_tpf <- function(file){
    tpf = readLines(file)
    ## extract ctns
    ctn.marker = "sections.units.parameters.ctnList"
    tpf.line = tpf[grep(ctn.marker,tpf)]
    ctns = unlist(strsplit(tpf.line, split="[{}]"))[2]
    ctns = unlist(strsplit(ctns," "))
    ## extract imeis
    imei.marker = "sections.units.parameters.iridiumImeiList"
    tpf.line = tpf[grep(imei.marker,tpf)]
    imeis = unlist(strsplit(tpf.line, split="[{}]"))[2]
    imeis = unlist(strsplit(imeis," "))
    ## set search parameters for fix rate schedules
    sched.markers = c("sections.gps.parameters.qfpScheduleUpdatePeriod",
                      "sections.auxiliary1ScheduleSet.parameters.qfpScheduleUpdatePeriod",
                      "sections.auxiliary2ScheduleSet.parameters.qfpScheduleUpdatePeriod",
                      "sections.auxiliary3ScheduleSet.parameters.qfpScheduleUpdatePeriod")
    ## custom function to calculate fix rate for a given sched.marker
    calc.tpf.hours = function(tpf, marker){
        days = 0
        hours = 0
        tpf.line = tpf[grep(marker,tpf)]
        if(any(grep("Hour",tpf.line))){
            indx = regexpr("Hour", tpf.line)
            string = substring(tpf.line, indx - 3, indx - 2)
            hours = as.numeric(regmatches(string, regexpr("[[:digit:]]+", string)))
        }
        if(any(grep("Day",tpf.line))){
            indx = regexpr("Day", tpf.line)
            string = substring(tpf.line, indx - 3, indx - 2)
            days = as.numeric(regmatches(string, regexpr("[[:digit:]]+", string)))
        }
        (days * 24) + hours
    }
    ## extract fix rate schedules
    fixschedule = sapply(sched.markers, calc.tpf.hours, tpf = tpf)
    names(fixschedule) = c("primary", paste0("aux_",1:3))
    ## compile output data.frame
    df = data.frame(ctn = ctns, imei = imeis, as.data.frame(t(fixschedule)), tpf_file = gsub(".*/","", file))
    return(df)
}
