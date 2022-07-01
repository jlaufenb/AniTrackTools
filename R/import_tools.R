
#' Import Telonics GPS collar data (generalized version of `import_TelCSVfiles`)
#'
#' @param path Character vector containing path(s) and file name(s) of Telonics Iridium CSV file(s),
#' character string specifying path to folder containing Telonics Iridium CSV file(s), or
#' character string specifying path to root folder containing folder(s) with Telonics Iridium CSV file(s) as subfolder(s).\cr\cr
#' NOTE: When specifying a root folder within which a search of subfolders is conducted, user must pass \code{recursive = TRUE} as an additional argument. Use of the \code{pattern} argument is recommended to avoid importing non-data files.
#' @param collar_type Character string specifying the type of GPS collar for which data is to be imported. Accepted values include:
#'   \itemize{
#'     \item{"Telonics Spreadspectrum"}
#'     \item{"Telonics Iridium"}
#'     }
#' @param nskip Numeric value indicating the number of rows to remove from top of the CSV file
#' @param pattern Character string used to identify Telonics Iridium CSV file(s) when a folder path is provided for the \code{path} argument. See help file for the \code{list.files} base R function.
#' @param ... Additional arguments to pass on.\cr\cr
#' NOTE: Common additional arguments to pass include:
#'   \itemize{
#'     \item{"\code{recursive = TRUE}" when specifying a root folder to search for data files}
#'     \item{"\code{recast = TRUE}" to recast selected variables}
#'     \item{"\code{colname_fun = 'function name'}" to specify alternative function used to reformat column names (e.g., \code{toupper})}
#'     \item{"\code{fix_attempt_keep = 'All'}" to return all QFP fixes. Default only returns "Resolved QFP" and "Resolved QFP (Uncertain)".}
#'     }
#' @return Dataframe containing GPS collar data.
#' @export

import_tel_gps <- function(path,
                           collar_type = NULL,
#                           nskip = NULL, currently not in use for Iridium collars
                           pattern = NULL,
                           ...){
    run.time = system.time({
        collar_types = c("Telonics Iridium", "Telonics Spreadspectrum")
        if(!collar_type %in% collar_types)
            stop("Select your type of collar. Options are: 'Telonics Iridium' or 'Telonics Spreadspectrum'.")

#        if(is.null(nskip))
#            stop("Must specify number of header lines in CSV file to skip.")

        message(paste("\n\nImporting", collar_type, "collar data...\n\n"))

        # Select which collar import function to use
        if (collar_type == "Telonics Spreadspectrum"){
            stop("Telonics Spreadspectrum currently not supported")
#            df <-
#                list.files(path = dir_path,
#                           pattern = "*.csv",
#                           full.names = TRUE,
#                           recursive = TRUE) %>%
#                map_df(function(x) import_telsst(x)) %>%
#                unique()
        }

        if (collar_type == "Telonics Iridium"){


            ## file name(s) provided
            if(all(grepl("\\.csv", path))){
                nfiles = length(path)
                csv_list = vector("list", nfiles)
                for(i in 1:nfiles){
                    csv_list[[i]] = import_telirid(path[i], ...)
                    cat("------------------------------------------------------------------------------------------\n\n",
                        "Processing file ", i, "of ", nfiles, "\n\n",
                        "File path: ", path[i], "\n\n")
                }
                df = do.call("rbind", csv_list)
            }


            ## path to folder containing file(s) provided
            if(length(path) == 1 & !any(grepl("\\.csv", path))){

                ## warning
                if(is.null(pattern))
                    warning(paste0("Specifying a value for the 'pattern' argument is recommended when searching multiple subfolders within\n",
                                   "a root folder for data files to avoid attempted import of non-data files."))
                files = list.files(path, full = TRUE, pattern = pattern, ...)
                nfiles = length(files)
                csv_list = vector("list", nfiles)
                for(i in 1:nfiles){
                    csv_list[[i]] = import_telirid(files[i], ...)
                    cat("------------------------------------------------------------------------------------------\n\n",
                        "Processing file ", i, "of ", nfiles, "\n\n",
                        "File path: ", files[i], "\n\n")
                }
                cat("Compiling files ...\n\n")
                df = do.call("rbind", csv_list)
            }
        }
    })
    message(paste0("Import complete. Execution time: ", round(run.time["elapsed"],2), " seconds"))
    return(df)
}




#' Internal Function to Import Individual Telonics Iridium CSV File
#'
#' @param file Character string containing path and file name of Telonics Iridium CSV file.
#' @param nskip Number of rows to remove from top of Iridium CSV file. This number may vary.
#' @param fix_attempt_keep Character vector containing location quality categories to retain. A value of "All" will return all QFP fixes.
#' @param recast Logical value specifying whether to recast the following selected variables:
#'   \itemize{
#'     \item{"Schedule_Set"}
#'     \item{"Acquisition_Time"}
#'     \item{"Acquisition_Start_Time"}
#'     \item{"GPS_Fix_Time"}
#'     \item{"Receive_Time"}
#'     \item{"GPS_Fix_Attempt"}
#'     }
#' @param colname_fun Function used to format column names of output data.frame. Default is the \code{tolower} function to format as snake case.
#'
#' @return Formatted data.frame.
#' @export

import_telirid <- function(file,
                   fix_attempt_keep = c("Resolved QFP", "Resolved QFP (Uncertain)"),
                   recast = TRUE,
                   colname_fun = "tolower",
                   ...
                   ){

    ## load file
    x = readLines(file)
    header_md <- x[grep("Headers Row,",x)]
    indx <- regexpr(",",header_md)
    nskip = as.numeric(substr(header_md,indx+1,nchar(header_md)))
    ctn = x[which(grepl("CTN",x))]
    ctn = substr(ctn, regexpr(",", ctn) + 1, nchar(ctn))
    x = utils::read.csv(textConnection(paste0(x[-(1:nskip)],collapse="\n")), stringsAsFactors = FALSE)
    names(x) <- gsub("\\.", "\\_", names(x))

    ## subset data by GPS_Fix_Attempt
    if(any(fix_attempt_keep == "All")) fix_attempt_keep = c("Resolved QFP", "Resolved QFP (Uncertain)", "Unresolved QFP")
    x = x[x$GPS_Fix_Attempt %in% fix_attempt_keep, ]


    ## message for null data sets
    if(nrow(x)==0){
        warning(paste0("No valid location data for CTN ", ctn, "."))
        x = list(NULL)
    }else{

        ## add collar ID variable
        x$Collar_ID = ctn


        ## reorder columns
        x = x[,c(ncol(x),1:(ncol(x)-1))]


        ## Recast selected variables
        if(recast){
            scheds = c("Primary","Auxiliary 1","Auxiliary 2","Auxiliary 3")
            x$Schedule_Set = factor(x$Schedule_Set, levels = c("Primary","Auxiliary 1","Auxiliary 2","Auxiliary 3"))
            x$Acquisition_Time = as.POSIXct(x$Acquisition_Time, tz = "UTC", format = "%Y.%m.%d %H:%M:%S")
            x$Acquisition_Start_Time = as.POSIXct(x$Acquisition_Start_Time, tz = "UTC", format = "%Y.%m.%d %H:%M:%S")
            x$GPS_Fix_Time = as.POSIXct(x$GPS_Fix_Time, tz = "UTC", format = "%Y.%m.%d %H:%M:%S")
            x$Receive_Time = as.POSIXct(x$Receive_Time, tz = "UTC", format = "%Y.%m.%d %H:%M:%S")
            x$GPS_Fix_Attempt = factor(x$GPS_Fix_Attempt, levels = fix_attempt_keep)
        }

        ## Remove duplicate rows
        x = unique(x)


        ## reformat column names
        if(is.null(colname_fun)){
            names(x) = gsub("\\_", "\\ ", names(x))
        }else{
            names(x) = do.call(colname_fun, list(names(x)))
        }
    }
    rownames(x) = NULL
    return(x)
}



#' Internal Function Used to Reformat Column Names
#'
#' @param x Character vector
#'
#' @return Character vector in lower snake case format
#'
snake_case <- function(x){
    tolower(gsub("\\.", "\\_", x))
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

    ## read in tpf file
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




#' Import Telonics Iridium CSV File(s)
#'
#' @param path Character vector containing path(s) and file name(s) of Telonics Iridium CSV file(s),
#' character string specifying path to folder containing Telonics Iridium CSV file(s), or
#' character string specifying path to root folder containing folders with Telonics Iridium CSV file(s).\cr\cr
#' NOTE: When specifying a root folder within which a search of subfolders is conducted, user must pass \code{recursive = TRUE} as an additional argument.
#' @param ... Additional arguments to pass on.
#'
#' @return Dataframe containing GPS collar data.
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

import_TelCSVfiles <- function(path, ...){

    ## file name(s) provided
    if(all(grepl(path, ".csv"))){
        nfiles = length(path)
        csv_list = vector("list", nfiles)
        for(i in 1:nfiles){
            csv_list[[i]] = import_telirid(path[i], ...)
            cat("------------------------------------------------------------------------------------------\n\n",
                "Processing file ", i, "of ", nfiles, "\n\n",
                "File path: ", path[i], "\n\n")
        }
        df = do.call("rbind", csv_list)
    }


    ## path to folder containing file(s) provided
    if(length(path) == 1 & !grepl(path, ".csv")){
        files = list.files(path, full = TRUE, ...)
        nfiles = length(files)
        csv_list = vector("list", nfiles)
        for(i in 1:nfiles){
            csv_list[[i]] = import_telirid(files[i], ...)
            cat("------------------------------------------------------------------------------------------\n\n",
                "Processing file ", i, "of ", nfiles, "\n\n",
                "File path: ", files[i], "\n\n")
        }
        cat("Compiling files ...\n\n")
        df = do.call("rbind", csv_list)
    }

    return(df)
}
