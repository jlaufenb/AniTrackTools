
#' Convert Telonics Iridium CSV File
#'
#' @param file Character string containing path and file name of Telonics Iridium CSV file
#' @param nskip Number of rows to remove from top of Iridium CSV file. This number may vary.
#' @param fixratescheds Optional data.frame with fix rate intervals assigned to each GPS schedule
#' @param QFPkeep Location quality categories to retain
#' @param addcol Names of columns to retain additional to default
#'
#' @return Formatted data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' csv2df(file = "700516A Complete.csv", nskip = 23)}

csv2df <- function(file,
                   nskip = 23,
                   fixratescheds = NULL, # named vector
                   QFPkeep = c("Resolved QFP", "Resolved QFP (Uncertain)"),
                   addcol = NULL){
    ## load file
    x = readLines(file)
    ctn = substr(x[8],5,nchar(x[8]))
    x = utils::read.csv(textConnection(paste0(x[-(1:nskip)],collapse="\n")),stringsAsFactors=FALSE)
    attr(x,"CTN") = ctn # assign CTN# as attribute
    ## Create some bookkeeping objects
    telcolnames = names(x) # Column names in Telonics Complete CSV data files
    if(!all(addcol %in% telcolnames)){
        stop("Check additional column names. Some do not occur in column names of Telonics data file.")
    }
    newcolnames = c("acqtime", "acqstime", "ceprad", "irlat", "irlon", "fixtime", "fixtype", "lat", "lon",
                    "utmzone", "utmy", "utmx", "alt", "herr", "hdop", "satbitmap", "nsats", "navtime", "activct",
                    "temp", "uplink", "rectime", "repct", "lowvolt", "mort", "ircommand", "fixsched", "predeploy",
                    "error")
    colkeep = c("GPS.Fix.Time", "GPS.Fix.Attempt", "GPS.Latitude", "GPS.Longitude",
                "Schedule.Set", "GPS.Horizontal.Dilution", "GPS.Satellite.Count", "Mortality", addcol)
    scheds <- c("Primary","Auxiliary 1","Auxiliary 2","Auxiliary 3")
    names(scheds) <- c("Primary","Aux1","Aux2","Aux3")
    newcolnames = newcolnames[telcolnames %in% colkeep]
    qfp_levels = c("Resolved QFP", "Resolved QFP (Uncertain)", "Unresolved QFP") # types of GPS fixes
    ## retain location data and convert to dataframe
    x = cbind(rep(NA,nrow(x)), rep(ctn, nrow(x)), x[,telcolnames %in% colkeep])
    dimnames(x)[[2]] = c("animal_id", "collar_id", newcolnames)
    x$fixtype[x$fixtype==""] = NA # replace empty values w/ NA
    ## Subset out location data based on desired fix rate(s)
    x = x[x$fixtype %in% qfp_levels,] # retain QFP data
    x$fixtype = factor(x$fixtype, levels = qfp_levels) # convert GPS fix type to factor
    x$fixsched = factor(x$fixsched, levels = scheds) # convert fix-rate schedule names to factor
    ## Format time variables
    x$fixtime = as.POSIXct(round(as.POSIXct(x$fixtime, tz = "UTC", format = "%Y.%m.%d %H:%M:%S"),"hours"))
    ## Remove duplicate fixes
    x <- x[!duplicated(x$fixtime),]
    ## Create fixrate variable
    if(is.data.frame(fixratescheds)){
        if(is.data.frame(fixratescheds))fixratescheds = fixratescheds[fixratescheds$ctn == ctn,]
        x$fixrate = unlist(fixratescheds[1, match(x$fixsched, scheds)+2])
    }
    ## check for valid data & subset data by fixtype
    if(nrow(x)==0){
        warning("No valid location data. NULL value returned. ")
        x = list(NULL)
    } else {
        x = x[x$fixtype %in% QFPkeep, ]
    } #ifelse
    return(x)
}

#' Extract CTNs, IMEIS, and fix-rate schedules from Telonics TPF File
#'
#' @param file Character string containing path and file name of Telonics TPF file
#'
#' @return Formatted data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' tpf2df("180906015A_1.tpf")}

tpf2df <- function(file){
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
    calc.tpf.hours <- function(tpf, marker){
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
