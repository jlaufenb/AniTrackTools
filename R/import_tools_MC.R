

#' Import Telonics Spreadspectrum collar data (combined version that imports and merges CSVs)
#'
#' @description Imports, merges, and (optional) casts Telonics Spreadspectrum GPS collar data
#'
#' @author McCrea Cobb \email{mccrea_cobb@@fws.gov}
#'
#' @param dir_path a relative directory path to a folder containing raw Telonics GPS collar data
#' @param csv_pattern an optional regular expression. Only file names which match the regular expression will be returned.
#' @nskip a numeric value indicating the number of rows to remove from top of the CSV file
#' @param recursive a logical value. Should the listing recurse into directories? Inherited from \code{list.files()}.
#' @param recast a logical value. Should the variable be recast?
#' @param fix_attempt_keep a character vector containing location quality categories to retain. Default is:
#'   \itemize{
#'     \item{"Succeeded (3D)"}
#'     \item{"Succeeded (2D)"}
#'     \item{"Resolved QFP"}
#'     \item{"Resolved QFP (Uncertain)"}
#'     }
#' @param ... other inherited argumements
#'
#' @return A dataframe containing Telonics Spreadspectrum collar data
#'
#' @export
#'
#' @example import_telsst_complete("./data/raw_data/telonics")

import_telsst_complete <- function(dir_path = NULL,
                          csv_pattern = "*",
                          nskip = 21,
                          recursive = TRUE,
                          recast = TRUE,
                          fix_attempt_keep = c("Succeeded (3D)",
                                               "Succeeded (2D)",
                                               "Resolved QFP",
                                               "Resolved QFP (Uncertain)"),
                          snake_case = FALSE,
                          ...){

  library(tidyverse)
  library(dplyr)

  message("Importing Telonics Spreadspectrum collar data...")

  # Import the data
  files <- list.files(path = dir_path,
                      pattern = paste0(csv_pattern, ".csv"),
                      full.names = TRUE,
                      recursive = TRUE)

  if(is.null(df) == TRUE) {
    stop("Did not find any CSVs. Did you use the correct file path?")
  }

  df <- files %>%
    map_df(function(x) read_csv(x, col_types = cols(.default = "c"), skip = nskip) %>%
             mutate(collar_id = gsub(".csv", "", basename(x))))

  cat("Successfully imported", length(files), "CSV files.\n")

  if(recast == TRUE){
    # Cast variables
    df <- df %>%
      dplyr::select(collar_id, everything()) %>%  # Reorder collar_id first
      mutate(`Acquisition Time` = as.POSIXct(`Acquisition Time`,
                                             tz = "UTC",
                                             format = "%Y.%m.%d %H:%M:%S"),
             `Acquisition Start Time` = as.POSIXct(`Acquisition Start Time`,
                                                   tz = "UTC",
                                                   format = "%Y.%m.%d %H:%M:%S"),
             `GPS Fix Time` = as.POSIXct(`GPS Fix Time`,
                                         tz = "UTC",
                                         format = "%Y.%m.%d %H:%M:%S"),
             `GPS Latitude` = as.numeric(`GPS Latitude`),
             `GPS Longitude` = as.numeric(`GPS Longitude`),
             `GPS UTM Northing` = as.numeric(`GPS UTM Northing`),
             `GPS UTM Easting` = as.numeric(`GPS UTM Easting`),
             `GPS Altitude` = as.numeric(`GPS Altitude`),
             `GPS Speed` = as.numeric(`GPS Speed`),
             `GPS Heading` = as.numeric(`GPS Heading`),
             `GPS Horizontal Error` = as.numeric(`GPS Horizontal Error`),
             `GPS Positional Dilution` = as.numeric(`GPS Positional Dilution`),
      )
    cat("Data recast.\n")
  }

  # Filter to `fix_attempt_keep` and keep only unique rows
  df <- df %>%
    filter(`GPS Fix Attempt` %in% fix_attempt_keep) %>%
    unique()
  cat("Data filtered to", fix_attempt_keep, "values.\n")

  # Rename the columns to snake case
  if (snake_case == TRUE){
    df <- df %>%
      rename_with(~ tolower(gsub(" ", "_", .x, fixed = TRUE)))
    cat("Variables renamed to snake case.\n")
  }

  message("Done.")

  return(df)
}










#' Internal Function to Import Individual Telonics Spreadspectrum CSV File (like `importtelfn`, but for Spreadspectrum collars)
#'
#' @param file Character string containing the path and file name of a Telonics Spreadspectrum CSV file
#' @param nskip Number of rows to remove from top of \code{file}
#' @param fix_attempt_keep Character vector containing location quality categories to retain
#' @param ... other inherited arguments
#'
#' @return Formatted data.frame
#'
#' @example import_telsst(file = "./data/raw_data/telonics/2013-10-22/666788A.csv")

import_telsst <- function(file,
                          nskip = 21,
                          fix_attempt_keep = c("Succeeded (3D)",
                                               "Succeeded (2D)",
                                               "Resolved QFP",
                                               "Resolved QFP (Uncertain)"),
                          ...){
  # load file
  x <- readLines(file)
  ctn <-x[which(grepl("CTN", x))]
  ctn <- gsub("CTN", "", ctn)
  ctn <- gsub(",", "", ctn)

  x <- utils::read.csv(textConnection(paste0(x[-(1:nskip)],collapse="\n")), stringsAsFactors = FALSE)

  # reformat column names
  names(x) <- gsub("\\.", "\\_", names(x))

  # add collar_id variable
  x$collar_id <-  ctn

  # reorder columns
  x <-  x[ ,c(ncol(x), 1:(ncol(x) - 1))]

  # convert time variables to POSIXct
  x$Acquisition_Time <- as.POSIXct(x$Acquisition_Time,
                                   tz = "UTC",
                                   format = "%Y.%m.%d %H:%M:%S")
  x$Acquisition_Start_Time <- as.POSIXct(x$Acquisition_Start_Time,
                                         tz = "UTC",
                                         format = "%Y.%m.%d %H:%M:%S")
  x$GPS_Fix_Time <- as.POSIXct(x$GPS_Fix_Time,
                               tz = "UTC",
                               format = "%Y.%m.%d %H:%M:%S")

  # make GPS Satellite Count consistently a character variable
  x$GPS_Satellite_Count <- as.character(x$GPS_Satellite_Count)

  # subset data by GPS_Fix_Attempt and convert GPS_Fix_Attempt to factor
  x <- x[x$GPS_Fix_Attempt %in% fix_attempt_keep, ]
  x$GPS_Fix_Attempt <- factor(x$GPS_Fix_Attempt,
                              levels = fix_attempt_keep)

  # remove duplicate fix times
  x <- unique(x)

  # error message for collars without fix data
  if(nrow(x) == 0){
    warning(paste0("No valid location data for CTN ",
                   ctn,
                   " (", file, ")."))
    x <- list(NULL)
  }

  return(x)
}






#' Import Telonics GPS collar data (generalized version of `import_TelCSVfiles`)
#'
#' @param dir_path a relative directory path to a folder containing raw Telonics GPS collar data
#' @param nskip a numeric value indicating the number of rows to remove from top of the CSV file
#' @param collar_type the make and model of GPS collar data to be imported. Accepted values include:
#' #'   \itemize{
#'     \item{"Telonics Spreadspectrum"}
#'     \item{"Telonics Iridium"}
#'     \item{"ATS Globalstar"}
#'     }
#' @param ... other inherited arguments
#'
#' @return a dataframe containing GPS collar data
#' @export
#'
#' @examples
import_gps_collars <- function(dir_path = "./data/raw_data/telonics",
                               nskip = 21,
                               collar_type = "Telonics Spreadspectrum",
                               ...){

  # Select which collar import function to use
  if (collar_type == "Telonics Spreadspectrum"){
    import_func <- import_telsst
  }
  if (collar_type == "Telonics Iridium"){
    import_func <- import_teliri
  }
  if (collar_type == "ATS Globalstar"){
    import_func <- import_atsglo
  } else {
    stop("Select your type of collar. Options are: 'Telonics Iridium', 'Telonics Spreadspectrum', or 'ATS Globalstar'.")
  }

  message(paste("Importing", collar_type, "collar data..."))

  df <-
    list.files(path = dir_path,
               pattern = "*.csv",
               full.names = TRUE,
               recursive = TRUE) %>%
    map_df(function(x) import_func(x)) %>%
    unique()

  message("Done")

}

