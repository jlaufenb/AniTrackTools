% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processing_tools.R
\name{make.moveData}
\alias{make.moveData}
\title{Convert GPS Data to moveData Class from moveHMM Package}
\usage{
make.moveData(att, by = "animal_id")
}
\arguments{
\item{att}{Data.frame containing GPS data following output format of \code{import_telcsv} function}

\item{by}{Character string containing name of column to be used to partition moveHMM data. Default is set to \code{animal_id}}
}
\value{
R object of class moveData
}
\description{
Convert GPS Data to moveData Class from moveHMM Package
}
\examples{
\dontrun{
gps_df <- import_telcsv(file = "700516A Complete.csv", nskip = 23)
att_df <- telcsv2ATT(telcsv = gps_df, animal_ids = data.frame(collar_id = "700516A", animal_id = "lynx_1"))
make.moveData(att)}
}
