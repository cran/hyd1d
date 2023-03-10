% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{df.gauging_data}
\alias{df.gauging_data}
\title{Gauging data for all WSV-run gauging stations along Elbe and Rhine}
\format{
A \code{data.frame} with 1379334 (rows and 3 variables):
\describe{
\item{gauging_station}{name of the gauging station (type \code{character}).  It is used as JOIN field for dataset \code{\link{df.gauging_station_data}}.}
\item{date}{of the measurement (type \code{Date}).}
\item{w}{water level relative to the gauge zero (cm, type \code{numeric}).}
}
}
\usage{
df.gauging_data
}
\description{
This dataset contains all \strong{daily-averaged} gauging data
  for the gauging stations along \strong{Elbe} and \strong{Rhine} operated by
  the waterway and shipping administration (Wasserstraßen- und
  Schifffahrtsverwaltung (WSV)) since 1960-01-01. Data from
  1960-01-01 until 2022-12-31 are validated and were queried from the
  BfG-Wiski
  (\href{http://www.bafg.de/DE/08_Ref/M1/03_Pegelwesen/HYDABA/hydaba_node.html}{HyDaBa})
  and supplied by \email{Datenstelle-M1@bafg.de}. Data after 2021-12-31 are
  continuously collected from \url{https://pegelonline.wsv.de/gast/start} and
  are not officially validated. Unvalidated recent data will be replaced
  anually and distributed through package and/or internal dataset updates.
  
  The latest version is stored locally under
  \code{paste0(options()$hyd1d.datadir, "/df.gauging_data_latest.RDS")}. To
  modify the location of your locally stored gauging data set using
  \code{options()} prior to loading the package, e.g.
  \code{options("hyd1d.datadir" = "~/.hyd1d");library(hyd1d)}. The location
  can be determined through the environmental variable \env{hyd1d_datadir}.
}
\examples{
  options("hyd1d.datadir" = tempdir())
  updateGaugingData(paste0(options()$hyd1d.datadir,
                           "/df.gauging_data_latest.RDS"))

}
\references{
\insertRef{wsv_pegeldaten_2021}{hyd1d}
   
   \insertRef{wsv_pegelonline_2018}{hyd1d}
}
\seealso{
\code{\link{updateGaugingData}}
}
\keyword{datasets}
