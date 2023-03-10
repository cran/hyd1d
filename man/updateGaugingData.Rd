% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/updateGaugingData.R
\name{updateGaugingData}
\alias{updateGaugingData}
\title{Update local copy of \code{df.gauging data}}
\usage{
updateGaugingData(x)
}
\arguments{
\item{x}{path to the file containing \code{\link{df.gauging_data}} 
(type \code{character}).}
}
\value{
\code{invisible(logical)} notifying whether an updated version of 
  \code{\link{df.gauging_data}} has been downloaded.
}
\description{
Function to overwrite and update the internal dataset
  \code{\link{df.gauging_data}}. This function is usually called during the
  initial loading of the package. If an update of 
  \code{\link{df.gauging_data}} took place more than 8 days ago, an updated
  version of \code{\link{df.gauging_data}} will be downloaded and used.
}
\examples{
  options("hyd1d.datadir" = tempdir())
  updateGaugingData(paste0(options()$hyd1d.datadir,
                           "/df.gauging_data_latest.RDS"))

}
