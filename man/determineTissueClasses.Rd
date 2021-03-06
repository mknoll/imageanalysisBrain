% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/determineTissueClasses.R
\name{determineTissueClasses}
\alias{determineTissueClasses}
\title{Determine tissue classes}
\usage{
determineTissueClasses(data)
}
\arguments{
\item{data}{list, containing data frames with columns key and val}
}
\value{
list with the identified cutoffs
}
\description{
Determines tissue classes for a set of 
identified cutoffs
}
\examples{
data <- data.frame(
key=c(823,623,923,723,1023,323,1113,123), 
val=c(0.0027, 0.0021, 0.0020, 0.0020, 0.0013, 0.0012, 0.0004, 0.0001))
determineTissueClasses(data)
}

