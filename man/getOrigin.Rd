% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brainImageClass.R
\name{getOrigin}
\alias{getOrigin}
\title{Get Center of origin}
\usage{
getOrigin(img)
}
\arguments{
\item{img}{brainImage instance}
}
\value{
coordinates (x,y,z) of the claculated origin
}
\description{
Center of origin, calculated with 
getZeroKoord()
}
\examples{
data <- data.frame(x=1:10, y=1:10, 
z=c(rep(2,5), rep(3,5)), val=rnorm(100))
img <- new("brainImage", data, "test", 10)
getOrigin(img)
}

