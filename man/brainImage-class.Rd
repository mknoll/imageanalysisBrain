% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brainImageClass.R
\docType{class}
\name{brainImage-class}
\alias{brainImage}
\alias{brainImage-class}
\title{An S4 class representing an brain image}
\description{
An S4 class representing an brain image
}
\section{Slots}{

\describe{
\item{\code{.Data}}{see EBImage::Image@.Data}

\item{\code{colormode}}{see EBImage::Image@colormode}

\item{\code{values}}{Via MITK exported imaging data,
has the form: x,y,z,Value or Value,COORD}

\item{\code{filename}}{Identifier from which the 
data comes from}

\item{\code{coordBIT}}{size used to encode each coordinate,
10 would be enough for dimension values up to 10^2}

\item{\code{coords}}{Encoded koordinates, eases set operations
when manipulating volumes}

\item{\code{measurements}}{actually sored measurements for this
object (see: values: Value)}

\item{\code{origin}}{center of origin of this volume}

\item{\code{originVersion}}{method to calculate the origin
might be prone to changes; therefore, a version
is stored}

\item{\code{xfactor}}{factor which can be used to make 
distances comparable between patients; Normalizes
the biparietal distance to 1}

\item{\code{xfactorVersion}}{see originVersion}

\item{\code{selectedThresholds}}{thresholds, which might 
be useful when analyzing different tissue classes
in MRI data}

\item{\code{type}}{Name of the stored datatype (colname from
the measurements column in the values dataframe)}
}}

