% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sumIncremAttributes.R
\name{sumIncremAttributes}
\alias{sumIncremAttributes}
\title{sumIncremAttributes}
\usage{
sumIncremAttributes(idseq, attrib, attrib_name)
}
\arguments{
\item{idseq}{staidseq or vstaidseq, integer vector site IDs assigned contiguously to
upstream incremental reaches}

\item{attrib}{specified attributes with length equal to number of reaches}

\item{attrib_name}{character string naming \code{attrib}}
}
\value{
\code{siteiarea} dataframe with summed attribute
}
\description{
Sums the specified attribute over the incremental area of the calibration
sites, based on the unique station sequence number for each reach (staidseq).   [NOTE THAT THE
FUNCTION ARGUMENTS COULD USE EDITING TO USE THE UNIQUE NAMES RATHER THAN GENERIC NAMES) \cr \cr
Executed By: \itemize{\item calcIncremLandUse.R
\item correlationMatrix.R} \cr
}
