% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replaceData1Names.R
\name{replaceData1Names}
\alias{replaceData1Names}
\title{replaceData1Names}
\usage{
replaceData1Names(data_names, data1)
}
\arguments{
\item{data_names}{data.frame of variable metadata from data_Dictionary.csv file}

\item{data1}{input data (data1)}
}
\value{
\code{data1} input data (data1) with data1UserNames replaced with sparrowNames
}
\description{
replaces data1UserNames with sparrowNames in the data1 object from the
dataDictionary.csv control file \cr \cr
Executed By: dataInputPrep.R \cr
Executes Routines: getVarList.R \cr
}
