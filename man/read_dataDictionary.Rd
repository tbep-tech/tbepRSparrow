% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_dataDictionary.R
\name{read_dataDictionary}
\alias{read_dataDictionary}
\title{read_dataDictionary}
\usage{
read_dataDictionary(file.output.list, batch_mode)
}
\arguments{
\item{batch_mode}{yes/no character string indicating whether RSPARROW is being run in batch
mode}
}
\value{
\code{data_names} data.frame of variable metadata from data_Dictionary.csv file
}
\description{
\cr \cr
Executed By: dataInputPrep.R \cr
Executes Routines: \itemize{\item getVarList.R
\item importCSVcontrol.R
\item unPackList.R} \cr
}
