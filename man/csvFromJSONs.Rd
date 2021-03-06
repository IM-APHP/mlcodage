% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ml.R
\name{csvFromJSONs}
\alias{csvFromJSONs}
\title{Generate dataframes from a set of JSON Files}
\usage{
csvFromJSONs(jsFolder, diagsPath, corpusName, serviceName)
}
\arguments{
\item{jsFolder}{path to your local folder containing JSON files.}

\item{diagsPath}{path to your local RData file containing diagnostics (codes). The file must contain an object called "Diagnostics" with at least two columns ('NDA', and 'ACTE').}

\item{corpusName}{choose a name for your corpus.}

\item{serviceName}{choose a service name from the built-in services: \{'urologie', 'chirgen', 'chirplas', 'chirmaxfac'\}.
This argument is used to choose the right UH (Hospital Unit) as well as the right variables from the JSON.
If you don't want any restriction regarding UH and VAR, please set serviceName to 'gen'.}
}
\value{
a list of two dataframes: 'texts' and 'codes' to be given separately to function 'buildFromDF'.
}
\description{
Generate 'df.text' and 'df.code' objects starting from a floder with a plenty of JSON files.
These objects can then be given to function 'buildFromDF' in order to build a predictive model.
}

