\name{ccamCode}
\alias{ccamCode}
\title{CCAM Code Prediction}
\usage{
  ccamCode(text = "", service="", nbCodes=10)
}
\arguments{
  \item{text}{discharge summary. Required.}
  \item{service}{the service from urology, surgery, ... etc, used to pick the right model. Required.}
  \item{nbCodes}{number of returned codes (default 10). Required.}
}
\description{
  CCAM Code Prediction function to be called from OpenCPU API.
}

