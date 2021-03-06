% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ocpu.R
\name{launchListener}
\alias{launchListener}
\title{Launcher Web Service Listener}
\usage{
launchListener(port = 80)
}
\arguments{
\item{port}{listening port (default 80).}
}
\value{
FALSE if everything went ok.
}
\description{
Launch the Web service daemon at the specified port. If the daemon is already listening, it will be stopped and replaced by the new one.
Please check that the message "Web interface at: \%url\%" is displayed to ensure everything went ok.
The coding Web interface is available at the displayed \%url\% address that you can access via navigator.
}
\examples{
launchListener(port=8020) # must return FALSE
}

