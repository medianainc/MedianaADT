\name{ModuleEApp}
\alias{ModuleEApp}
\title{
Graphical user interface for event prediction in trials with an event-driven design
}
\description{
This function creates a web application with a Shiny-based graphical user interface to facilitate the process of event forecasting in a Phase II or Phase III clinical trial with an event-driven design. The application supports the same functionality as the \code{\link{ModuleE}} function. For an example of the function call, see \link{ExampleE}. \cr
This application prompts the user to upload a data set in a CSV (comma-separated values) format with blinded patient enrollment, event and patient dropout data. The data set is required to include four variables (\code{enrollment}, \code{time}, \code{event} and \code{dropout}) with a single record per patient, see for example the \code{\link{ModuleEData}} data set. 
}
\usage{
ModuleEApp()
}
\seealso{
  \code{\link{ModuleE}}, \code{\link{ModuleEReport}}
}

