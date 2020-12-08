\name{ModuleEReport}
\alias{ModuleEReport}
\alias{ModuleEReportDoc}
\title{
Simulation report for event prediction in trials with an event-driven design 
}
\description{
This function creates a detailed simulation report for event prediction in trials with an event-driven design in a Microsoft Word format. For an example of the function call, see \link{ExampleE}.
}
\usage{
ModuleEReport(results, report_filename)
}
\arguments{
  \item{results}{Object of class \samp{ModuleEResults} created by the \code{\link{ModuleE}} function.  
  }
  \item{report_filename}{Character value defining the report's filename. The report is saved in the current working directory.
  }
}
\seealso{
  \code{\link{ModuleE}}, \code{\link{ModuleEApp}}
}
