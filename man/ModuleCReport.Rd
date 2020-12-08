\name{ModuleCReport}
\alias{ModuleCReport}
\alias{ModuleCReportDoc}
\title{
Simulation report for adaptive trials with data-driven population selection 
}
\description{
This function creates a detailed simulation report for adaptive trials with data-driven population selection in a Microsoft Word format. For examples of the function call, see \link{ExampleC1}, \link{ExampleC2} or \link{ExampleC3}.
}
\usage{
ModuleCReport(results, report_filename)
}
\arguments{
  \item{results}{Object of class \samp{ModuleCResults} created by the \code{\link{ModuleC}} function.  
  }
  \item{report_filename}{Character value defining the report's filename. The report is saved in the current working directory.
  }
}
\seealso{
  \code{\link{ModuleC}}, \code{\link{ModuleCApp}}
}
