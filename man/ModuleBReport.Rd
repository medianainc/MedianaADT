\name{ModuleBReport}
\alias{ModuleBReport}
\alias{ModuleBReportDoc}
\title{
Simulation report for adaptive trials with data-driven treatment selection 
}
\description{
This function creates a detailed simulation report for adaptive trials with data-driven treatment selection in a Microsoft Word format. For examples of the function call, see \link{ExampleB1}, \link{ExampleB2} or \link{ExampleB3}.
}
\usage{
ModuleBReport(results, report_filename)
}
\arguments{
  \item{results}{Object of class \samp{ModuleBResults} created by the \code{\link{ModuleB}} function.  
  }
  \item{report_filename}{Character value defining the report's filename. The report is saved in the current working directory.
  }
}
\seealso{
  \code{\link{ModuleB}}, \code{\link{ModuleBApp}}
}
