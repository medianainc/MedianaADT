\name{ModuleAReport}
\alias{ModuleAReport}
\alias{ModuleAReportDoc}
\title{
Simulation report for adaptive trials with data-driven sample size or event count re-estimation 
}
\description{
This function creates a detailed simulation report for adaptive trials with data-driven sample size or event count re-estimation in a Microsoft Word format. For examples of the function call, see \link{ExampleA1}, \link{ExampleA2} or \link{ExampleA3}.
}
\usage{
ModuleAReport(results, report_filename)
}
\arguments{
  \item{results}{Object of class \samp{ModuleAResults} created by the \code{\link{ModuleA}} function.  
  }
  \item{report_filename}{Character value defining the report's filename. The report is saved in the current working directory.
  }
}
\seealso{
  \code{\link{ModuleA}}, \code{\link{ModuleAApp}}
}
