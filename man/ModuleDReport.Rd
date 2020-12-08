\name{ModuleDReport}
\alias{ModuleDReport}
\alias{ModuleDReportDoc}
\title{
Simulation report for optimal futility stopping rules 
}
\description{
This function creates a detailed simulation report for optimal futility stopping rules at an interim analysis in a Microsoft Word format. For examples of the function call, see \link{ExampleD1}, \link{ExampleD2} or \link{ExampleD3}.
}
\usage{
ModuleDReport(results, report_filename)
}
\arguments{
  \item{results}{Object of class \samp{ModuleDResults} created by the \code{\link{ModuleD}} function.  
  }
  \item{report_filename}{Character value defining the report's filename. The report is saved in the current working directory.
  }
}
\seealso{
  \code{\link{ModuleD}}, \code{\link{ModuleDApp}}
}
