\name{MedianaADT-package}
\alias{MedianaADT-package}
\alias{MedianaADT}
\docType{package}
\title{
Adaptive Design Toolkit: Adaptive designs commonly used in Phase III and seamless Phase II/III trials
}
\description{
The MedianaADT package provides efficient simulation-based tools for adaptive designs commonly used in Phase III and seamless Phase II/III trials. The Adaptive Design Toolkit currently includes the following five modules:
\itemize{
\item Module A: Adaptive designs with data-driven sample size or event count re-estimation.
\item Module B: Adaptive designs with data-driven treatment selection.
\item Module C: Adaptive designs with data-driven population selection.
\item Module D: Optimal selection of a futility stopping rule.
\item Module E: Event prediction in event-driven trials.
}
For more information on this package, visit \url{http://www.medianainc.com/free-software/}. The technical manuals with a detailed description of the statistical methodology implemented in each module can be found in the package's \code{doc} folder.
}
\details{
\tabular{ll}{
Package: \tab MedianaADT\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2020-11-25\cr
License: \tab GPL-3
}
Key functions included in the package:
\itemize{
\item \code{\link{ModuleA}}: Simulation-based design of adaptive trials with data-driven sample size or event count re-estimation.
\item \code{\link{ModuleAReport}}: Simulation report for adaptive trials with data-driven sample size or event count re-estimation.
\item \code{\link{ModuleAApp}}: Graphical user interface to design adaptive trials with data-driven sample size or event count re-estimation.
\item \code{\link{ModuleB}}: Simulation-based design of adaptive trials with data-driven treatment selection.
\item \code{\link{ModuleBReport}}: Simulation report for adaptive trials with data-driven treatment selection.
\item \code{\link{ModuleBApp}}: Graphical user interface to design adaptive trials with data-driven treatment selection.
\item \code{\link{ModuleC}}: Simulation-based design of adaptive trials with data-driven population selection.
\item \code{\link{ModuleCReport}}: Simulation report for adaptive trials with data-driven population selection.
\item \code{\link{ModuleCApp}}: Graphical user interface to design adaptive trials with data-driven population selection.
\item \code{\link{ModuleD}}: Simulation-based selection of an optimal futility stopping rule at an interim analysis.
\item \code{\link{ModuleDReport}}: Simulation report for optimal futility stopping rules.
\item \code{\link{ModuleDApp}}: Graphical user interface for an optimal selection of a futility stopping rule.
\item \code{\link{ModuleE}}: Simulation-based event prediction in trials with an event-driven design.
\item \code{\link{ModuleEPriorDistribution}}: Calculation of the parameters of prior gamma distributions.
\item \code{\link{ModuleEReport}}: Simulation report for event prediction.
\item \code{\link{ModuleEApp}}: Graphical user interface for event prediction.
}
The package comes with one example data set:
\itemize{
\item \code{\link{ModuleEData}}: Example data set to illustrate event prediction in event-driven trials in Module E.
}
Multiple case studies are included in this vignette to illustrate the use of this package for designing clinical trials with data-driven design elements:
\itemize{
\item \link{ExampleA1}: Simulation-based design of an adaptive trial with sample size re-estimation (normally distributed endpoint).
\item \link{ExampleA2}: Simulation-based design of an adaptive trial with sample size re-estimation (binary endpoint).
\item \link{ExampleA3}: Simulation-based design of an adaptive trial with event count re-estimation (time-to-event endpoint).
\item \link{ExampleB1}: Simulation-based design of an adaptive trial with treatment selection (normally distributed endpoint).
\item \link{ExampleB2}: Simulation-based design of an adaptive trials with treatment selection (binary endpoint).
\item \link{ExampleB3}: Simulation-based design of an adaptive trials with treatment selection (time-to-event endpoint).
\item \link{ExampleC1}: Simulation-based design of an adaptive trial with population selection (normally distributed endpoint).
\item \link{ExampleC2}: Simulation-based design of an adaptive trials with population selection (binary endpoint).
\item \link{ExampleC3}: Simulation-based design of an adaptive trials with population selection (time-to-event endpoint).
\item \link{ExampleD1}: Simulation-based selection of an optimal futility stopping rule (normally distributed endpoint).
\item \link{ExampleD2}: Simulation-based selection of an optimal futility stopping rule (binary endpoint).
\item \link{ExampleD3}: Simulation-based selection of an optimal futility stopping rule (time-to-event endpoint).
\item \link{ExampleE}: Simulation-based event prediction in trials with an event-driven design (time-to-event endpoint).

}

}
\author{
Mediana (www.medianainc.com)
}
\references{
Bagiella, E., Heitjan, D.F. (2001). Predicting analysis times in randomized clinical trials. \emph{Statistics in Medicine}. 20, 2055-2063.

Chuang-Stein, C., Kirby, S., French, J., Kowalski, K., Marshall, S., Smith, M. K. (2011). A quantitative approach for making go/no-go decisions in drug development. \emph{Drug Information Journal}. 45, 187-202.

Dmitrienko, A., Paux, G. (2017). Subgroup analysis in clinical trials. \emph{Clinical Trial Optimization Using R}. Dmitrienko, A., Pulkstenis, E. (editors). Chapman and Hall/CRC Press, New York.

Dmitrienko, A., D'Agostino, R.B. (2018). Multiplicity considerations in clinical trials. \emph{New England Journal of Medicine}. 378, 2115-2122.

Herson, J. (1979). Predictive probability early termination plans for Phase II clinical trials. \emph{Biometrics}. 35, 775-783.

Millen, B., Dmitrienko, A., Ruberg, S., Shen, L. (2012). A statistical framework for decision making in confirmatory multi-population tailoring clinical trials. \emph{Drug Information Journal}. 46, 647-656.

Wang, D., Cui, L., Zhang, L., Yang, B. (2014). An ROC approach to evaluate interim go/no-go decision-making quality with application to futility stopping in the clinical trial designs. \emph{New Developments in Statistical Modeling, Inference and Application}. Jin, Z., Liu, M., Luo, X. (editors). Springer, New York. 121-147.

Wassmer, G., Brannath, W. (2016). \emph{Group Sequential and Confirmatory Adaptive Designs in Clinical Trials}. New York: Springer.

}
\keyword{ package }
