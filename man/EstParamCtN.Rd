% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WoLF_functionsV2.R
\name{EstParamCtN}
\alias{EstParamCtN}
\title{Estimation of chamberlet number parameters ICtN (initial chamberlet number), CtNI (initial chamber base length) and NOC
(Number of operculinid chambers).}
\usage{
EstParamCtN(print.mod, path_plot, dev_plot)
}
\arguments{
\item{print.mod}{input if the model should be printed; TRUE or FALSE}

\item{path_plot}{path where graphs should be saved; string}

\item{dev_plot}{giving which extension should be used; string - if = NULL then graphs are not saved}
}
\value{
the function returns the estimated parameters ICtN and CtNI for each specimens as a new column in the summary table
}
\description{
Fitting the chamber number vs. chamberlet number series with a power function and extracting the biometric parameters. The NOC
of operculinid chambers is directly counted from the input data.
}
\examples{


EstParamCtN(print.mod=FALSE, path_plot ="C:/Users/user/Desktop/ggplots",dev_plot ="jpg" )
EstParamCtN(print.mod=TRUE, path_plot ="C:/Users/user/Desktop/ggplots",dev_plot ="jpg" )

}
