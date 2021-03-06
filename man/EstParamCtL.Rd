% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WoLF_functionsV2.R
\name{EstParamCtL}
\alias{EstParamCtL}
\title{Estimation of chamberlet length parameters CtLFex (Initial Final Chamberlet Length), CtLFinc (increase of final chamberlet length)
and CtLD (Chamberlet Length Decrease)}
\usage{
EstParamCtL(startMM1, startMM2, print.mod, path_plot, dev_plot, dev_plot_Ct)
}
\arguments{
\item{startMM1}{a vector with lower and upper limit for the starting values for the first Michaelis Menten parameter; default is between 100 and 200}

\item{startMM2}{a vector with lower and upper limit for the starting values for the second Michaelis Menten parameter; default  is between-1 and 1}

\item{print.mod}{input if the model should be printed; TRUE or FALSE}

\item{path_plot}{path where graphs should be saved; string}

\item{dev_plot}{which extension should be used to save graphs of the exponential function; string - if = NULL then graphs are not saved}

\item{dev_plot_Ct}{which extension should be used to save graphs of the Michaelis Menten function; string - if = NULL then graphs are not saved}
}
\value{
the function returns the estimated parameters CtLFex, CtLFinc  and CtLD for each specimens as new columns in the summary table
}
\description{
Fitting the chamberlet number vs. chamberlet length series of each chamber of a specimen with a Michaelis Menten function. The function
parameters of the Michealis Menten function correspond to the decrease of chamberlet length and the length of the final chamberlet within one chamber.
The mean of the decrease of chamberlet length for one specimen is calculated to get CtLD. The length of the final chamberlet vs. chamber series
is fitted by a linear function and the function parameters are extracted as the biometric parameters CtLFex and CtLFinc.
}
\examples{


EstParamCtL(startMM1=c(100,200),startMM2=c(-1,1),print.mod=F,path_plot ="C:/Users/user/Desktop/ggplots",dev_plot ="jpg",dev_plot_Ct = "jpg")
EstParamCtL(startMM1=c(100,200),startMM2=c(-1,1),print.mod=F,path_plot ="C:/Users/user/Desktop/ggplots",dev_plot ="jpg",dev_plot_Ct = "jpg")
EstParamCtL(print.mod=FALSE, path_plot ="C:/Users/user/Desktop/ggplots", dev_plot ="jpg", dev_plot_Ct = "jpg")
EstParamCtL(print.mod=TRUE, path_plot ="C:/Users/user/Desktop/ggplots", dev_plot =NULL, dev_plot_Ct = NULL)

}
