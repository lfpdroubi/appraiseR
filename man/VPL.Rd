% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/involutivo.R
\name{VPL}
\alias{VPL}
\title{Net Present Value}
\usage{
VPL(fcl, tma)
}
\arguments{
\item{fcl}{the Net Cash Flow of the project}

\item{tma}{the attractiveness rate}
}
\value{
the net present value of the project
}
\description{
Net Present Value
}
\examples{
vpl_provavel <- VPL(fcl_provavel, tma = tma)
}
