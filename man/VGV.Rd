% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/involutivo.R
\name{VGV}
\alias{VGV}
\title{Overall Sales Value calculation}
\usage{
VGV(area_construida, valor_unitario)
}
\arguments{
\item{area_construida}{Building area (sq. meters)}

\item{valor_unitario}{unit selling value ($/sq. meters)}
}
\value{
The Overall Sales Value
}
\description{
Overall Sales Value calculation
}
\examples{
vgv <- VGV(20*94.5, 7000)
}
