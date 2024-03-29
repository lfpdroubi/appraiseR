% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elasticidade.R
\name{elasticidade}
\alias{elasticidade}
\title{Elasticity calculation function}
\usage{
elasticidade(object, variable, func, at, factor = 0.1)
}
\arguments{
\item{object}{object of class "lm"}

\item{variable}{regressor to calculate elasticity}

\item{func}{transformation applied to dependent variable}

\item{at}{point in which elasticity will be calculated}

\item{factor}{factor applied to calculate elasticity}
}
\description{
Based on a lm object, returns elasticity based in any parameter in the model.
}
\examples{
library(sf)
data(centro_2015)
fit <- lm(log(valor) ~ area_total + quartos + suites + garagens +
             dist_b_mar + padrao,
             data = centro_2015)
new <- data.frame(area_total = 205, quartos = 3, suites = 1,
             garagens = 2, dist_b_mar = 250, padrao = "medio")
(p <- predict(fit, newdata = new))
new1 <- data.frame(area_total = 1.1*205, quartos = 3, suites = 1,
             garagens = 2, dist_b_mar = 250, padrao = "medio")
(p1 <- predict(fit, newdata = new1))
(exp(p1) - exp(p))/exp(p)
elasticidade(fit, "area_total", func = "log",
             at = list(area_total = 205, quartos = 3, suites = 1,
             garagens = 2, dist_b_mar = 250, padrao = "medio"))
elasticidade(fit, "dist_b_mar", func = "log",
             at = list(area_total = 205, quartos = 3, suites = 1,
             garagens = 2, dist_b_mar = 250, padrao = "medio"))
elasticidade(fit, "padrao", func = "log",
             at = list(area_total = 205, quartos = 3, suites = 1,
             garagens = 2, dist_b_mar = 250, padrao = "medio"),
             factor = +1)
elasticidade(fit, "padrao", func = "log",
             at = list(area_total = 205, quartos = 3, suites = 1,
             garagens = 2, dist_b_mar = 250, padrao = "medio"),
             factor = -1)
}
