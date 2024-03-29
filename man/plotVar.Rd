% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotVar.R
\name{plotVar}
\alias{plotVar}
\title{Plot model predictors against the response variable with intervals}
\usage{
plotVar(
  object,
  variable,
  FUN,
  interval = c("none", "confidence", "prediction", "both"),
  level = 0.8,
  ca = FALSE,
  av,
  at,
  ...
)
}
\arguments{
\item{object}{object of class lm}

\item{variable}{variable to be plotted against response variable}

\item{FUN}{function used to transform the response (optional)}

\item{interval}{the type of interval calculation (provided to predict.lm) to
be ploted.}

\item{level}{Tolerance/confidence level (provided to predict.lm) to
be ploted.}

\item{ca}{(T/F) should the limits of the invertal of arbitration be plotted?}

\item{av}{(T/F) should the arbitrated value be plotted?}

\item{at}{list to be used for calculate the estimates
(defaults for center of each variable).}

\item{\dots}{further arguments passed to predict.lm.}
}
\description{
\code{plotvar} plots model variables with confidence/prediction intervals
}
\examples{
library(sf)
data(centro_2015)
centro_2015 <- within(centro_2015, VU <- valor/area_total)
fit <- lm(log(VU) ~ log(area_total) + quartos + suites + garagens +
            log(dist_b_mar) + padrao,
          data = centro_2015)
plotVar(fit, "area_total")
plotVar(fit, "area_total", residuals = TRUE)
plotVar(fit, "area_total", residuals = TRUE, colour = padrao)
plotVar(fit, "area_total", interval = "confidence", residuals = TRUE)
plotVar(fit, "area_total", interval = "prediction", residuals = TRUE)
plotVar(fit, "area_total", interval = "both", residuals = TRUE)
plotVar(fit, "area_total", interval = "both", ca = TRUE, residuals = TRUE)
plotVar(fit, "area_total", FUN = "log")
plotVar(fit, "area_total", FUN = "log",
        interval = "both", ca = TRUE, residuals = TRUE)
plotVar(fit, "suites")
plotVar(fit, "suites", interval = "confidence")
plotVar(fit, "suites", interval = "confidence", FUN = "log")

# Plot model passing through point 'at'

plotVar(fit, "area_total", interval = "both", ca = TRUE,
        at = list(area_total = 205, quartos = 3, suites = 1,
                  garagens = 2, dist_b_mar = 250, padrao = "medio"))
# Same above + Point valuation at R$ 5.650,00 /m2
plotVar(fit, "area_total", interval = "both",
        at = data.frame(area_total = 205, quartos = 3, suites = 1,
                        garagens = 2, dist_b_mar = 250, padrao = "medio"),
        ca = TRUE, av = log(5650))
# Same above, in the original scale
plotVar(fit, "area_total", interval = "both", FUN = 'log',
        at = data.frame(area_total = 205, quartos = 3, suites = 1,
                        garagens = 2, dist_b_mar = 250, padrao = "medio"),
        ca = TRUE, av = 5650)

# Plotting factors

plotVar(fit, "padrao")
plotVar(fit, "padrao", ca = TRUE)
plotVar(fit, "padrao", FUN = "log", ca = TRUE)
plotVar(fit, "padrao", interval = "confidence")
plotVar(fit, "padrao", interval = "prediction")
plotVar(fit, "padrao", interval = "both")
plotVar(fit, "padrao", FUN = "log", interval = "confidence",
        at = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
                  dist_b_mar = 250, padrao = "medio"))
plotVar(fit, "padrao", FUN = "log", interval = "prediction",
        at = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
                  dist_b_mar = 250, padrao = "medio"),
        av = 5650)
# Remove outliers
fit2 <- update(fit, .~.-suites, subset = -c(31,39, 45))
plotVar(fit2, "padrao", FUN = "log", interval = "prediction",
        at = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
                  dist_b_mar = 250, padrao = "medio"),
        av = 5650)
plotVar(fit2, "area_total", FUN = "log", interval = "both",
        at = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
                  dist_b_mar = 250, padrao = "medio"),
        ca = TRUE, av = 5650)
plotVar(fit2, "dist_b_mar", FUN = "log", interval = "both",
        at = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
                  dist_b_mar = 250, padrao = "medio"),
        ca = TRUE, av = 5650)
}
