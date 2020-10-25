#' Plot bestfit models
#'
#' \code{plotmod} plots \code{\link{bestfit}} models
#'
#' @param object Object of class \code{\link{bestfit}}
#' @param fit the number of the chosen fit from the combination matrix (defaults
#'   for the best fit found with \code{\link{bestfit}}).
#' @param interval the type of interval calculation (provided to predict.lm) to
#'   be ploted with the model.
#' @param \dots further arguments passed to predict.lm.
#' @param level Tolerance/confidence level (provided to predict.lm) to be
#'   ploted.
#' @param func function used to transform the response (optional)
#' @param local Data Frame to be used for calculate the estimates
#' (defaults for center of each variable).
#' @export
#' @examples
#' data <- centro_2015@data
#' best_fit <- bestfit(valor ~ .,  data = data)
#' plotmod(best_fit, interval = "confidence")
#' plotmod(best_fit, fit = 2, interval = "confidence")
#' plotmod(best_fit, interval = "confidence",
#'         local = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'         dist_b_mar = 250, padrao = "medio"))
#'
#' fit <- lm(log(valor) ~ ., data = data)
#' plotmod(fit)
#' plotmod(fit, interval = "confidence")
#' plotmod(fit, interval = "confidence", func = "log")
#' plotmod(fit, interval = "confidence",
#'         local = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'         dist_b_mar = 250, padrao = "medio"))
#' plotmod(fit, interval = "confidence", func = "log",
#'         local = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'         dist_b_mar = 250, padrao = "medio"))
#'
plotmod <- function(object, ...) UseMethod("plotmod")

#' @rdname plotmod
#' @export

plotmod.bestfit <- function(object, fit = 1, ...){
  s <- summary(object, fit = fit)
  z <- s$fit
  func <- as.character(s$bestfit[, object$response])
  terms <- object$predictors

  r <- length(terms)
  par1 <- round(sqrt(r))
  par2 <- ceiling((r)/par1)
  p <- list()

  for (i in terms) {
    p[[i]] <- plotvar(object = z, variable = i, ...)
  }

  est <- list(plots = p,
              par1 = par1,
              par2 = par2)
  class(est) <- "plotmod.bestfit"
  return(est)
}

#' @rdname plotmod
#' @export
plotmod.lm <- function(object, func = "identity", ...){
  z <- object
  preds <- parameters(z)$predictors

  r <- length(preds)
  par1 <- round(sqrt(r))
  par2 <- ceiling((r)/par1)
  p <- list()

  for (i in preds) {
   p[[i]] <- plotvar(object = z, variable = i, ...)
  }

  est <- list(plots = p,
              par1 = par1,
              par2 = par2)
  class(est) <- "plotmod.lm"
  return(est)
}

#' @export
#'
print.plotmod.lm <- function(x, ...){
  gridExtra::grid.arrange(grobs =x$plots, nrow =x$par1, ncol = x$par2)
}

#' @export
#'
print.plotmod.bestfit <- function(x, ...){
  gridExtra::grid.arrange(grobs =x$plots, nrow =x$par1, ncol = x$par2)
}