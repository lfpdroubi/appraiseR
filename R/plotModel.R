#' Plot bestfit models
#'
#' \code{plotModel} plots \code{\link{bestfit}} models
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
#' library(sf)
#' data <- st_drop_geometry(centro_2015)
#' dados <- data
#' dados$padrao <- as.numeric(dados$padrao)
#' ## plotModel.lm
#' fit <- lm(log(valor)~area_total + quartos + suites + garagens +
#'             log(dist_b_mar) + I(1/padrao),
#'             data = dados, subset = -c(31, 39))
#' plotModel(fit)
#' plotModel(fit, interval = "both", level = .80)
#' ## On the original scale
#' plotModel(fit, interval = "both", level = .80, func = "log")
#' plotModel(fit, interval = "both", level = .80, func = "log", ca = TRUE,
#'         local = data.frame(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'                      dist_b_mar = 250, padrao = 2))
#' plotModel(fit, interval = "both", level = .80, func = "log", ca = TRUE,
#'         local = data.frame(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'                      dist_b_mar = 250, padrao = 2),
#'           av = 1100000)
#' mod <- lm(log(valor) ~ ., data = data)
#' plotModel(mod)
#' plotModel(mod, interval = "confidence")
#' plotModel(mod, interval = "confidence", func = "log")
#' plotModel(mod, interval = "confidence",
#'         local = data.frame(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'         dist_b_mar = 250, padrao = "medio"))
#' plotModel(mod, interval = "confidence", func = "log",
#'         local = data.frame(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'         dist_b_mar = 250, padrao = "medio"))
#'
plotModel <- function(object, ...) UseMethod("plotModel")

#' @rdname plotModel
#' @param fit chosen fit
#' @param scale Plot model under original or transformed scale?
#' @examples
#' dados <- st_drop_geometry(centro_2015)
#' best_fit <- bestfit(valor ~ .,  data = dados)
#' plotModel(best_fit)
#' plotModel(best_fit, fit = 514, scale = "original", interval = "both",
#'           ca = TRUE,
#'           local = list(area_total = 205, quartos = 3, suites = 1,
#'           garagens = 2, dist_b_mar = 250, padrao = "medio"),
#'           av = 1100000)
#' best_fit <- bestfit(valor ~ .,  data = data)
#' plotModel(best_fit, fit = 514) # Plots the best fit
#' plotModel(best_fit, fit = 514, interval = "confidence") # adds CIs
#' plotModel(best_fit, fit = 514, scale = "original") # plots in the original scale
#' plotModel(best_fit, fit = 514, scale = "original", interval = "both",
#'           ca = TRUE)
#' plotModel(best_fit, fit = 514, scale = "original", interval = "both",
#'           ca = TRUE,
#'           local = list(area_total = 205, quartos = 3, suites = 1,
#'           garagens = 2, dist_b_mar = 250, padrao = 2), av = 1100000)
#' plotModel(best_fit, interval = "confidence") # choose another fit
#' plotModel(best_fit, interval = "confidence", scale = "original",
#'         local = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'         dist_b_mar = 250, padrao = 2))
#'
#' @export

plotModel.bestfit <- function(object, fit = 1,
                              scale = c("transformed", "original"), ...){
  scale <- match.arg(scale)
  s <- summary(object, fit = fit)
  z <- s$fit
  response <- object$response
  func <- as.character(object$tab[fit, response])

  if (scale == "original"){
    p <- plotModel.lm(z, func = func, ...)
  } else {
    p <- plotModel.lm(z, ...)
  }
  return(p)
}

#' @rdname plotModel
#' @export
plotModel.lm <- function(object, ...){
  z <- object
  preds <- parameters(z)$predictors

  r <- length(preds)
  par1 <- round(sqrt(r))
  par2 <- ceiling((r)/par1)
  p <- list()

  for (i in preds) {
   p[[i]] <- plotVar(object = z, variable = i, ...)
  }

  est <- list(plots = p,
              par1 = par1,
              par2 = par2)
  class(est) <- "plotModel.lm"
  return(est)
}

#' @export
#'
print.plotModel.lm <- function(x, ...){
  gridExtra::grid.arrange(grobs =x$plots, nrow =x$par1, ncol = x$par2)
}