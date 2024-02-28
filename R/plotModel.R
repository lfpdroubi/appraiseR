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
#' @param FUN function used to transform the response (optional)
#' @param at Data Frame to be used to calculate the predictions, instead of
#' mean values.
#' (defaults for center of each variable).
#' @export
#' @examples
#' # Crete random bivariate normal data just for testing
#' library(MASS)
#' sample_mean <- c(10000, 250)
#' sample_cov <- matrix(c(1000^2, -37500,
#'                        -37500, 50^2),
#'                      ncol = 2, byrow = T)
#' n <- 10
#' set.seed(4)
#' dados <- mvrnorm(n = n,
#'                 mu = sample_mean,
#'                 Sigma = sample_cov,
#'                 empirical = T)
#' colnames(dados) <- c("VU", "Area")
#' dados <- as.data.frame(dados)
#'
#' fit <- lm(VU ~ Area, data = dados)
#'
#' plotModel(fit)
#' plotModel(fit, residuals = T)
#' plotModel(fit, residuals = T, at = list(Area = 275))
#' plotModel(fit, residuals = T, at = list(Area = 275),
#'           interval = "both", level = .80,
#'           av = 11000, ca = TRUE,
#'           )
#' #
#' # With real data:
#' #
#' library(sf)
#' data(centro_2015)
#' centro_2015 <- within(centro_2015, VU <- valor/area_total)
#' fit <- lm(log(VU) ~ log(area_total) + quartos + suites + garagens +
#'             log(dist_b_mar) + padrao,
#'           data = centro_2015)
#' plotModel(fit)
#' plotModel(fit, interval = "both", level = .80, ca = TRUE)
#' plotModel(fit, interval = "both", level = .80, ca = TRUE, residuals = T)
#' plotModel(fit, at = list(area_total = 205, quartos = 3, suites = 1,
#'                          garagens = 2, dist_b_mar = 250, padrao = 'medio'))
#' plotModel(fit, at = list(area_total = 205, quartos = 3, suites = 1,
#'                          garagens = 2, dist_b_mar = 250, padrao = 'medio'),
#'           interval = "both")
#' plotModel(fit, at = list(area_total = 205, quartos = 3, suites = 1,
#'                          garagens = 2, dist_b_mar = 250, padrao = 'medio'),
#'           interval = "both", ca = TRUE)
#' plotModel(fit, at = list(area_total = 205, quartos = 3, suites = 1,
#'                          garagens = 2, dist_b_mar = 250, padrao = 'medio'),
#'           interval = "both", ca = TRUE, residuals = TRUE,
#'           av = log(5650))
#' ## On the original scale
#' plotModel(fit, interval = "both", level = .80, FUN = "log", ca = TRUE)
#' plotModel(fit, interval = "both", level = .80, FUN = "log",
#'           at = data.frame(area_total = 205, quartos = 3, suites = 1,
#'                           garagens = 2, dist_b_mar = 250, padrao = 'medio'),
#'           ca = TRUE)
#' plotModel(fit, interval = "both", level = .80, FUN = "log",
#'           at = data.frame(area_total = 205, quartos = 3, suites = 1,
#'                           garagens = 2, dist_b_mar = 250, padrao = 'medio'),
#'           ca = TRUE, av = 5650)
#' # Modelo apostila Prof. Norberto Hochheim
#' dados <- within(centro_2015, padrao <- as.numeric(padrao))
#' fit1 <- lm(log(valor) ~ area_total + quartos + suites + garagens +
#'             log(dist_b_mar) +I(1/padrao),
#'             data = dados, subset = -c(31, 39))
#' plotModel(fit1, interval = "both", level = .80,
#'           at = data.frame(area_total = 205, quartos = 3, suites = 1,
#'                           garagens = 2, dist_b_mar = 250, padrao = 2),
#'           ca = TRUE, av = log(1100000),
#'           residuals = T)

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
#'           at = list(area_total = 205, quartos = 3, suites = 1,
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
#'           at = list(area_total = 205, quartos = 3, suites = 1,
#'           garagens = 2, dist_b_mar = 250, padrao = 2), av = 1100000)
#' plotModel(best_fit, interval = "confidence") # choose another fit
#' plotModel(best_fit, interval = "confidence", scale = "original",
#'         at = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'         dist_b_mar = 250, padrao = 2))
#'
#' @export

plotModel.bestfit <- function(object, fit = 1,
                              scale = c("transformed", "original"), ...){
  scale <- match.arg(scale)
  s <- summary(object, fit = fit)
  z <- s$fit
  response <- object$response
  FUN <- as.character(object$tab[fit, response])

  if (scale == "original"){
    p <- plotModel.lm(z, FUN = FUN, ...)
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
  #patchwork::wrap_plots(x$plots)
}