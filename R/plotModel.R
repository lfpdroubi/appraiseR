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
#' # 1. Crete random bivariate normal data just for testing
#'
#' library(MASS)
#'
#' sample_cov <- matrix(c(1000^2, -25000,
#'                        -25000,  50^2),
#'                      ncol = 2, byrow = T)
#' n <- 50
#' set.seed(4)
#' dados <- mvrnorm(n = n,
#'                 mu = c(5000, 360),
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
#'           av = 6725, ca = TRUE,
#'           )
#'
#' # 2. Random data with variables transformation (log-log)
#'
#' m <- c(5000, 360)
#' s <- c(1000, 50)
#' r <- -0.75
#'
#' sigma <- sqrt(log(s^2/m^2 + 1))
#' mu <- log(m) - sigma^2/2
#' rho <- log(r*prod(s)/prod(m) + 1)
#'
#' set.seed(1)
#' dados <- exp(mvrnorm(n = n, mu = mu, Sigma = diag(sigma^2 - rho) + rho,
#'              empirical = TRUE))
#' colnames(dados) <- c("VU", "Area")
#' dados <- as.data.frame(dados)
#'
#' # Wrong fit:
#' wfit <- lm(VU ~ Area, data = dados)
#' plotModel(wfit, residuals = T)
#'
#' # Right fit:
#' fit <- lm(log(VU)~log(Area), data = dados)
#'
#' plotModel(fit)
#' plotModel(fit, residuals = T)
#' plotModel(fit, residuals = T, at = list(Area = 500))
#' plotModel(fit, residuals = T, at = list(Area = 500),
#'           interval = "both", level = .80,
#'           ca = TRUE)
#' plotModel(fit, residuals = T, at = list(Area = 500),
#'           interval = "both", level = .80,
#'           ca = TRUE, av = log(3850))
#'
#' plotModel(fit, residuals = T, at = list(Area = 500), FUN = "log",
#'           interval = "both", level = .80,
#'           ca = TRUE, av = 3850)
#'
#' # 3. Random data with 2 covariates
#'
#' ##                VU      Area    Frente
#' sigma <- matrix(c( 1000^2, -60000, -750,
#'                    -60000,  100^2,  240,
#'                    -750,      240, 3^2),
#'                 ncol = 3, byrow = T)
#'
#' set.seed(1)
#' dados <- mvrnorm(n=100,
#'                  mu=c(5000, 360, 12),
#'                  Sigma=sigma,
#'                  empirical = T)
#' colnames(dados) <- c("VU", "Area", "Frente")
#' dados <- as.data.frame(dados)
#'
#' fit <- lm(VU ~ Area + Frente, data = dados)
#'
#' plotModel(fit, residuals = TRUE)
#' plotModel(fit, vars = "Area", residuals=T)
#' plotModel(fit, vars = "Frente", residuals = T)
#' plotModel(fit, interval = "both", ca = TRUE, residuals = T,
#'           at = list(Area = 360, Frente = 12))
#'
#' plotModel(fit, interval = "both", ca = TRUE, residuals = T,
#'           at = list(Area = 360, Frente = 12),
#'           av = 5750)
#'
#' # 4. Random data with 2 covariates and transformations (Cobb-Douglas)
#'
#' m <- c(5000, 360, 12)
#' s <- c(1000, 50, 3)
#' ryx1 <- -0.60
#' ryx2 <- -0.25
#' rx1x2 <- .80
#'
#' sigma <- sqrt(log(s^2/m^2 + 1))
#' mu <- log(m) - sigma^2/2
#' rhoyx1 <- log(ryx1*prod(s[1:2])/prod(m[1:2]) + 1)
#' rhoyx2 <- log(ryx2*prod(s[c(1, 3)])/prod(m[c(1, 3)]) + 1)
#' rhox1x2 <- log(rx1x2*prod(s[2:3])/prod(m[2:3]) + 1)
#'
#' ##                     VU           Area         Frente
#' Sigma <- matrix(c(sigma[1]^2,       rhoyx1,      rhoyx2, # VU
#'                       rhoyx1,   sigma[2]^2,     rhox1x2, # Area
#'                       rhoyx2,      rhox1x2, sigma[3]^2), # Frente
#'                 ncol = 3, byrow = T)
#' set.seed(1)
#' dados <- exp(mvrnorm(n=100, mu=  mu, Sigma = Sigma,
#'                      empirical = T))
#' colnames(dados) <- c("VU", "Area", "Frente")
#' dados <- as.data.frame(dados)
#'
#' fit <- lm(log(VU) ~ log(Area) + log(Frente), data = dados)
#'
#' plotModel(fit, residuals = TRUE)
#'
#' plotModel(fit, interval = "both", ca = TRUE, residuals = T,
#'           at = list(Area = 450, Frente = 18), av = log(4850))
#'
#' plotModel(fit, interval = "both", ca = TRUE, residuals = T, FUN = "log",
#'           at = list(Area = 450, Frente = 18), av = 4850)
#'
#' # 5. With real data
#'
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
#'
#' # 5.1 Another model, with same data (apostila Prof. Norberto Hochheim)
#'
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
#' #
#' # plotModel.besfit()
#'
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
plotModel.lm <- function(object,
                         vars = parameters(object)$predictors,
                         ...){

  #params <- parameters(object)
  #vars <- params$predictors
  preds <- vars

  r <- length(preds)
  par1 <- round(sqrt(r))
  par2 <- ceiling((r)/par1)
  p <- list()

  for (i in preds) {
   p[[i]] <- plotVar(object = object, variable = i, ...)
  }

  z <- list(plots = p,
            par1 = par1,
            par2 = par2)
  class(z) <- "plotModel.lm"
  return(z)
}

#' @export
#'
print.plotModel.lm <- function(x, ...){
  gridExtra::grid.arrange(grobs =x$plots, nrow =x$par1, ncol = x$par2)
  #patchwork::wrap_plots(x$plots)
}