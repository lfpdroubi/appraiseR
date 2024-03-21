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
#' # 1. Random generated data
#'
#' # 1.1 Bivariate normal data just for testing
#'
#' library(MASS)
#'
#' sample_cov <- matrix(c(1000^2, -40000,
#'                        -40000,  50^2),
#'                      ncol = 2, byrow = T)
#' n <- 50
#' set.seed(4)
#' dados <- mvrnorm(n = n,
#'                 mu = c(5000, 360),
#'                 Sigma = sample_cov,
#'                 empirical = T)
#' colnames(dados) <- c("PU", "Area")
#' dados <- as.data.frame(dados)
#'
#' fit <- lm(PU ~ Area, data = dados)
#'
#' plotModel(fit)
#' plotModel(fit, residuals = T)
#' plotModel(fit, residuals = T, at = list(Area = 275))
#' plotModel(fit, residuals = T, at = list(Area = 275),
#'           interval = 'confidence', level = .95)
#' plotModel(fit, residuals = T, at = list(Area = 275),
#'           interval = 'prediction', level = .95)
#' plotModel(fit, residuals = T, at = list(Area = 275),
#'           interval = "both", level = .80,
#'           av = 7250, ca = TRUE,
#'           )
#'
#' # 1.2 Data with variables transformation (log-log)
#'
#' m <- c(5000, 360)
#' s <- c(2000, 50)
#' r <- -0.75
#'
#' sigma <- sqrt(log(s^2/m^2 + 1))
#' mu <- log(m) - sigma^2/2
#' rho <- log(r*prod(s)/prod(m) + 1)
#'
#' set.seed(2)
#' dados <- exp(mvrnorm(n = n, mu = mu, Sigma = diag(sigma^2 - rho) + rho,
#'              empirical = TRUE))
#' colnames(dados) <- c("PU", "Area")
#' dados <- as.data.frame(dados)
#'
#' # Wrong fit:
#' wfit <- lm(PU ~ Area, data = dados)
#' plotModel(wfit, interval = 'confidence', level = .95, residuals = T)
#' plotModel(wfit, residuals = T, at = list(Area = 450)) #~ 2.225,00 R$/m2
#'
#' # Right fit:
#' fit <- lm(log(PU)~log(Area), data = dados)
#'
#' plotModel(fit, interval = 'confidence', level = .95, residuals = T)
#' plotModel(fit, interval = 'confidence', level = .95,
#'           residuals = T, at = list(Area = 450)) #~ 2.765,00 R$/m2 (+24%)
#'
#' # Testing if arbitrating R$ 1.400.000,00 (3.111 R$/m2) is a good idea:
#'
#' plotModel(fit, residuals = T, at = list(Area = 450),
#'           interval = "both", level = .80,
#'           ca = TRUE, av = log(3111))
#'
#' # Visualizing in the original scale
#'
#' plotModel(fit, residuals = T, at = list(Area = 450), FUN = "log",
#'           interval = "both", level = .80,
#'           ca = TRUE, av = 3111)
#'
#' # 1.3 Data with 2 covariates
#'
#' ##                PU      Area    Frente
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
#' colnames(dados) <- c("PU", "Area", "Frente")
#' dados <- as.data.frame(dados)
#'
#' fit <- lm(PU ~ Area + Frente, data = dados)
#'
#' plotModel(fit, interval = 'confidence', level = .95, residuals = TRUE)
#' plotModel(fit, vars = "Area", residuals = T)
#' plotModel(fit, vars = "Frente", residuals = T)
#' plotModel(fit, residuals = T, at = list(Area = 360, Frente = 12))
#'
#' plotModel(fit, interval = "both", ca = TRUE, residuals = T,
#'           at = list(Area = 360, Frente = 12),
#'           av = 5750)
#'
#' plotModel(fit, residuals = T, at = list(Area = 600, Frente = 5))
#' plotModel(fit, interval = "both", ca = TRUE, residuals = T,
#'            at = list(Area = 600, Frente = 5))
#'
#' # 1.4 Data with 2 covariates and transformations (Cobb-Douglas)
#'
#' m <- c(5000, 360, 12)
#' s <- c(500, 150, 3)
#' ryx1 <- -0.65
#' ryx2 <- -0.15
#' rx1x2 <- .80
#'
#' sigma <- sqrt(log(s^2/m^2 + 1))
#' mu <- log(m) - sigma^2/2
#' rhoyx1 <- log(ryx1*prod(s[1:2])/prod(m[1:2]) + 1)
#' rhoyx2 <- log(ryx2*prod(s[c(1, 3)])/prod(m[c(1, 3)]) + 1)
#' rhox1x2 <- log(rx1x2*prod(s[2:3])/prod(m[2:3]) + 1)
#'
#' ##                     PU           Area         Frente
#' Sigma <- matrix(c(sigma[1]^2,       rhoyx1,      rhoyx2, # PU
#'                       rhoyx1,   sigma[2]^2,     rhox1x2, # Area
#'                       rhoyx2,      rhox1x2, sigma[3]^2), # Frente
#'                 ncol = 3, byrow = T)
#' set.seed(1)
#' dados <- exp(mvrnorm(n=100, mu=  mu, Sigma = Sigma,
#'                      empirical = T))
#' colnames(dados) <- c("PU", "Area", "Frente")
#' dados <- as.data.frame(dados)
#'
#' # Wrong fit:
#' wfit <- lm(PU ~ Area + Frente, data = dados)
#'
#' plotModel(wfit, interval = 'confidence', level = .95, residuals = T)
#' plotModel(wfit, residuals = T, at = list(Area = 360, Frente = 12))# not bad
#' plotModel(wfit, residuals = T, at = list(Area = 875, Frente = 20))# very bad
#'
#' # Good fit:
#'
#' fit <- lm(log(PU) ~ log(Area) + log(Frente), data = dados)
#'
#' plotModel(fit, residuals = T)
#'
#' plotModel(fit, residuals = T, at = list(Area = 360, Frente = 12)) # good!
#' plotModel(fit, residuals = T, at = list(Area = 875, Frente = 20)) # good!
#'
#' # Testing av = R$ 4.200.000,00 (4800 R$/m2)
#' plotModel(fit, interval = "both", ca = TRUE, residuals = T, FUN = "log",
#'           at = list(Area = 875, Frente = 20), av = 4800)
#'
#' # 1.5 Data with one continuous regressor plus one factor
#'
#' n = 25
#' set.seed(1)
#' Area <- rnorm(n = 3*n, mean = 360, sd = 50)
#'
#' Bairro <- factor(sample(LETTERS[1:3], size = 3*n, replace = T))
#'
#' dados <- data.frame(Area, Bairro)
#'
#' dados <- within(dados, {
#'   PU <- ifelse(Bairro == "A", 7500,
#'                ifelse(Bairro == "B", 10000, 12500))
#'   PU <- PU - 15*Area + rnorm(n = 3*n, mean = 0, sd = 1000)
#' })
#'
#' fit <- lm(PU ~ Area + Bairro, data = dados)
#'
#' plotModel(fit, residuals = T, colour = Bairro)
#'
#' # 2 With real data
#'
#' library(sf)
#' data(centro_2015)
#' centro_2015 <- within(centro_2015, PU <- valor/area_total)
#' fit <- lm(log(PU) ~ log(area_total) + quartos + suites + garagens +
#'             log(dist_b_mar) + padrao, data = centro_2015)
#' plotModel(fit)
#' plotModel(fit, residuals = T)
#' plotModel(fit, residuals = T,
#'          at = data.frame(area_total = 205, quartos = 3, suites = 1,
#'                           garagens = 2, dist_b_mar = 250, padrao = 'medio'))
#' plotModel(fit, residuals = T, interval = 'confidence', level = .80,
#'          at = data.frame(area_total = 205, quartos = 3, suites = 1,
#'                           garagens = 2, dist_b_mar = 250, padrao = 'medio'))
#' plotModel(fit, residuals = T, interval = "both", level = .80, ca = TRUE)
#' plotModel(fit, residuals = T, interval = "both", ca = TRUE,
#'          at = data.frame(area_total = 205, quartos = 3, suites = 1,
#'                           garagens = 2, dist_b_mar = 250, padrao = 'medio'),
#'           av = log(5600))
#' ## On the original scale
#' plotModel(fit, interval = "both", level = .80, FUN = "log", ca = TRUE)
#' plotModel(fit, interval = "both", level = .80, FUN = "log",
#'           at = data.frame(area_total = 205, quartos = 3, suites = 1,
#'                           garagens = 2, dist_b_mar = 250, padrao = 'medio'),
#'           ca = TRUE)
#' plotModel(fit, interval = "both", level = .80, FUN = "log",
#'           at = data.frame(area_total = 205, quartos = 3, suites = 1,
#'                           garagens = 2, dist_b_mar = 250, padrao = 'medio'),
#'           ca = TRUE, av = 5600)
#'
#' # 2.1 Another model, with same data (apostila Prof. Norberto Hochheim)
#'
#' dados <- within(centro_2015, padrao <- as.numeric(padrao))
#' fit1 <- lm(log(valor) ~ area_total + quartos + suites + garagens +
#'             log(dist_b_mar) +I(1/padrao),
#'             data = dados, subset = -c(31, 39))
#' plotModel(fit1, residuals = T, colour = factor(padrao))
#' plotModel(fit1, interval = "both", level = .80,
#'           at = data.frame(area_total = 205, quartos = 3, suites = 1,
#'                           garagens = 2, dist_b_mar = 250, padrao = 2),
#'           ca = TRUE, av = log(1100000),
#'           residuals = T, colou = factor(padrao))
#'
#' # 2.2 Yet another model with the same data
#'
#' fit2 <- lm(rsqrt(PU) ~ rsqrt(area_total) + quartos + suites + garagens +
#'             rsqrt(dist_b_mar) + padrao, data = centro_2015)
#' plotModel(fit2)
#' plotModel(fit2, residuals = T)
#' plotModel(fit2, FUN = "rsqrt")
#' plotModel(fit2, FUN = "rsqrt", interval = "confidence")
#' plotModel(fit2, FUN = "rsqrt", interval = "both", ca = TRUE)
#' plotModel(fit2, FUN = "rsqrt", interval = "both", ca = TRUE,
#'           at = data.frame(area_total = 205, quartos = 3, suites = 1,
#'                           garagens = 2, dist_b_mar = 250, padrao = "medio"),
#'           av = 5335)
#'
#' # 2.3 Atibaia
#'
#' data(atibaia)

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