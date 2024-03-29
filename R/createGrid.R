#' Create grid for predictions
#'
#' \code{createGrid} create grids to predictions for ploting model variables
#' (terms) with or without confidence/prediction intervals
#' @param object object of class lm
#' @param variable variable to be plotted against response variable
#' @param func function used to transform the response (optional)
#' @param interval the type of interval calculation (provided to predict.lm) to
#'   be ploted.
#' @param level Tolerance/confidence level (provided to predict.lm) to
#'   be ploted.
#' @param ca (T/F) should the limits of the invertal of arbitration be plotted?
#' @param av (T/F) should the arbitrated value be plotted?
#' @param at List to be used to calculate the predictions
#' (defaults for center of each variable).
#' @param \dots further arguments passed to predict.lm.
#' @export
#' @examples
#' # Crete random bivariate normal data just for testing
#' library(MASS)
#' sample_mean <- c(10000, 250)
#' sample_cov <- matrix(c(1000^2, -37500,
#'                        -37500, 50^2),
#'                      ncol = 2, byrow = T)
#' n <- 10
#' set.seed(1)
#' dados <- mvrnorm(n = n,
#'                 mu = sample_mean,
#'                 Sigma = sample_cov)
#' colnames(dados) <- c("VU", "Area")
#' dados <- as.data.frame(dados)
#'
#' fit <- lm(VU ~ Area, data = dados)
#' createGrid("Area", fit)
#' #
#' # Test with real data
#' library(sf)
#' data(centro_2015)
#' centro_2015 <- within(centro_2015, PU <- valor/area_total)
#' fit <- lm(sqrt(PU) ~ sqrt(area_total) + quartos + suites + garagens +
#'           sqrt(dist_b_mar) + padrao,
#'           data = centro_2015)
#'
#' # Continuos variables
#'
#' createGrid("area_total", fit)
#' createGrid("area_total", fit,
#'            at = list(area_total = 205, quartos = 3, suites = 1,
#'                      garagens = 2, dist_b_mar = 250, padrao = "medio"))
#'
#' # Factors
#'
#' createGrid("padrao", fit)
#' createGrid("padrao", fit,
#'            at = list(area_total = 205, quartos = 3, suites = 1,
#'                      garagens = 2, dist_b_mar = 250, padrao = "medio"))
#'
#' # Random generated data
#'
#' n <- 30
#' Bairro_A <- rnorm(n, mean = 500, sd = 100)
#' Bairro_B <- rnorm(n, mean = 750, sd = 100)
#' Bairro_C <- rnorm(n, mean = 1000, sd = 100)
#'
#' dados <- data.frame(VU = c(Bairro_A, Bairro_B, Bairro_C),
#'                     Bairro = c(rep("A", n), rep("B", n), rep("C", n))
#' )
#'
#' fit <- lm(VU ~ Bairro, data = dados)
#'
#' createGrid("Bairro", fit)

createGrid <- function(x, object, at, ...){
  variable <- x
  params <- parameters(object)
  parametros <- params$parameters
  response <- params$response
  predictors <- params$predictors
  mframe <- expand.model.frame(object, extras = parametros, na.expand = TRUE)

  if (is.numeric(mframe[, variable])) {
    grid <- seq(min(mframe[, variable], na.rm = TRUE),
                max(mframe[, variable], na.rm = TRUE),
                length = 101)
  } else if (is.factor(mframe[, variable]) | is.character(mframe[, variable])){
    grid <- unique(mframe[, variable])
  }

  if (length(predictors)>1) {
    if (missing(at)) {
      new <- data.frame(grid, lapply(mframe[, setdiff(predictors, variable),drop=F],
                                     FUN = centre))
      p_local <- NULL
    } else {
      new <- data.frame(grid, at[setdiff(predictors, variable)])
      p_local <- predict(object, newdata = at)
    }
  } else {
    new <- data.frame(grid)
    if (missing(at)){
      p_local <- NULL
    } else
      p_local <- predict(object, newdata = at)
  }

  names(new)[1] <- variable

  z <- list()
  z$new <- new
  z$p_local <- p_local
  z$mframe <- mframe

  return(z)
}
