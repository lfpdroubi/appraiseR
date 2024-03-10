#'Based on a lm object, returns reasoning degree parameters according to
#'NBR14.653-2.
#'
#'Reasoning degree based on NBR14.653-2.
#'
#'@param object object of class "lm"
#'@export
#' @examples
#' library(sf)
#' data(centro_2015)
#' centro_2015 <- within(centro_2015, VU <- valor/area_total)
#' fit <- lm(VU ~ log(area_total) + quartos + suites + garagens +
#'            log(dist_b_mar) + padrao,
#'            data = centro_2015, subset = -c(31,39))
#' summary(fit) # R2 ajustado = 0.66
#' grau(fit) # grau II
#'
#' fit2 <- lm(VU ~ log(area_total/205) + I(quartos-3) +
#'             I(garagens-2) +  log(dist_b_mar/250) + padrao - 1,
#'             data = centro_2015, subset = -c(31,39))
#' summary(fit2) # R2 ajustado = 0.98
#' grau(fit2) # grau III
#'
#' # Predictions
#'
#' p <- predict(fit, interval = 'confidence', level = 0.80,
#'               newdata = list(area_total = 205, quartos = 3, suites = 1,
#'                            garagens = 2, dist_b_mar = 250, padrao = 'medio'))
#' p2 <- predict(fit2, interval = 'confidence', level = 0.80,
#'                 newdata = list(area_total = 205, quartos = 3, suites = 1,
#'                            garagens = 2, dist_b_mar = 250, padrao = 'medio'))

grau <- function(object){
  z <- object
  params <- parameters(z)

  s <- summary(z)
  preds <- params$predictors
  resp <- params$response

  n <- nrow(z$model)
  k <- length(preds)

  nmin <- ifelse(n >= 6*(k+1), sprintf("n = %i >= %i --> Grau III", n, 6*(k+1)),
                 ifelse(n >= 4*(k+1), sprintf("%i <= n = %i < %i --> Grau II", 4*(k+1), n, 6*(k+1)),
                        ifelse(n >= 3*(k+1), sprintf("%i <= n = %i < %i --> Grau I", 3*(k+1), n, 4*(k+1)),
                               sprintf("n = %i < %i --> Fora de Especifica\u00E7\u00E3o", n, 3*(k+1)))))

  tstats <- s$coefficients[!(rownames(s$coefficients) %in% "(Intercept)"), "Pr(>|t|)"]
  max_t <- max(tstats)

  tmax <- ifelse(max_t < .1, sprintf("t m\u00e1ximo = %.2f %%  < 10%% --> Grau III", 100*max_t),
                 ifelse(max_t < .2, sprintf("10%% < t m\u00e1ximo = %.2f %% < 20%% --> Grau II", 100*max_t),
                        ifelse(max_t < .3, sprintf("20%% < t m\u00e1ximo = %.2f %% < 30%% --> Grau I", 100*max_t),
                               sprintf("t m\u00e1ximo = %.2f %%  > 30%% --> Fora de Especifica\u00E7\u00E3o", 100*max_t))))

  f <- s$fstatistic
  pVal <- stats::pf(f[1], f[2], f[3], lower.tail = FALSE)

  fstat <- ifelse(pVal < .01, sprintf("p-valor F = %.2e %% < 1%% --> Grau III", 100*pVal),
                  ifelse(pVal < .02, sprintf("1%% < p-valor F =  %.2e %% < 2%% --> Grau II", 100*pVal),
                         ifelse(pVal < .05, sprintf("2%% < p-valor F =  %.2e %% < 5%% --> Grau I", 100*pVal),
                                sprintf("p-valor F = %.2e %% > 5%% --> Fora de Especifica\u00E7\u00E3o", 100*pVal))))

  attributes(fstat) <- NULL

  est <- list(nmin = nmin,
              tmax = tmax,
              fstat = fstat)

  return(est)
}