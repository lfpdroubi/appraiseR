#'Based on a lm object, returns reasoning degree parameters according to
#'NBR14.653-2.
#'
#'Reasoning degree based on NBR14.653-2.
#'
#'@param object object of class "lm"
#'@export
#' @examples
#' dados <- centro_2015@data
#' fit <- lm(log(valor) ~ ., data = dados)
#' grau(fit)

grau <- function(object){
  s <- summary(object)
  param <- parameters(object)
  preds <- param$predictors
  resp <- param$response
  data <- param$data

  n <- nrow(data)
  k <- length(preds)

  nmin <- ifelse(n >= 6*(k+1), sprintf("n = %i >= %i --> Grau III", n, 6*(k+1)),
                 ifelse(n >= 4*(k+1), sprintf("%i <= n = %i < %i --> Grau II", 4*(k+1), n, 6*(k+1)),
                        ifelse(n >= 3*(k+1), sprintf("%i <= n = %i < %i --> Grau I", 3*(k+1), n, 4*(k+1)),
                               sprintf("n = %i < %i --> Fora de Especifica\u00E7\u00E3o", n, 3*(k+1)))))

  tstats <- s$coefficients[,"Pr(>|t|)"]
  max_t <- max(tstats)

  tmax <- ifelse(max_t < .1, sprintf("t m\u00e1ximo = %.2f %%  < 10%% --> Grau III", 100*max_t),
                 ifelse(max_t < .2, sprintf("10%% < t m\u00e1ximo = %.2f %% < 20%% --> Grau II", 100*max_t),
                        ifelse(max_t < .3, sprintf("20%% < t m\u00e1ximo = %.2f %% < 30%% --> Grau I", 100*max_t),
                               sprintf("t m\u00e1ximo = %.2f %%  > 30%% --> Fora de Especificacao", 100*max_t))))

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