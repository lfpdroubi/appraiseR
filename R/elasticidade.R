#' Elasticity calculation function
#'
#' Based on a lm object, returns elasticity based in any parameter in the model.
#'
#'
#' @param object object of class "lm"
#' @param variable regressor to calculate elasticity
#' @param func transformation applied to dependent variable
#' @param local point in which elasticity will be calculated
#' @param factor factor applied to calculate elasticity
#' @export
#' @examples
#' data <- centro_2015@data
#' fit <- lm(log(valor) ~ ., data = data)
#' elasticidade(fit, "area_total",
#'              local = list(area_total = 205, quartos = 3, suites = 1,
#'              garagens = 2, dist_b_mar = 250, padrao = "medio"))
#' elasticidade(fit, "dist_b_mar",
#'              local = list(area_total = 205, quartos = 3, suites = 1,
#'              garagens = 2, dist_b_mar = 250, padrao = "medio"))
#' elasticidade(fit, "area_total",
#'              local = list(area_total = 205, quartos = 3, suites = 1,
#'              garagens = 2, dist_b_mar = 250, padrao = "medio"),
#'              factor = -0.1)
#' elasticidade(fit, "padrao",
#'              local = list(area_total = 205, quartos = 3, suites = 1,
#'              garagens = 2, dist_b_mar = 250, padrao = "medio"),
#'              factor = +1)
#' elasticidade(fit, "padrao",
#'              local = list(area_total = 205, quartos = 3, suites = 1,
#'              garagens = 2, dist_b_mar = 250, padrao = "medio"),
#'              factor = -1)
elasticidade <- function(object, variable, func = "log", local, factor = 0.1){
  z <- object
  df <- eval(stats::getCall(z)$data)

  p <- predict(z, newdata = local)
  if (is.factor(dplyr::pull(df[, variable]))) {
    levels <- levels(dplyr::pull(df[, variable]))
    current_level <- which(levels == local[[variable]])
    next_level <- current_level + factor
    local[[variable]] <- levels[next_level]
  } else {
    local[[variable]] <- (1 + factor)*local[[variable]]
  }
  p1 <- predict(z, newdata = local)
  elasticidade <- 100*(inverse(p1, func) - inverse(p, func))/inverse(p, func)
  elasticidade
}

