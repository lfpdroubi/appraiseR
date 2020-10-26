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
#' data <- st_drop_geometry(centro_2015)
#' fit <- lm(log(valor) ~ ., data = data)
#' new <- data.frame(area_total = 205, quartos = 3, suites = 1,
#'              garagens = 2, dist_b_mar = 250, padrao = "medio")
#' (p <- predict(fit, newdata = new))
#' new1 <- data.frame(area_total = 1.1*205, quartos = 3, suites = 1,
#'              garagens = 2, dist_b_mar = 250, padrao = "medio")
#' (p1 <- predict(fit, newdata = new1))
#' (exp(p1) - exp(p))/exp(p)
#' elasticidade(fit, "area_total", func = "log",
#'              local = list(area_total = 205, quartos = 3, suites = 1,
#'              garagens = 2, dist_b_mar = 250, padrao = "medio"))
#' elasticidade(fit, "dist_b_mar", func = "log",
#'              local = list(area_total = 205, quartos = 3, suites = 1,
#'              garagens = 2, dist_b_mar = 250, padrao = "medio"))
#' elasticidade(fit, "padrao", func = "log",
#'              local = list(area_total = 205, quartos = 3, suites = 1,
#'              garagens = 2, dist_b_mar = 250, padrao = "medio"),
#'              factor = +1)
#' elasticidade(fit, "padrao", func = "log",
#'              local = list(area_total = 205, quartos = 3, suites = 1,
#'              garagens = 2, dist_b_mar = 250, padrao = "medio"),
#'              factor = -1)
elasticidade <- function(object, variable, func, local, factor = 0.1){
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
  if (!missing(func)) {
    elasticidade <- (inverse(p1, func) - inverse(p, func))/inverse(p, func)
  } else{
    elasticidade <- (p1 - p)/p
  }
  cat(pct(elasticidade))
}

