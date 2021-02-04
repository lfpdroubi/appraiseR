
#' Power plots with ggplot2
#'
#' \code{powerPlot} generates Power Plots for \code{\link{lm}} or
#' \code{\link{bestfit}} objects with \code{\link{ggplot2}}.
#' @param y observed values
#' @param yhat predicted values
#' @return a power plot
#' @name powerPlot
#' @export
powerPlot <- function(y, yhat, ...) {
  UseMethod("powerPlot")
}

#' @name powerPlot
#' @param axis option to plot predicted values on the x axis (inverted) or in
#' the y axis (standard)
#' @param smooth option to add a regression line to the plot
#' @param se option to add confidence interval of regression line to the plot
#' @param metrics TRUE or FALSE. If TRUE, metrics (R2, RMSE, MAE and MAPE) are
#' displayed at the plot area.
#' @examples
#' library(sf)
#' dados <- st_drop_geometry(centro_2015)
#' dados$padrao <- as.numeric(dados$padrao)
#' fit <- lm(log(valor) ~ area_total + quartos + suites + garagens +
#' log(dist_b_mar) + I(1/padrao), dados, subset = -c(31, 39))
#' s <- summary(fit)
#' # Mediana
#' library(ggplot2)
#' p <- powerPlot(y = na.omit(dados$valor)[-c(31, 39)], yhat = exp(fitted(fit)),
#' axis = "inverted")
#' p + labs(title = "Poder de Predição", subtitle = "Mediana")
#' # Média
#' p1 <- powerPlot(y = na.omit(dados$valor)[-c(31, 39)],
#'                 yhat = exp(fitted(fit) + s$sigma^2/2),  axis = "inverted")
#' p1 + labs(title = "Poder de Predição", subtitle = "Média")
#' # Moda
#' p2 <- powerPlot(y = na.omit(dados$valor)[-c(31, 39)],
#'                 yhat = exp(fitted(fit) - s$sigma^2), axis = "inverted")
#' p2 + labs(title = "Poder de Predição", subtitle = "Moda")
#' fit <- lm(I(valor^(1/3)) ~ area_total + quartos + suites + garagens +
#' log(dist_b_mar) + I(1/padrao), dados, subset = -c(31, 39, 42))
#' s <- summary(fit)
#' powerPlot(y = na.omit(dados$valor)[-c(31, 39, 42)],
#'           yhat = fitted(fit)^3 + 3*fitted(fit)*s$sigma^2
#' )
#' @export
powerPlot.default <- function(y, yhat, axis = c("standard", "inverted"),
                              smooth = TRUE, se = FALSE, metrics = TRUE, ...){
  axis <- match.arg(axis)
  invres <- data.frame(y = y, yhat = yhat)

  if (axis == "inverted") {
    p <- ggplot(invres, aes(x = yhat, y = y)) +
      geom_point(alpha=0.5) +
      xlab(bquote(~hat(Y))) +
      ylab("Y") +
      geom_abline(color = "red")
  } else {
    p <- ggplot(invres, aes(x = y, y = yhat)) +
      geom_point(alpha=0.5) +
      ylab(bquote(~hat(Y))) +
      xlab("Y") +
      geom_abline(color = "red")
  }

  if (smooth == TRUE) {
    p <- p + stat_smooth(method = "lm", se = se)
  }

  if (metrics == TRUE) {
    RMSE <- paste("RMSE = ", brf(Metrics::rmse(y, yhat), nsmall = 0))
    MAE <- paste("MAE = ", brf(Metrics::mae(y, yhat), nsmall = 0))
    MAPE <- paste("MAPE =", pct(Metrics::mape(y, yhat), digits = 2))
    RSQ <- substitute(R^2~"="~r2,
                      list(r2 = brf(cor(y, yhat)^2, nsmall = 2)))

    p <- p +
      ggpmisc::geom_label_npc(aes(npcx = "left", npcy = "top", label = RMSE),
                              color = "blue") +
      ggpmisc::geom_label_npc(aes(npcx = "right", npcy = "bottom", label = MAE),
                              color = "darkgreen") +
      ggpmisc::geom_label_npc(aes(npcx = "center", npcy = "bottom", label = MAPE),
                              color = "red") +
      ggpmisc::geom_label_npc(aes(npcx = "center", npcy = "top",
                                  label = as.character(as.expression(RSQ))),
                              parse = TRUE,
                              color = "darkblue")
  }

  return(p)

}

#' @rdname powerPlot
#' @param object object of class \code{\link{lm}}, \code{\link{rq}},
#' \code{\link{lmerMod}} or \code{\link{bestfit}}.
#' @param func function used to transform the dependent variable (the inverse
#' of this function will be used to retransform the data to the original scale).
#' @param \dots further args passed to powerPlot.default
#' @examples
#' powerPlot(fit)
#' powerPlot(fit, se = TRUE)
#' powerPlot(fit, smooth = FALSE)
#' powerPlot(fit, axis = "inverted")
#' p <- powerPlot(fit, func = "log", axis = "inverted")
#' p + labs(title = "Poder de Predição", subtitle = "Em milhões de Reais")
#' # Changes Dep Var Transformation
#' fit <- lm(sqrt(valor) ~ area_total + quartos + suites + garagens +
#' log(dist_b_mar) + I(1/padrao), dados)
#' powerPlot(fit)
#' powerPlot(fit, func = "sqrt", axis = "inverted")
#' @export
#'
powerPlot.lm <- function(object, func, ...) {
  z <- object
  data <- stats::model.frame(z)

  Y <- data[, attr(z$terms, "response")]
  Y_ajustado <- z$fitted.values

  if (!missing(func)) {
    Y <- inverse(Y, func)
    Y_ajustado <- inverse(Y_ajustado, func)
  }

  p <- powerPlot(Y, Y_ajustado, ...)

  if (!missing(func)) {
    p <- p +
      scale_y_continuous(labels = scales::label_number_si(accuracy = .01,
                                                          big.mark = ".",
                                                          decimal.mark = ",")) +
      scale_x_continuous(labels = scales::label_number_si(accuracy = .01,
                                                          big.mark = ".",
                                                          decimal.mark = ","))
  }

  return(p)
}

#' @rdname powerPlot
#' @param fit chosen fit
#' @param scale Plot values in original or transformed scale?
#' @examples
#' dados <- st_drop_geometry(centro_2015)
#' dados <- within(dados, padrao <- as.numeric(padrao))
#' best_fit <- bestfit(valor ~ ., dados,
#'                     transf = c("rec", "rsqrt", "log", "sqrt"),
#'                     subset = -c(31, 39))
#' p1 <- powerPlot(best_fit)
#' p2 <- powerPlot(best_fit, scale = "original")
#' cowplot::plot_grid(p1, p2, nrow = 1, ncol = 2)
#' p1 <- powerPlot(best_fit, axis = "inverted")
#' p2 <- powerPlot(best_fit, axis = "inverted", scale = "original")
#' cowplot::plot_grid(p1, p2, nrow = 1, ncol = 2)
#' powerPlot(best_fit, fit = 514)
#' p1 <- powerPlot(best_fit, fit = 514, axis = "inverted")
#' p2 <- powerPlot(best_fit, fit = 514, scale = "original", axis = "inverted")
#' cowplot::plot_grid(p1, p2, nrow = 1, ncol = 2)
#'
#' @export

powerPlot.bestfit <- function(object, fit = 1,
                              scale = c("transformed", "original"), ...) {
  scale <- match.arg(scale)
  s <- summary(object, fit = fit)
  z <- s$fit
  response <- object$response
  func <- as.character(object$tab[fit, response])
  if (scale == "original"){
    p <- powerPlot.lm(z, func = func, ...)
  } else {
    p <- powerPlot.lm(z, ...)
  }
  p
}

#' @rdname powerPlot
#' @examples
#' library(lme4)
#' Mfit <- lmer(log(valor) ~ area_total + quartos + suites + garagens +
#' dist_b_mar + (1|padrao), dados)
#' powerPlot(Mfit)
#' powerPlot(Mfit, func = "log")
#' @export
#'
powerPlot.lmerMod <-  function(object, func, ...){
  require(broom.mixed)
  z <- object

  Y <- z@resp$y
  Y_ajustado <- z@resp$mu

  if (!missing(func)) {
    Y <- inverse(Y, func)
    Y_ajustado <- inverse(Y_ajustado, func)
  }

  p <- powerPlot(Y, Y_ajustado, ...)

  if (!missing(func)) {
    p <- p +
      scale_y_continuous(labels = scales::label_number_si(accuracy = .01,
                                                          big.mark = ".",
                                                          decimal.mark = ",")) +
      scale_x_continuous(labels = scales::label_number_si(accuracy = .01,
                                                          big.mark = ".",
                                                          decimal.mark = ","))
  }
  return(p)
}

#' @rdname powerPlot
#' @examples
#' library(quantreg)
#' set.seed(1)
#' dados <- data.frame(
#' Area = runif(100, min = 360, max = 600)
#' )
#' dados$LVU <- 12 - .0075*dados$Area + rnorm(100, mean = 0, sd = .25)
#' dados$VU <- exp(dados$LVU)
#' medFit <- rq(VU~Area, data = dados)
#' powerPlot(medFit)
#' powerPlot(medFit, axis = "inverted")
#' @export
#'
powerPlot.rq <-  function(object, func, ...){
  z <- object
  data <- stats::model.frame(z)

  Y <- data[, attr(z$terms, "response")]
  Y_ajustado <- z$fitted.values

  if (!missing(func)) {
    Y <- inverse(Y, func)
    Y_ajustado <- inverse(Y_ajustado, func)
  }

  p <- powerPlot(y = Y, yhat = Y_ajustado, ...)

  if (!missing(func)) {
    p <- p +
      scale_y_continuous(labels = scales::label_number_si(accuracy = .01,
                                                          big.mark = ".",
                                                          decimal.mark = ",")) +
      scale_x_continuous(labels = scales::label_number_si(accuracy = .01,
                                                          big.mark = ".",
                                                          decimal.mark = ","))
  }
  return(p)
}
