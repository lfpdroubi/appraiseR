
#' Power plots with ggplot2
#'
#' \code{powerPlot} generates Power Plots for \code{\link{lm}} or
#' \code{\link{bestfit}} objects with \code{\link{ggplot2}}.
#'
#' @param object object of class \code{\link{lm}} or \code{\link{bestfit}}
#' @param func function used to transform the dependent variable
#' @param axis option to plot predicted values on the x axis (inverted) or in
#' the y axis (standard)
#' @param smooth option to add a regression line to the plot
#' @param \dots not used.
#' @return a power plot
#' @name powerPlot
#' @export
powerPlot <- function(object, ...) {
  UseMethod("powerPlot")
}

#' @rdname powerPlot
#' @examples
#' library(sf)
#' dados <- st_drop_geometry(centro_2015)
#' dados$padrao <- as.numeric(dados$padrao)
#' fit <- lm(log(valor) ~ area_total + quartos + suites + garagens +
#' log(dist_b_mar) + I(1/padrao), dados, subset = -c(31, 39))
#' powerPlot(fit)
#' powerPlot(fit, se = TRUE)
#' powerPlot(fit, smooth = FALSE)
#' powerPlot(fit, axis = "inverted")
#' library(ggplot2)
#' p <- powerPlot(fit, func = "log", axis = "inverted")
#' p + labs(title = "Poder de Predição", subtitle = "Em milhões de Reais")
#' @export
#'
powerPlot.lm <- function(object, func, axis = c("standard", "inverted"),
                         smooth = TRUE, se = FALSE, ...) {
  z <- object
  attr(z$terms, "variables")
  data <- stats::model.frame(z)
  axis <- match.arg(axis)

  Y <- data[, attr(z$terms, "response")]
  Y_ajustado <- z$fitted.values

  if (!missing(func)) {
    Y <- inverse(Y, func)
    Y_ajustado <- inverse(Y_ajustado, func)
  }

  invres <- data.frame(Y, Y_ajustado)

  if (axis == "inverted") {
    p <- ggplot(data = invres, aes(x = Y_ajustado, y = Y)) +
      geom_point(alpha=0.5) +
      xlab(bquote(~hat(Y))) +
      ylab("Y") +
      geom_abline(color="red")
  } else {
    p <- ggplot(data = invres, aes(x = Y, y = Y_ajustado)) +
      geom_point(alpha=0.5) +
      ylab(bquote(~hat(Y))) +
      xlab("Y") +
      geom_abline(color="red")
  }

  if (!missing(func)) {
    p <- p +
      scale_y_continuous(labels = scales::label_number_si(accuracy = .01,
                                                          big.mark = ".",
                                                          decimal.mark = ",")) +
      scale_x_continuous(labels = scales::label_number_si(accuracy = .01,
                                                          big.mark = ".",
                                                          decimal.mark = ","))
  }

  if (smooth == TRUE) {
    p <- p + stat_smooth(method = "lm", se = se)
  }

  return(p)
}

#' @rdname powerPlot
#' @param fit chosen fit
#' @examples
#' dados <- st_drop_geometry(centro_2015)
#' best_fit <- bestfit(valor ~ ., dados)
#' powerPlot(best_fit, fit = 257)
#' powerPlot(best_fit, fit = 257, func = "log")
#' powerPlot(best_fit, fit = 257, func = "log", axis = "inverted")
#' @export

powerPlot.bestfit <- function(object, fit = 1, ...) {
  s <- summary(object, fit = fit)
  z <- s$fit
  p <- powerPlot.lm(z, ...)
  p
}

#' @rdname powerPlot
#' @param fit chosen fit
#' @examples
#' library(lme4)
#' data(centro_2015)
#' dados <- st_drop_geometry(centro_2015)
#' Mfit <- lmer(log(valor) ~ area_total + quartos + suites + garagens +
#' dist_b_mar + (1|padrao), dados)
#' powerPlot(Mfit)
#' powerPlot(Mfit, func = "log")
#' @export
#'
powerPlot.lmerMod <-  function(object, func, ...){
  require(broom.mixed)
  z <- object
  df <- data.frame(.fitted = z@resp$mu, Y = z@resp$y)

  if (missing(func)) {
    p <- ggplot(df, aes(x = .fitted, y = Y)) +
      geom_point(alpha=0.5) +
      xlab(bquote(~hat(Y))) +
      geom_abline(color = "red") +
      geom_smooth(method = "lm", se=FALSE) +
      coord_fixed()
    return(p)
  } else {
    df <- data.frame(.fitted = inverse(df$.fitted, func),
                     Y = inverse(df$Y, func))
    p <- ggplot(df, aes(x = .fitted, y = Y)) +
      geom_point(alpha=0.5) +
      xlab(bquote(~hat(Y))) +
      geom_abline(color = "red") +
      geom_smooth(method = "lm", se=FALSE) +
      coord_fixed()
  }

  if (missing(func)) {
    return(p)
  } else {
    p <- p +
      scale_y_continuous(labels = scales::label_number_si(accuracy = .01,
                                                          big.mark = ".",
                                                          decimal.mark = ",")) +
      scale_x_continuous(labels = scales::label_number_si(accuracy = .01,
                                                          big.mark = ".",
                                                          decimal.mark = ","))
    return(p)
  }
  return(p)
}
