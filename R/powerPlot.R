
#' Power plots with ggplot2
#'
#' \code{powerPlot} generates Power Plots for \code{\link{lm}} or
#' \code{\link{bestfit}} objects with \code{\link{ggplot2}}.
#'
#' @param object object of class \code{\link{lm}} or \code{\link{bestfit}}
#' @param \dots not used.
#' @return a power plot
#' @name powerPlot
#' @export
powerPlot <- function(object, ...) {
  UseMethod("powerPlot")
}

#' @rdname powerPlot
#' @examples
#' library(ggplot2)
#' dados <- st_drop_geometry(centro_2015)
#' dados$padrao <- as.numeric(dados$padrao)
#' fit <- lm(log(valor) ~ area_total + quartos + suites + garagens +
#' log(dist_b_mar) + I(1/padrao), dados, subset = -c(31, 39))
#' powerPlot(fit)
#' powerPlot(fit, axis = "inverted")
#' p <- powerPlot(fit, func = "log", axis = "inverted")
#' p + labs(title = "Poder de Predição", subtitle = "Em milhões de Reais")
#' @export
#'
powerPlot.lm <- function(object, func, axis = c("standard", "inverted"), ...) {
  z <- object
  attr(z$terms, "variables")
  data <- stats::model.frame(z)
  axis <- match.arg(axis)

  if (missing(func)) {
    Y <- data[, attr(z$terms, "response")]
    Y_ajustado <- z$fitted.values
    invres <- data.frame(Y, Y_ajustado)
  } else {
    Y <- data[, attr(z$terms, "response")]
    Y <- inverse(Y, func)
    Y_ajustado <- z$fitted.values
    Y_ajustado <- inverse(Y_ajustado, func)
    invres <- data.frame(Y, Y_ajustado)
  }

  p <- ggplot(data = invres, aes(x = Y, y = Y_ajustado)) +
    geom_point(alpha=0.5) +
    ylab(bquote(~hat(Y))) +
    xlab("Y") +
    geom_abline(color="red") +
    geom_smooth(method = "lm", se=FALSE) +
    coord_fixed()

  if (axis == "inverted"){
    p <- p + coord_flip() + theme(aspect.ratio = 1)
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

}

#' @rdname powerPlot
#' @param fit chosen fit
#' @examples
#' dados <- st_drop_geometry(centro_2015)
#' best_fit <- bestfit(valor ~ ., dados)
#' powerPlot(best_fit, fit = 257)
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
