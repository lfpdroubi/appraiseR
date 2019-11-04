
#' Power plots with ggplot2
#'
#' \code{power_plot} generates Power Plots for \code{\link{lm}} or
#' \code{\link{bestfit}} objects with \code{\link{ggplot2}}.
#'
#' @param object object of class \code{\link{lm}} or \code{\link{bestfit}}
#' @param \dots not used.
#' @return a power plot
#' @name power_plot
#' @export
power_plot <- function(object, ...) {
  UseMethod("power_plot")
}

#' @rdname power_plot
#' @examples
#' dados <- centro_2015@data
#' fit <- lm(log(valor) ~ ., dados)
#' power_plot(fit)
#' @export
#'
power_plot.lm <- function(object, ...) {
  z <- object
  attr(z$terms, "variables")
  data <- stats::model.frame(z)


  Y <- data[, attr(z$terms, "response")]
  Y_ajustado <- z$fitted.values
  invres <- data.frame(Y, Y_ajustado)
  p <- ggplot(data = invres, aes(x = Y_ajustado, y = Y)) +
    geom_point(alpha=0.5) +
    xlab(bquote(~hat(Y))) +
    geom_abline(color="red") +
    geom_smooth(method = "lm", se=FALSE) +
    coord_fixed()
  p
}

#' @rdname power_plot
#' @param fit chosen fit
#' @examples
#' dados <- centro_2015@data
#' best_fit <- bestfit(valor ~ ., dados)
#' power_plot(best_fit, fit = 257)
#' @export

power_plot.bestfit <- function(object, fit = 1, ...) {
  s <- summary(object, fit = fit)
  z <- s$fit
  p <- power_plot.lm(z)
  p
}