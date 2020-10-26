#' Predict method for bestfit linear models
#'
#' Predict values based on a \code{\link{bestfit}} object.
#'
#' @param object object of class \code{\link{bestfit}}
#' @param fit The number of the chosen model from the combinations matrix
#'   (defaults for the best R2 model)
#' @param newdata An optional data frame in which to look for variables with
#'   which to predict. Defaults for the eventually existent "aval" data in data
#'   frame used to build the \code{bestfit} model. If omitted, the fitted values
#'   are used.
#' @param interval Type of interval calculation. Can be abbreviated. Defaults to
#'   confidence interval
#' @param level Tolerance/confidence level. Defaults for the standardized value
#'   in NBR-14.653-2 (80\%)
#' @param \dots further arguments passed to predict.lm.
#' @export
#' @examples
#' dados <- st_drop_geometry(centro_2015)
#' best_fit <- bestfit(valor ~ .,  data = dados)
#' p <- predict(best_fit, interval = "confidence")
#' p
#'
predict.bestfit <- function(object, fit = 1, newdata = object$newdata,
                            interval = c("none", "confidence", "prediction"),
                            level = 0.80, ...){
  s <- summary(object = object, fit = fit)
  modelo <- s$fit
  if (missing(interval)) interval <- "none" else interval <- interval
  if (is.null(newdata)) {
    Y <- stats::predict.lm(object = modelo, interval = interval,
                           level = level, ...)
  } else {
    Y <- stats::predict.lm(object = modelo, newdata = newdata,
                           interval = interval, level = level, ...)
  }

  Y <- inverse(Y, as.character(s$bestfit[, object$response]))
  amp <- amplitude(Y)
  gp <- g_precisao(amp)
  ca <- campo_arbitrio(Y)
  avi <- aval_intervalar(Y)
  est <- list(predictions = Y,
              intervalo = interval,
              amplitude = amp,
              grau_precisao = gp,
              campo_arbitrio = ca,
              avaliacao_intervalar = avi)
  class(est) <- "predict.bestfit"
  est
}

#' @export
#'
print.predict.bestfit <- function(x, ...){

  if (is.null(dim(x$predictions))) {
    est <- data.frame(x$predictions, x$campo_arbitrio)
    colnames(est) <- c('fit', 'C.A.I.', 'C.A.S.')
  } else {
    est <- data.frame(x$predictions, x$amplitude, x$grau_precisao,
                      x$campo_arbitrio, x$avaliacao_intervalar)
    colnames(est) <- c('fit', 'lwr', 'upr', 'AMP', 'G.P.', 'C.A.I.',
                       'C.A.S.', 'L.I.', 'L.S.')
  }

  cat("Predictions:\n")
  print(est)
}