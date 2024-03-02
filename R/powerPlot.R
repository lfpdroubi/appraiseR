
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
#' @param Smooth option to add a regression line to the plot
#' @param se option to add confidence interval of regression line to the plot
#' @param metrics TRUE or FALSE. If TRUE, metrics (RMSE, MAE and MAPE) are
#' displayed at the plot area.
#' @param R2 TRUE or FALSE. If TRUE, R2 between predicted and observed values
#' are printed.
#' @examples
#' library(sf)
#' library(broom)
#' library(ggplot2)
#' centro_2015$padrao <- as.numeric(centro_2015$padrao)
#' fit <- lm(log(valor) ~ area_total + quartos + suites + garagens +
#'           log(dist_b_mar) + I(1/padrao),
#'           data = centro_2015, subset = -c(31, 39))
#' s <- summary(fit)
#' centro_2015 <- expand.model.frame(fit, ~ valor + dist_b_mar + padrao,
#'                                   na.expand = T)
#' # Transformed Scale:
#' y <- centro_2015$`log(valor)`
#' powerPlot(y = y, yhat = fitted(fit),
#'           system = "ggplot2", Smooth = TRUE)
#' powerPlot(y = y, yhat = fitted(fit),
#'           system = "ggplot2", Smooth = TRUE, axis = "inverted")
#' # Original Scale:
#' ## Mediana
#' Y <- centro_2015$valor
#' YHAT <-  exp(fitted(fit))
#' p <- powerPlot(y = Y,
#'                yhat = YHAT,
#'                system = 'ggplot2',
#'                axis = "inverted",
#'                Smooth = TRUE)
#' p + labs(title = "Poder de Predição", subtitle = "Mediana")
#' ## Média
#' YHAT1 <- exp(fitted(fit) + s$sigma^2/2)
#' p1 <- powerPlot(y = Y,
#'                 yhat = YHAT1,
#'                 system = 'ggplot2',
#'                 axis = "inverted",
#'                 Smooth = TRUE)
#' p1 + labs(title = "Poder de Predição", subtitle = "Média")
#' ## Moda
#' YHAT2 <- exp(fitted(fit) - s$sigma^2)
#' p2 <- powerPlot(y = Y,
#'                 yhat = YHAT2,
#'                 system = 'ggplot2',
#'                 axis = "inverted",
#'                 Smooth = TRUE)
#' p2 + labs(title = "Poder de Predição", subtitle = "Moda")
#' @export
powerPlot.default <- function(y, yhat, system = c("base", "ggplot2", "lattice"),
                              axis = c("standard", "inverted"),
                              Smooth = FALSE, methods = "lm", se = FALSE,
                              metrics = c("none", "rmse", "mae", "mape"),
                              R2 = FALSE, ...){
  axis <- match.arg(axis)
  system <- match.arg(system)
  metrics <- match.arg(metrics, several.ok = TRUE)

  col <- RColorBrewer::brewer.pal(n = 8, "Set1")

  if (system == "ggplot2") {
    invres <- data.frame(y = y, yhat = yhat)
    if (axis == "inverted") {
      p <- ggplot(invres, aes(x = yhat, y = y)) +
        geom_point(colour = col[5], alpha=0.5) +
        xlab(bquote(~hat(Y))) +
        ylab("Y") +
        geom_abline(color = col[1])
    } else {
      p <- ggplot(invres, aes(x = y, y = yhat)) +
        geom_point(colour = col[5], alpha=0.5) +
        ylab(bquote(~hat(Y))) +
        xlab("Y") +
        geom_abline(color = col[1])
    }
    if (Smooth == TRUE) {
      i <- 0
      for (method in methods) {
        i <- i + 2
        p <- p + stat_smooth(method = method, se = se, color = col[i])
      }
    }

    if (R2 == TRUE){
      RSQ <- substitute(R^2~"="~r2,
                        list(r2 = brf(cor(y, yhat)^2, nsmall = 2)))
      p <- p +
        ggpp::geom_label_npc(aes(npcx = "center", npcy = "top",
                                 label = as.character(as.expression(RSQ))),
                             parse = TRUE,
                             color = "darkblue")
    }

    if ("none" %in% metrics) {
      return(p)
    } else {
      npcx <- c(rmse = "left", mae = "right", mape = "center")
      npcy <- c(rmse = "top", mae = "bottom", mape = "bottom")
      col <- c(rmse = "blue", mae = "darkgreen", mape = "red")
      require(Metrics)
      a <- vector(mode = "character", length = 0)
      for (metric in metrics) {
        a[[metric]] <- paste(metric, " = ",
                             brf(do.call(metric, args = list(y, yhat)),
                                 nsmall = 0))
      }
      DF <- data.frame(x.char = npcx[metrics], y.char =  npcy[metrics],
                       text = a[metrics], color = col[metrics])
      p <- p +
        ggpp::geom_label_npc(data = DF,
                             aes(npcx = x.char, npcy = y.char, label = text),
                             color = col[metrics]
        )
    }
    return(p)
  } else if (system == "lattice") {
    f <- as.formula(paste("y ~ yhat"))
    xyplot(f,
           panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
    }) +
    latticeExtra::layer(panel.smoother(y ~ x, se = FALSE, span = 0.5))
  } else {
    if (axis == "standard") {
      car::scatterplot(x = y, y = yhat,
                       col = "grey",
                       boxplots = "",
                       id = T,
                       pch = 20,
                       cex = .75,
                       las = 1,
                       regLine = list(method=lm, lty=1, lwd=2, col="green"),
                       smooth=list(smoother = loessLine, style = "none",
                                   lty.smooth = 1, col.smooth = "blue"),
                       xlab = "Y",
                       ylab = bquote(~hat(Y)),
                       ...
      )
      abline(a = 0, b = 1, col = "red")
    } else {
      car::scatterplot(x = yhat, y = y,
                       col = col[5],
                       boxplots = "",
                       id = T,
                       pch = 20,
                       cex = .75,
                       las = 1,
                       pty = "s",
                       regLine = list(method=lm, lty=1, lwd=2, col=col[2]),
                       smooth=list(smoother = car::loessLine(), style = "none",
                                   lty.smooth = 1, col.smooth = col[4]),
                       xlab = bquote(~hat(Y)),
                       ylab = "Y",
                       ...
      )
      abline(a = 0, b = 1, col = col[1])
    }

  }

}

#' @rdname powerPlot
#' @param object object of class \code{\link{lm}}, \code{\link{rq}},
#' \code{\link{lmerMod}} or \code{\link{bestfit}}.
#' @param FUN function used to transform the dependent variable (the inverse
#' of this function will be used to retransform the data to the original scale).
#' @param \dots further args passed to powerPlot.default
#' @examples
#' # powerPlot.lm:
#' par(pty="s") #default par(pty="m")
#' powerPlot(fit)
#' powerPlot(fit, main = "Poder de Predição")
#' powerPlot(fit, FUN = "log",
#'           main = "Poder de Predição")
#' title(sub = "Em reais", adj = 1)
#' powerPlot(fit, axis = "inverted")
#' p <- powerPlot(fit, system = "ggplot2",
#'                FUN = "log", axis = "inverted")
#' p +
#'   labs(title = "Poder de Predição", subtitle = "Em Reais") +
#'   theme(aspect.ratio = 1)
#' # Changes Dep Var Transformation
#' fit1 <- lm(sqrt(valor) ~ area_total + quartos + suites + garagens +
#'           log(dist_b_mar) + I(1/padrao), centro_2015)
#' powerPlot(fit1)
#' powerPlot(fit1, FUN = "sqrt", axis = "inverted")
#' @export
#'
powerPlot.lm <- function(object, FUN, system = c("base", "ggplot2", "lattice"),
                         ...) {
  z <- object
  data <- stats::model.frame(z)
  system <- match.arg(system)

  params <- parameters(z)
  depvar <- params$response
  lhs <- params$lhs

  DF <- expand.model.frame(model = z, extras = depvar, na.expand = TRUE)
  DF$`.fitted` <- fitted(z)
  #DF <- broom::augment(z, data = DF)

  if (!missing(FUN)) {
    DF[, ".Fitted"] <- inverse(DF[, ".fitted"], FUN)
    y <- DF[, depvar]
    yhat <- DF[, ".Fitted"]
  } else {
    y <- DF[, lhs]
    yhat <- DF[, ".fitted"]
  }

  if (system == "ggplot2") {
    if (!missing(FUN)) {
      p <- powerPlot(y, yhat, system = system, ...)
      p <- p +
        scale_y_continuous(labels = scales::label_number_auto()) +
        scale_x_continuous(labels = scales::label_number_auto())
      return(p)
    } else {
      p <- powerPlot(y, yhat, system = system, ...)
      return(p)
    }
  } else if (system == "base") {
    powerPlot(y, yhat, system = system, ...)
    #abline(a = 0, b = 1, col = "red")
  }
}

#' @rdname powerPlot
#' @param fit chosen fit
#' @param scale Plot values in original or transformed scale?
#' @examples
#' # powerPlot.bestfit:
#' library(sf)
#' dados <- st_drop_geometry(centro_2015)
#' dados$padrao <- as.numeric(dados$padrao)
#' best_fit <- bestfit(valor ~ ., dados,
#'                     transf = c("rec", "rsqrt", "log", "sqrt"),
#'                     subset = -c(31, 39))
#' p1 <- powerPlot(best_fit, system ="ggplot2")
#' p2 <- powerPlot(best_fit, system ="ggplot2", scale = "original")
#' cowplot::plot_grid(p1, p2, nrow = 1, ncol = 2)
#' p1 <- powerPlot(best_fit, system = "ggplot2",
#'                 axis = "inverted")
#' p2 <- powerPlot(best_fit, system = "ggplot2",
#'                 axis = "inverted", scale = "original")
#' cowplot::plot_grid(p1, p2, nrow = 1, ncol = 2)
#' powerPlot(best_fit, fit = 514)
#' p1 <- powerPlot(best_fit, fit = 514, system = "ggplot2",
#'                 axis = "inverted")
#' p2 <- powerPlot(best_fit, fit = 514, system = "ggplot2",
#'                 scale = "original", axis = "inverted")
#' cowplot::plot_grid(p1, p2, nrow = 1, ncol = 2)
#'
#' @export

powerPlot.bestfit <- function(object, fit = 1,
                              scale = c("transformed", "original"), ...) {
  scale <- match.arg(scale)
  s <- summary(object, fit = fit)
  z <- s$fit
  response <- object$response
  FUN <- as.character(object$tab[fit, response])
  if (scale == "original"){
    p <- powerPlot.lm(z, FUN = FUN, ...)
  } else {
    p <- powerPlot.lm(z, ...)
  }
  p
}

#' @rdname powerPlot
#' @examples
#' # powerPlot.lmerMod:
#' library(lme4)
#' Mfit <- lmer(log(valor) ~ area_total + quartos + suites + garagens +
#' dist_b_mar + (1|padrao), dados)
#' powerPlot(Mfit)
#' powerPlot(Mfit, FUN = "log")
#' @export
#'
powerPlot.lmerMod <-  function(object, FUN, ...){
  require(broom.mixed)
  z <- object

  Y <- z@resp$y
  Y_ajustado <- z@resp$mu

  if (!missing(FUN)) {
    Y <- inverse(Y, FUN)
    Y_ajustado <- inverse(Y_ajustado, FUN)
  }

  p <- powerPlot(Y, Y_ajustado, ...)

  if (!missing(FUN)) {
    p <- p +
      scale_y_continuous(labels = scales::label_number(accuracy = .01,
                                                       big.mark = ".",
                                                       decimal.mark = ",")) +
      scale_x_continuous(labels = scales::label_number(accuracy = .01,
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
powerPlot.rq <-  function(object, FUN, ...){
  z <- object
  data <- stats::model.frame(z)

  Y <- data[, attr(z$terms, "response")]
  Y_ajustado <- z$fitted.values

  if (!missing(FUN)) {
    Y <- inverse(Y, FUN)
    Y_ajustado <- inverse(Y_ajustado, FUN)
  }

  p <- powerPlot(y = Y, yhat = Y_ajustado, ...)

  if (!missing(FUN)) {
    p <- p +
      scale_y_continuous(labels = scales::label_number(accuracy = .01,
                                                       big.mark = ".",
                                                       decimal.mark = ",")) +
      scale_x_continuous(labels = scales::label_number(accuracy = .01,
                                                       big.mark = ".",
                                                       decimal.mark = ","))
  }
  return(p)
}
