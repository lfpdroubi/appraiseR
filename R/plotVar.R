#' Plot model predictors against the response variable with intervals
#'
#' \code{plotvar} plots model variables with confidence/prediction intervals
#' @param object object of class lm
#' @param variable variable to be plotted against response variable
#' @param func function used to transform the response (optional)
#' @param interval the type of interval calculation (provided to predict.lm) to
#'   be ploted.
#' @param level Tolerance/confidence level (provided to predict.lm) to
#'   be ploted.
#' @param ca (T/F) should the limits of the invertal of arbitration be plotted?
#' @param av (T/F) should the arbitrated value be plotted?
#' @param at list to be used for calculate the estimates
#' (defaults for center of each variable).
#' @param \dots further arguments passed to predict.lm.
#' @export
#' @examples
#' library(sf)
#' data(centro_2015)
#' centro_2015 <- within(centro_2015, VU <- valor/area_total)
#' fit <- lm(log(VU) ~ log(area_total) + quartos + suites + garagens +
#'             log(dist_b_mar) + padrao,
#'           data = centro_2015)
#' plotVar(fit, "area_total")
#' plotVar(fit, "area_total", residuals = TRUE)
#' plotVar(fit, "area_total", interval = "both", residuals = TRUE)
#' plotVar(fit, "dist_b_mar", interval = "both", residuals = TRUE)
#' plotVar(fit, "area_total", interval = "both", residuals = TRUE,
#'         at = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'         dist_b_mar = 250, padrao = "medio"))
#' plotVar(fit, "area_total", func = "log")
#' plotVar(fit, "suites")
#' plotVar(fit, "suites", func = "log")
#' plotVar(fit, "area_total", interval = "confidence", ca = TRUE)
#' plotVar(fit, "area_total", interval = "confidence", func = "log", ca = TRUE)
#' plotVar(fit, "area_total", interval = "prediction")
#' plotVar(fit, "area_total", interval = "both")
#' # Both intervals + CA
#' plotVar(fit, "area_total", interval = "both", ca = TRUE,
#'         at = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'         dist_b_mar = 250, padrao = "medio"))
#' # Same above + Point valuation at R$ 5.650,00 /m2
#' plotVar(fit, "area_total", interval = "both",
#'         at = data.frame(area_total = 205, quartos = 3, suites = 1,
#'                         garagens = 2, dist_b_mar = 250, padrao = "medio"),
#'         ca = TRUE, av = log(5650))
#' # Same above, in the original scale
#' plotVar(fit, "area_total", interval = "both", func = 'log',
#'         at = data.frame(area_total = 205, quartos = 3, suites = 1,
#'                         garagens = 2, dist_b_mar = 250, padrao = "medio"),
#'         ca = TRUE, av = 5650)
#' plotVar(fit, "padrao")
#' plotVar(fit, "padrao", ca = TRUE)
#' plotVar(fit, "padrao", func = "log", ca = TRUE)
#' plotVar(fit, "padrao", interval = "confidence")
#' plotVar(fit, "padrao", interval = "prediction")
#' plotVar(fit, "padrao", interval = "both")
#' plotVar(fit, "padrao", func = "log", interval = "confidence",
#'         at = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'                   dist_b_mar = 250, padrao = "medio"))
#' plotVar(fit, "padrao", func = "log", interval = "prediction",
#'         at = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'                   dist_b_mar = 250, padrao = "medio"),
#'         av = 5650)
#' # Remove outliers
#' fit2 <- update(fit, .~.-suites, subset = -c(31,39, 45))
#' plotVar(fit2, "padrao", func = "log", interval = "prediction",
#'         at = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'                   dist_b_mar = 250, padrao = "medio"),
#'         av = 5650)
#' plotVar(fit2, "area_total", func = "log", interval = "both",
#'         at = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'                   dist_b_mar = 250, padrao = "medio"),
#'         ca = TRUE, av = 5650)
#' plotVar(fit2, "dist_b_mar", func = "log", interval = "both",
#'         at = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'                   dist_b_mar = 250, padrao = "medio"),
#'         ca = TRUE, av = 5650)

plotVar <- function(object, variable, func,
                    interval = c("none", "confidence", "prediction", "both"),
                    level = 0.80,
                    ca = FALSE,
                    av,
                    at, ...){
  interval <- match.arg(interval)
  DF <- eval(stats::getCall(object)$data)
  vars <- all.vars(stats::formula(object))
  params <- parameters(object)
  response <- params$response
  preds <- params$predictors

  DF %<>% dplyr::as_tibble() %>% dplyr::mutate_if(is.character, as.factor)

  variavel <- DF[, variable, drop = FALSE]

  if (is.factor(variavel[, variable, drop = T])){
    plotFactor(variable, object,
               interval = interval, level = level,
               func = func, ca = ca, av = av, at = at, ...)
  } else {
    plotContinuousVariable(variable, object,
                           interval = interval, level = level,
                           func = func, ca = ca, av = av, at = at, ...)
  }
}
#' export
plotFactor <- function(x, object,
                       interval =  c("none", "confidence", "prediction", "both"),
                       level = 0.80,
                       at,
                       func,
                       ca = FALSE,
                       av,
                       elasticidade = TRUE,
                       ...){
  interval <- match.arg(interval)
  variable <- x
  params <- parameters(object)
  response <- params$response
  preds <- params$predictors
  coeffs <- coef(object)

  DF <- as.data.frame(eval(stats::getCall(object)$data))
  variavel <- DF[, variable, drop = FALSE]
  grid <- unique(variavel)

  if (missing(at)) {
    new <- data.frame(grid, lapply(DF[setdiff(preds, variable)], centre))
    p_local <- NULL
  } else {
    new <- data.frame(grid, at[setdiff(preds, variable)])
    p_local <- predict(object, newdata = at)
  }
  if(interval != "both") {
    Y <- stats::predict.lm(object = object,
                           newdata = remove_missing_levels(object, new),
                           interval = interval, level = level, ...)
  } else {
    message("Plotagem de ambos os intervalos disponivel apenas para variaveis
              continuas. Adotado intervalo de predicao")
    Y <- stats::predict.lm(object = object,
                           newdata = remove_missing_levels(object, new),
                           interval = "prediction", level = level, ...)
  }
  if (!missing(func)) {
    Y <- inverse(Y, func)
    Y <- cbind(Y, campo_arbitrio(Y))
  }

  pred <- data.frame(new[ , variable, drop = FALSE], Y)
  pred_plot <- reshape2::melt(pred,
                              id.var = variable,
                              value.name = response,
                              variable.name = "var")

  p <- ggplot(data = pred_plot, aes(x = reorder(!!as.name(variable),
                                                !!as.name(response),
                                                median),
                                    y = !!as.name(response))) +
    geom_boxplot(aes(fill = .data[[variable]])) +
    xlab(variable) +
    theme(legend.position="none") +
    scale_y_continuous(labels = scales::label_number_auto())

  if(!missing(at)) {
    p_local <- ifelse(missing(func), p_local, inverse(p_local, func))
    if (elasticidade == TRUE) {
      cat(elasticidade(object, variable, func, at, factor = +1), " ")
    }
    if (!missing(av)) {
      p_local <- data.frame(y = p_local, at, av = av)
    } else {
      p_local <- data.frame(y = p_local, at)
    }
    names(p_local)[1] <- response
    p <- p + geom_point(data = p_local,
                        aes(x = .data[[variable]],
                            y = .data[[response]]),
                        color = "red",
                        size = 3)
  }
  if (ca == TRUE & missing(func)) {
    message("Campo de Arbítrio somente possivel na escala original.")
  }
  if (!missing(av) & missing(at)){
    message("Valor arbitrado somente pode ser plotado caso seja informado local.")
  } else if (!missing(av)) {
    p <- p + geom_point(data = p_local,
                        aes(x = .data[[variable]], y = .data$av),
                        color = "purple",
                        size = 3)
  }
  return(p)
}
#'
#' @export
plotContinuousVariable <-
  function(x, object,
           system = "ggplot2",
           interval =  c("none", "confidence", "prediction", "both"),
           level = 0.80,
           residuals = FALSE,
           func,
           at,
           ca = FALSE,
           av,
           elasticidade = TRUE,
           ...){

  system <- match.arg(system)
  interval <- match.arg(interval)
  DF <-  eval(stats::getCall(object)$data)
  variable <- x
  params <- parameters(object)
  response <- params$response
  coeffs <- coef(object)

  # Calling predictResponse()

  if (!missing(func)){
    if (!missing(at)){
      predResp <- predictResponse(variable, object,
                                  interval = interval, level = level,
                                  func = func, at = at,
                                  residuals = residuals, ...)
    } else {
      predResp <- predictResponse(variable, object,
                                  interval = interval, level = level,
                                  func = func, residuals = residuals, ...)
    }
  } else if (missing(at)) {
    predResp <- predictResponse(variable, object,
                                interval = interval, level = level,
                                residuals = residuals, ...)
  } else {
    predResp <- predictResponse(variable, object,
                                interval = interval, level = level,
                                at = at, residuals = residuals, ...)
  }

  pred <- predResp$pred
  grid <- predResp$grid
  p_local <- predResp$p_local
  mframe <- predResp$mframe
  pres <- predResp$pres

  if (system == 'ggplot2'){

  # Basic Plot
  # see https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html
  p <- ggplot(data = pred,
              aes(x = .data[[variable]], y = .data[[response]])
              ) +
    geom_line(linewidth = 1) +
    theme(legend.position="bottom") +
    scale_y_continuous(labels = scales::label_number_auto()) +
    scale_x_continuous(labels = scales::label_number_auto()) +
    theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1))


  # Adds CA, if TRUE
  if (ca == TRUE) {
    if (missing(func)) {
      message("Campo de Arbítrio somente possivel na escala original.")
    } else {
      p <- p +
        geom_line(aes(y = .data$C.A.I.), linetype = 2) +
        geom_line(aes(y = .data$C.A.S.), linetype = 2)
    }
  }

  # Adds chosen intervals ribbons, if any
  if (interval == "both") {
    p <- p +
      geom_ribbon(aes(ymin = .data$lwr.y, ymax = .data$upr.y,
                       colour = "grey", alpha = 0.25),
                  stat = "identity") +
      geom_ribbon(aes(ymin = .data$lwr.x, ymax = .data$upr.x,
                      colour = "grey", alpha = .25),
                  stat = "identity") +
      theme(legend.position="none")
  } else if (interval != "none"){
    p <- p + geom_ribbon(aes(ymin = .data$lwr, ymax = .data$upr,
                             colour = "grey", alpha = 0.5),
                         stat = "identity") +
      theme(legend.position="none")
  }

  # Adds partial residuals, if residuals = TRUE

  if (residuals == TRUE & missing(func) & missing(at)) {
    vars <- attr(terms(object), "variables")

    sel <- lapply(vars, all.vars)[-1]

    res <- lapply(all.vars(terms(object)), \(x) which(sapply(sel, \(y) x %in% y)))
    res <- setNames(res, all.vars(terms(object)))

    k <- predict(object = object,
                 newdata = lapply(DF, centre))

    partialResiduals <-
      data.frame(X = mframe[, variable, drop = T],
                 Y = pres[, res[[variable]]-1] + k
                 )
    p <- p +
      geom_point(data = partialResiduals,
                 aes(x = .data$X, y = .data$Y),
                 pch = 4)
  }

  # Adds point of 'at' argument, if not missing
  if (!missing(at)) {
    p_local <- ifelse(missing(func), p_local, inverse(p_local, func))
    if (elasticidade == TRUE) {
      cat(elasticidade(object, variable, func, at), " ")
    }
    if (!missing(av)) {
      p_local <- data.frame(y = p_local, at, av = av)
    } else {
      p_local <- data.frame(y = p_local, at)
    }
    names(p_local)[1] <- response
    p <- p + geom_point(data = p_local,
                        aes(x = .data[[variable]],
                            y = .data[[response]]),
                        color = "red",
                        size = 3)
  }

  # Adds arbitrated value, if not missing
  if (!missing(av) & missing(at)){
    message("Valor arbitrado somente pode ser plotado caso seja informado local.")
  } else if (!missing(av)) {
    p <- p + geom_point(data = p_local,
                        aes(x = .data[[variable]], y = .data$av),
                        color = "purple",
                        size = 3)
  }
  return(p)
  }
}
