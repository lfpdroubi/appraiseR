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
#' mod <- lm(sqrt(valor) ~ sqrt(area_total) + suites, data = centro_2015)
#' plotVar(mod, "area_total")
#' plotVar(mod, "area_total", func = "sqrt")
#' plotVar(mod, "suites")
#' plotVar(mod, "suites", func = "sqrt")
#' plotVar(mod, "area_total", interval = "confidence", ca = TRUE)
#' plotVar(mod, "area_total", interval = "confidence", func = "sqrt", ca = TRUE)
#' fit <- lm(log(valor) ~ area_total + quartos + suites + garagens + dist_b_mar
#'                        + padrao,
#'           data = centro_2015)
#' plotVar(fit, "area_total")
#' plotVar(fit, "area_total", interval = "confidence")
#' plotVar(fit, "area_total", interval = "prediction")
#' plotVar(fit, "area_total", interval = "both")
#' plotVar(fit, "area_total", "log")
#' plotVar(fit, "area_total", "log", ca = TRUE)
#' plotVar(fit, "area_total", "log", interval = "confidence")
#' plotVar(fit, "area_total", "log", interval = "prediction")
#' plotVar(fit, "area_total", "log", interval = "both")
#' plotVar(fit, "area_total", interval = "both", ca = TRUE,
#'         at = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'         dist_b_mar = 250, padrao = "medio"))
#' plotVar(fit, "area_total", func = "log", interval = "both", ca = TRUE,
#'         at = data.frame(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'         dist_b_mar = 250, padrao = "medio"), av = 1100000)
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
#'                         dist_b_mar = 250, padrao = "medio"),
#'         av = 1100000)
#' plotVar(fit, "area_total", func = "log", interval = "prediction",
#'         at = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'                   dist_b_mar = 250, padrao = "medio"),
#'         av = 1100000, ca = TRUE)
#' centro_2015$padrao <- relevel(centro_2015$padrao, ref="medio")
#' mod2 <- lm(log(valor) ~ area_total + quartos + suites + garagens + dist_b_mar
#'                        + padrao,
#'                        data = centro_2015)
#' plotVar(mod2, "padrao", interval = "confidence")
#' plotVar(mod2, "padrao", interval = "confidence", func = "log")

plotVar <- function(object, variable, func,
                    interval = c("none", "confidence", "prediction", "both"),
                    level = 0.80,
                    ca = FALSE,
                    av,
                    at, ...){
  interval <- match.arg(interval)
  z <- object
  df <- eval(stats::getCall(z)$data)
  vars <- all.vars(stats::formula(z))
  params <- parameters(z)
  response <- params$response
  preds <- params$predictors

  df %<>% dplyr::as_tibble() %>% dplyr::mutate_if(is.character, as.factor)

  variavel <- df[, variable, drop = FALSE]

  if (is.factor(variavel[, variable, drop = T])){
    plotFactor(variable, z, interval = interval, level = level, func = func,
               ca = ca, av = av, at = at, ...)
  } else {
    plotContinuousVariable(variable, z, interval = interval, level = level,
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
  z <- object
  params <- parameters(z)
  response <- params$response
  preds <- params$predictors

  df <- as.data.frame(eval(stats::getCall(z)$data))
  variavel <- df[, variable, drop = FALSE]
  grid <- unique(variavel)

  if (missing(at)) {
    new <- data.frame(grid, lapply(df[setdiff(preds, variable)], centre))
    p_local <- NULL
  } else {
    new <- data.frame(grid, at[setdiff(preds, variable)])
    p_local <- predict(z, newdata = at)
  }
  if(interval != "both") {
    Y <- stats::predict.lm(object = z, newdata = remove_missing_levels(z, new),
                           interval = interval, level = level, ...)
  } else {
    message("Plotagem de ambos os intervalos disponivel apenas para variaveis
              continuas. Adotado intervalo de predicao")
    Y <- stats::predict.lm(object = z, newdata = remove_missing_levels(z, new),
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
    geom_boxplot(aes_(fill = as.name(variable))) +
    xlab(variable) +
    theme(legend.position="none") +
    scale_y_continuous(labels = scales::label_number(accuracy = .01,
                                                     big.mark = ".",
                                                     decimal.mark = ","))
  if(!missing(at)) {
    p_local <- ifelse(missing(func), p_local, inverse(p_local, func))
    if (elasticidade == TRUE) {
      cat(elasticidade(z, variable, func, at, factor = +1), " ")
    }
    if (!missing(av)) {
      p_local <- data.frame(y = p_local, at, av = av)
    } else {
      p_local <- data.frame(y = p_local, at)
    }
    names(p_local)[1] <- response
    p <- p + geom_point(data = p_local,
                        aes_(x = as.name(variable),
                             y = as.name(response)),
                        color = "red",
                        size = 2)
  }
  if (ca == TRUE & missing(func)) {
    message("Campo de Arbítrio somente possivel na escala original.")
  }
  if (!missing(av) & missing(at)){
    message("Valor arbitrado somente pode ser plotado caso seja informado local.")
  } else if (!missing(av)) {
    p <- p + geom_point(data = p_local,
                        aes_(x = as.name(variable), y = ~av),
                        color = "purple",
                        size = 2)
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
           func,
           at,
           ca = FALSE,
           av,
           elasticidade = TRUE,
           ...){

  system <- match.arg(system)
  interval <- match.arg(interval)
  variable <- x
  z <- object
  params <- parameters(z)
  response <- params$response

  # Calling predictResponse()

  if (!missing(func)){
    if (!missing(at)){
      predResp <- predictResponse(x, object, interval = interval, level = level,
                               func = func, at = at, ...)
    } else {
      predResp <- predictResponse(x, object, interval = interval, level = level,
                                  func = func, ...)
    }
  } else if (missing(at)) {
    predResp <- predictResponse(x, object, interval = interval, level = level,
                                ...)
  } else {
    predResp <- predictResponse(x, object, interval = interval, level = level,
                                at = at, ...)
  }

  pred <- predResp$pred
  grid <- predResp$grid
  p_local <- predResp$p_local

  if (system == 'ggplot2'){

  # Basic Plot
  p <- ggplot(data = pred, aes_(x = as.name(variable), y = as.name(response))) +
    geom_line(size = 1) +
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
        geom_line(aes_string(y = "C.A.I."), linetype = 2) +
        geom_line(aes_string(y = "C.A.S."), linetype = 2)
    }
  }

  # Adds chosen intervals ribbons, if any
  if (interval == "both") {
    p <- p +
      geom_ribbon(aes_(ymin = ~lwr.y, ymax = ~upr.y,
                       colour = "grey", alpha = 0.25),
                  stat = "identity") +
      geom_ribbon(aes_(ymin = ~lwr.x, ymax = ~upr.x,
                       colour = "grey", alpha = .25),
                  stat = "identity") +
      theme(legend.position="none")
  } else if (interval != "none"){
    p <- p + geom_ribbon(aes_(ymin = ~lwr, ymax = ~upr,
                              colour = "grey", alpha = 0.5),
                         stat = "identity") +
      theme(legend.position="none")
  }

  # Adds point of 'at' argument, if not missing
  if(!missing(at)) {
    p_local <- ifelse(missing(func), p_local, inverse(p_local, func))
    if (elasticidade == TRUE) {
      cat(elasticidade(z, variable, func, at), " ")
    }
    if (!missing(av)) {
      p_local <- data.frame(y = p_local, at, av = av)
    } else {
      p_local <- data.frame(y = p_local, at)
    }
    names(p_local)[1] <- response
    p <- p + geom_point(data = p_local,
                        aes_(x = as.name(variable),
                             y = as.name(response)),
                        color = "red",
                        size = 2)
  }

  # Adds arbitrated value, if not missing
  if (!missing(av) & missing(at)){
    message("Valor arbitrado somente pode ser plotado caso seja informado local.")
  } else if (!missing(av)) {
    p <- p + geom_point(data = p_local,
                        aes_(x = as.name(variable), y = ~av),
                        color = "purple",
                        size = 2)
  }
  return(p)
  }
}
