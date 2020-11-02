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
#' @param local Data Frame to be used for calculate the estimates
#' (defaults for center of each variable).
#' @param \dots further arguments passed to predict.lm.
#' @export
#' @examples
#' library(sf)
#' data(centro_2015)
#' dados <- st_drop_geometry(centro_2015)
#' mod <- lm(log(valor) ~ ., data = dados)
#' plotVar(mod, "area_total")
#' plotVar(mod, "area_total", interval = "confidence")
#' plotVar(mod, "area_total", interval = "prediction")
#' plotVar(mod, "area_total", interval = "prediction", ca = TRUE) @lm(log(valor)
#' plotVar(mod, "area_total", interval = "both")
#' plotVar(mod, "area_total", "log")
#' plotVar(mod, "area_total", "log", ca = TRUE)
#' plotVar(mod, "area_total", "log", interval = "confidence")
#' plotVar(mod, "area_total", "log", interval = "prediction")
#' plotVar(mod, "area_total", "log", interval = "both")
#' plotVar(mod, "area_total", interval = "both", ca = TRUE,
#'         local = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'         dist_b_mar = 250, padrao = "medio"))
#' plotVar(mod, "area_total", func = "log", interval = "both", ca = TRUE,
#'         local = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'         dist_b_mar = 250, padrao = "medio"))
#' plotVar(mod, "padrao")
#' plotVar(mod, "padrao", ca = TRUE)
#' plotVar(mod, "padrao", func = "log", ca = TRUE)
#' plotVar(mod, "padrao", interval = "confidence")
#' plotVar(mod, "padrao", interval = "prediction")
#' plotVar(mod, "padrao", interval = "both")
#' plotVar(mod, "padrao", func = "log", interval = "confidence",
#' local = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'         dist_b_mar = 250, padrao = "medio"))
#' plotVar(mod, "padrao", func = "log", interval = "prediction",
#' local = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'         dist_b_mar = 250, padrao = "medio"), av = 1100000)
#' plotVar(mod, "area_total", func = "log", interval = "prediction",
#' local = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'         dist_b_mar = 250, padrao = "medio"), av = 1100000, ca = TRUE)

plotVar <- function(object, variable, func,
                    interval = c("none", "confidence", "prediction", "both"),
                    level = 0.80,
                    ca = FALSE,
                    av,
                    local, ...){
  interval <- match.arg(interval)
  z <- object
  df <- eval(stats::getCall(z)$data)
  vars <- all.vars(stats::formula(z))
  params <- parameters(z)
  response <- params$response
  preds <- params$predictors

  df %<>% dplyr::as_tibble() %>% dplyr::mutate_if(is.character, as.factor)

  variavel <- dplyr::pull(df[, variable])

  if (is.factor(variavel)){
    grid <- unique(variavel)
    if (missing(local)) {
      new <- data.frame(grid, lapply(df[setdiff(preds, variable)], centre))
      p_local <- NULL
    } else {
      new <- data.frame(grid, local[setdiff(preds, variable)])
      p_local <- predict(z, newdata = local)
    }
    names(new)[1] <- variable
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
    pred <- data.frame(grid, Y)
    colnames(pred)[1] <- variable
    pred_plot <- reshape2::melt(pred, id.var = variable, value.name = response)
    p <- ggplot(data = pred_plot, aes_(x = as.name(variable), y = as.name(response))) +
      geom_boxplot(aes_(fill = as.name(variable))) +
      theme(legend.position="bottom") +
      scale_y_continuous(labels = scales::label_number_si(accuracy = .01,
                                                          big.mark = ".",
                                                          decimal.mark = ","))
    if(!missing(local)) {
      p_local <- ifelse(missing(func), p_local, inverse(p_local, func))
      cat(elasticidade(z, variable, func, local, factor = +1), " ")
      if (!missing(av)) {
        p_local <- data.frame(y = p_local, local, av = av)
      } else {
        p_local <- data.frame(y = p_local, local)
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
    if (!missing(av) & missing(local)){
      message("Valor arbitrado somente pode ser plotado caso seja informado local.")
    } else if (!missing(av)) {
      p <- p + geom_point(data = p_local,
                          aes_(x = as.name(variable), y = ~av),
                          color = "purple",
                          size = 2)
    }
    return(p)
  } else {
    # caso variavel continua
    grid <- seq(min(df[, variable], na.rm = TRUE),
                max(df[, variable], na.rm = TRUE),
                length = 101)
    if (missing(local)) {
      new <- data.frame(grid, lapply(df[setdiff(preds, variable)], centre))
      p_local <- NULL
    } else {
      new <- data.frame(grid, local)
      p_local <- predict(z, newdata = local)
    }
    names(new)[1] <- variable
    if(interval != "both") {
      Y <- stats::predict.lm(object = z, newdata = new, interval = interval,
                           level = level, ...)
    } else {
      Y1 <- stats::predict.lm(object = z, newdata = remove_missing_levels(z, new),
                              interval = "confidence", level = level, ...)
      Y2 <- stats::predict.lm(object = z, newdata = remove_missing_levels(z, new),
                              interval = "prediction", level = level, ...)
      Y1 <- as.data.frame(Y1)
      Y2 <- as.data.frame(Y2)
      Y <- dplyr::inner_join(Y1, Y2, by = "fit")
    }
    if (!missing(func)) {
      Y <- inverse(Y, func)
      Y <- cbind(Y, campo_arbitrio(Y))
    }
    pred <- data.frame(grid, Y)
    colnames(pred)[1] <- variable
    colnames(pred)[2] <- response
    p <- ggplot(data = pred, aes_(x = as.name(variable), y = as.name(response))) +
        geom_line(size = 1) +
        theme(legend.position="bottom") +
        scale_y_continuous(labels = scales::label_number_si(accuracy = .01,
                                                            big.mark = ".",
                                                            decimal.mark = ",")) +
        scale_x_continuous(labels = scales::label_number_si(accuracy = .01,
                                                            big.mark = ".",
                                                            decimal.mark = ",")) +
      theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1))
    if (ca == TRUE) {
      if (missing(func)) {
        message("Campo de Arbítrio somente possivel na escala original.")
        return(p)
      } else {
        p <- p +
          geom_line(aes_string(y = "C.A.I."), linetype = 2) +
          geom_line(aes_string(y = "C.A.S."), linetype = 2)
      }
    }
    if(!missing(local)) {
      p_local <- ifelse(missing(func), p_local, inverse(p_local, func))
      cat(elasticidade(z, variable, func, local), " ")
      if (!missing(av)) {
        p_local <- data.frame(y = p_local, local, av = av)
      } else {
        p_local <- data.frame(y = p_local, local)
      }
      names(p_local)[1] <- response
      p <- p + geom_point(data = p_local,
                          aes_(x = as.name(variable),
                               y = as.name(response)),
                          color = "red",
                          size = 2)
    }
    if (!missing(av) & missing(local)){
      message("Valor arbitrado somente pode ser plotado caso seja informado local.")
    } else if (!missing(av)) {
      p <- p + geom_point(data = p_local,
                          aes_(x = as.name(variable), y = ~av),
                          color = "purple",
                          size = 2)
    }
    if (interval == "none") {
      return(p)
    } else if (interval != "both"){
      p <- p + geom_ribbon(aes_(ymin = ~lwr, ymax = ~upr,
                                colour = "grey", alpha = 0.5),
                           stat = "identity") +
        theme(legend.position="none")
      return(p)
    } else {
      p <- p +
        geom_ribbon(aes_(ymin = ~lwr.y, ymax = ~upr.y,
                         colour = "grey", alpha = 0.25),
                    stat = "identity") +
        geom_ribbon(aes_(ymin = ~lwr.x, ymax = ~upr.x,
                         colour = "grey", alpha = .25),
                    stat = "identity") +
        theme(legend.position="none")
      return(p)
    }
    }
}