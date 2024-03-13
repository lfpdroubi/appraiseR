#' Plot model predictors against the response variable with intervals
#'
#' \code{plotvar} plots model variables with confidence/prediction intervals
#' @param object object of class lm
#' @param variable variable to be plotted against response variable
#' @param FUN function used to transform the response (optional)
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
#' plotVar(fit, "area_total", residuals = TRUE, colour = padrao)
#' plotVar(fit, "area_total", interval = "confidence", residuals = TRUE)
#' plotVar(fit, "area_total", interval = "prediction", residuals = TRUE)
#' plotVar(fit, "area_total", interval = "both", residuals = TRUE)
#' plotVar(fit, "area_total", interval = "both", ca = TRUE, residuals = TRUE)
#' plotVar(fit, "area_total", FUN = "log")
#' plotVar(fit, "area_total", FUN = "log",
#'         interval = "both", ca = TRUE, residuals = TRUE)
#' plotVar(fit, "suites")
#' plotVar(fit, "suites", interval = "confidence")
#' plotVar(fit, "suites", interval = "confidence", FUN = "log")
#'
#' # Plot model passing through point 'at'
#'
#' plotVar(fit, "area_total", interval = "both", ca = TRUE,
#'         at = list(area_total = 205, quartos = 3, suites = 1,
#'                   garagens = 2, dist_b_mar = 250, padrao = "medio"))
#'
#' # Same above + Point valuation at R$ 5.650,00 /m2
#'
#' plotVar(fit, "area_total", interval = "both",
#'         at = data.frame(area_total = 205, quartos = 3, suites = 1,
#'                         garagens = 2, dist_b_mar = 250, padrao = "medio"),
#'         ca = TRUE, av = log(5650))
#'
#' # Same above, in the original scale
#'
#' plotVar(fit, "area_total", interval = "both", FUN = 'log',
#'         at = data.frame(area_total = 205, quartos = 3, suites = 1,
#'                         garagens = 2, dist_b_mar = 250, padrao = "medio"),
#'         ca = TRUE, av = 5650)
#'
#' # Plotting factors
#'
#' plotVar(fit, "padrao")
#' plotVar(fit, "padrao", ca = TRUE)
#' plotVar(fit, "padrao", FUN = "log", ca = TRUE)
#' plotVar(fit, "padrao", interval = "confidence")
#' plotVar(fit, "padrao", interval = "prediction")
#' plotVar(fit, "padrao", interval = "both")
#' plotVar(fit, "padrao", FUN = "log", interval = "confidence",
#'         at = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'                   dist_b_mar = 250, padrao = "medio"))
#' plotVar(fit, "padrao", FUN = "log", interval = "prediction",
#'         at = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'                   dist_b_mar = 250, padrao = "medio"),
#'         av = 5650)
#' # Remove outliers
#' fit2 <- update(fit, .~.-suites, subset = -c(31,39, 45))
#' plotVar(fit2, "padrao", FUN = "log", interval = "prediction",
#'         at = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'                   dist_b_mar = 250, padrao = "medio"),
#'         av = 5650)
#' plotVar(fit2, "area_total", FUN = "log", interval = "both",
#'         at = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'                   dist_b_mar = 250, padrao = "medio"),
#'         ca = TRUE, av = 5650)
#' plotVar(fit2, "dist_b_mar", FUN = "log", interval = "both",
#'         at = list(area_total = 205, quartos = 3, suites = 1, garagens = 2,
#'                   dist_b_mar = 250, padrao = "medio"),
#'         ca = TRUE, av = 5650)

plotVar <- function(object, variable, FUN,
                    interval = c("none", "confidence", "prediction", "both"),
                    level = 0.80,
                    ca = FALSE,
                    av,
                    at,
                    ...){
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
               FUN = FUN, ca = ca, av = av, at = at,
               ...)
  } else {
    plotContinuousVariable(variable, object,
                           interval = interval, level = level,
                           FUN = FUN, ca = ca, av = av, at = at,
                           ...)
  }
}
#' export
plotFactor <- function(x, object,
                       interval =  c("none", "confidence", "prediction", "both"),
                       level = 0.80,
                       at,
                       FUN,
                       ca = FALSE,
                       av,
                       elasticidade = TRUE,
                       palette = "Paired",
                       ...){
  interval <- match.arg(interval)
  variable <- x
  params <- parameters(object)
  response <- params$response
  lhs <- ifelse(!missing(FUN), response, params$lhs)
  preds <- params$predictors
  coeffs <- coef(object)

  # Creates col vector for chosen palette
  palNum <- which(rownames(RColorBrewer::brewer.pal.info) == palette)
  maxcolors <- RColorBrewer::brewer.pal.info[palNum, "maxcolors"]
  col <- RColorBrewer::brewer.pal(n = maxcolors, name = palette)

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
  if (!missing(FUN)) {
    Y <- inverse(Y, FUN)
    Y <- cbind(Y, campo_arbitrio(Y))
  }

  pred <- data.frame(new[ , variable, drop = FALSE], Y)
  pred_plot <- reshape2::melt(pred,
                              id.var = variable,
                              value.name = lhs,
                              variable.name = "var")


  p <- ggplot(data = pred_plot, aes(x = reorder(.data[[variable]],
                                                .data[[lhs]],
                                                median),
                                    y = .data[[lhs]]
                                    )
              ) +
    geom_boxplot(aes(fill = .data[[variable]])) +
    xlab(variable) +
    theme(legend.position="none") +
    scale_y_continuous(labels = scales::label_number_auto())

  # Adds point of 'at' argument, if not missing
  if(!missing(at)) {
    p_local <- ifelse(missing(FUN), p_local, inverse(p_local, FUN))
    if (elasticidade == TRUE) {
      cat(elasticidade(object, variable, FUN, at, factor = +1), " ")
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
                        color = col[6],
                        size = 3)
  }

  if (!missing(av) & missing(at)){
    message("Valor arbitrado somente pode ser plotado caso seja informado local.")
  } else if (!missing(av)) {
    p <- p + geom_point(data = p_local,
                        aes(x = .data[[variable]], y = .data$av),
                        color = col[10],
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
           colour,
           FUN,
           at,
           ca = FALSE,
           av,
           elasticidade = FALSE,
           palette = "Paired",
           ...){

  system <- match.arg(system)
  interval <- match.arg(interval)
  DF <-  as.data.frame(eval(stats::getCall(object)$data))
  variable <- x
  params <- parameters(object)
  response <- params$response
  lhs <- ifelse(!missing(FUN), response, params$lhs)
  predictors <- params$predictors
  depvarTrans <- params$depvarTrans
  coeffs <- coef(object)

  # Creates col vector for chosen palette
  palNum <- which(rownames(RColorBrewer::brewer.pal.info) == palette)
  maxcolors <- RColorBrewer::brewer.pal.info[palNum, "maxcolors"]
  col <- RColorBrewer::brewer.pal(n = maxcolors, name = palette)

  # Calling predictResponse()

  if (!missing(FUN)){
    if (!missing(at)){
      predResp <- predictResponse(variable, object,
                                  interval = interval, level = level,
                                  FUN = FUN, at = at,
                                  residuals = residuals, ...)
    } else {
      predResp <- predictResponse(variable, object,
                                  interval = interval, level = level,
                                  FUN = FUN, residuals = residuals, ...)
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
              aes(x = .data[[variable]], y = .data[[lhs]])
              ) +
    #geom_line(lwd = 2, lty = 1, color = col[2]) +
    theme(legend.position="bottom") +
    scale_y_continuous(labels = scales::label_number_auto()) +
    scale_x_continuous(labels = scales::label_number_auto()) +
    theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1))

  # Adds chosen intervals ribbons, if any
  if (interval == "both") {
    p <- p +
      geom_ribbon(aes(ymin = .data$lwr.pred, ymax = .data$upr.pred),
                  colour = "grey",
                  fill = "grey",
                  alpha = 0.25,
                  stat = "identity") +
      geom_ribbon(aes(ymin = .data$lwr.conf, ymax = .data$upr.conf),
                  colour = "grey",
                  fill = "grey",
                  alpha = .75,
                  stat = "identity") +
      theme(legend.position="none")
  } else if (interval != "none"){
    p <- p + geom_ribbon(aes(ymin = .data$lwr, ymax = .data$upr),
                         colour = "grey",
                         fill = "grey",
                         alpha = 0.25,
                         stat = "identity") +
      theme(legend.position="none")
  }

  p <- p + geom_line(lwd = 1.5, lty = 1, color = col[2])

  # Adds CA, if TRUE
  if (ca == TRUE) {
    p <- p +
      geom_line(aes(y = .data$C.A.I.), lty = 5, lwd = 1) +
      geom_line(aes(y = .data$C.A.S.), lty = 5, lwd = 1)
  }

  # Adds partial residuals, if residuals = TRUE

  if (residuals == TRUE & missing(FUN)) {
    vars <- attr(terms(object), "variables")

    sel <- lapply(vars, all.vars)[-1]

    res <- lapply(all.vars(terms(object)), \(x) which(sapply(sel, \(y) x %in% y)))
    res <- setNames(res, all.vars(terms(object)))

    new <- lapply(DF[, predictors, drop = FALSE], centre)

    if (missing(at)) {
      k <- predict(object = object,
                   newdata = new)

      partialResiduals <-
        data.frame(X = mframe[, variable, drop = T],
                   Y = pres[, res[[variable]]-1] + k
        )
      MODELFRAME <- cbind(mframe, partialResiduals)
    } else {
        new_k <- new
        new_k[setdiff(names(new_k), variable)] <-
          at[setdiff(names(new_k), variable)]

      k <- predict(object = object,
                   newdata = new_k)

      partialResiduals <-
        data.frame(X = mframe[, variable, drop = T],
                   Y = pres[, res[[variable]]-1] + k
                   )
      MODELFRAME <- cbind(mframe, partialResiduals)
    }

    if (missing(colour)){
      p <- p +
        geom_point(data = MODELFRAME,
                   aes(x = .data$X, y = .data$Y),
                   pch = 19, color = col[8], alpha = .5) +
        geom_smooth(data = MODELFRAME,
                    aes(x = .data$X, y = .data$Y),
                    method = "loess", colour= col[7], se = FALSE)
    } else {
      p <- p +
        geom_point(data = MODELFRAME,
                   aes(x = .data$X, y = .data$Y, colour = {{colour}}),
                   pch = 19, alpha = .5) +
        theme(legend.position = "bottom",
              legend.box="horizontal",
              legend.title=element_text(size=10)) +
        guides(colour = guide_legend(title.position="top",
                                     title.hjust = 0.5))
    }
  }

  # Adds point of 'at' argument, if not missing
  if (!missing(at)) {
    p_local <- ifelse(missing(FUN), p_local, inverse(p_local, FUN))
    if (!missing(av)) {
      p_local <- data.frame(y = p_local, at, av = av)
    } else {
      p_local <- data.frame(y = p_local, at)
    }

    names(p_local)[1] <- lhs

    p <- p + geom_point(data = p_local,
                        aes(x = .data[[variable]],
                            y = .data[[lhs]]),
                        #color = "red",
                        color = col[6],
                        size = 3) +
      labs(caption = paste(variable, "=", at[variable], ";",
                           response, "=",
                           ifelse(missing(FUN) & !is.na(depvarTrans),
                                  brf(inverse(p_local[lhs], depvarTrans)),
                                  brf(p_local[lhs])
                                  )
                           )
           )
  }

  if (elasticidade == TRUE & !missing(at)) {
    cat(elasticidade(object, variable, FUN, at), " ")
  }

  # Adds arbitrated value, if not missing
  if (!missing(av) & missing(at)){
    message("Valor arbitrado somente pode ser plotado caso seja informado local.")
  } else if (!missing(av)) {
    p <- p + geom_point(data = p_local,
                        aes(x = .data[[variable]], y = .data$av),
                        color = col[10],
                        size = 3) +
      labs(caption = paste(variable, "=", at[variable], ";",
                           response, "=",
                           ifelse(missing(FUN) & !is.na(depvarTrans),
                                  brf(inverse(p_local[lhs], depvarTrans)),
                                  brf(p_local[lhs])
                                  ),
                                  "\n",
                           "Arbitrado =",
                           ifelse(missing(FUN) & !is.na(depvarTrans),
                                  brf(inverse(av, depvarTrans)),
                                  brf(av)
                           )
                           )
           )
  }
  return(p)
  }
}
