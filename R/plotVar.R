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
#' @examples
#' plotVar(fit, "area_total", residuals = T)
#' plotVar(fit, "area_total", residuals = T, palette = "Dark2")
#' @export

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
  predictors <- params$predictors


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
#'
#' @export
plotFactor <- function(x, object,
                       interval =  c("none", "confidence", "prediction", "both"),
                       level = 0.80,
                       residuals = FALSE,
                       colour,
                       at,
                       FUN,
                       ca = FALSE,
                       av,
                       elasticidade = TRUE,
                       palette = "Paired",
                       ...){
  interval <- match.arg(interval)
  #DF <-  as.data.frame(eval(stats::getCall(object)$data))
  variable <- x
  params <- parameters(object)
  parametros <- params$parameters
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

  # pred_plot <- reshape2::melt(pred,
  #                             id.var = variable,
  #                             value.name = lhs,
  #                             variable.name = "var")
  #
  #
  # p <- ggplot(data = pred_plot, aes(x = reorder(.data[[variable]],
  #                                               .data[[lhs]],
  #                                               median),
  #                                   y = .data[[lhs]]
  #                                   )
  #             ) +
  #   geom_boxplot(aes(fill = .data[[variable]])) +
  #   scale_fill_brewer(palette = palette) +
  #   xlab(variable) +
  #   theme(legend.position="none") +
  #   scale_y_continuous(labels = scales::label_number_auto())

  p <- ggplot(pred, aes(x = reorder(.data[[variable]],
                                    .data[[lhs]],
                                    median),
                        y = .data[[lhs]],
                        colour = .data[[variable]])) +
    geom_point() +
    geom_line(aes(group = .data[[variable]])) +
    xlab(variable) +
    theme(legend.position="none") +
    scale_y_continuous(labels = scales::label_number_auto())

  if (residuals == TRUE & missing(FUN)) {

    if (length(predictors)>1) {

      if (missing(at)) {

        predFrame <- data.frame(mframe[, c(lhs, variable), drop=F],
                                lapply(mframe[, setdiff(predictors, variable),drop=F],
                                       centre), check.names = F)
      } else {

        predFrame <- data.frame(mframe[, c(lhs, variable), drop = F],
                                at[setdiff(predictors, variable), drop = F],
                                check.names = F)
      }
    } else {
      predFrame <- mframe[, c(lhs, variable), drop = F]
    }

    predFrame <- broom::augment(object, newdata = predFrame)
    #predFrame <- within(predFrame, Y <- pres[, res[[variable]]-1] + .fitted)

    p <- p +
      geom_violin(data = predFrame,
                  aes(x = .data[[variable]], y = .data$.fitted + .data$.resid,
                      fill = .data[[variable]]),
                  alpha = .5) +
      geom_point(data = predFrame,
                 position = position_jitter(seed = 1, width = 0.2),
                 aes(x = .data[[variable]], y = .data$.fitted + .data$.resid),
                 pch = 20, size = 2, color = col[8], alpha = .5)

  }

  if (interval == "confidence") {
    p <- p +
      geom_errorbar(aes(ymin = .data$lwr, ymax = .data$upr),
                    linewidth = 1.25, width = .2)
  } else if (interval == "prediction") {
    p <- p +
      geom_pointrange(aes(ymin = .data$lwr, ymax = .data$upr), linewidth = 1)
  } else if (interval == "both") {
    p <- p +
      geom_pointrange(aes(ymin = .data$lwr.pred, ymax = .data$upr.pred),
                      linewidth = 1) +
      geom_errorbar(aes(ymin = .data$lwr.conf, ymax = .data$upr.conf),
                    linewidth = 1.25, width = .2, colour = col[8])
  }

  if (ca == TRUE) {
    p <- p +
      geom_crossbar(aes(ymin = .data$C.A.I., ymax = .data$C.A.S.),
                    width = .3)
  }

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

    names(p_local)[1] <- lhs

    p <- p + geom_point(data = p_local,
                        aes(x = .data[[variable]],
                            y = .data[[lhs]]),
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

  variable <- x
  params <- parameters(object)
  response <- params$response
  lhs <- ifelse(!missing(FUN), response, params$lhs)
  predictors <- params$predictors
  depvarTrans <- params$depvarTrans
  # coeffs <- coef(object)

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
  yAt <- predResp$yAt

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

    tt <- attr(terms(object), "variables")

    sel <- lapply(tt, all.vars)[-1]

    res <- lapply(all.vars(terms(object)), \(x) which(sapply(sel, \(y) x %in% y)))
    res <- setNames(res, all.vars(terms(object)))

    if (missing(at)) {

      # k <- mean(mframe[, response, drop = T]) # problem: response scale when transformed
      # k <- predict(object, newdata = lapply(mframe[, predictors], centre))
      # k <- mean(mframe[, lhs, drop = T])
      # k <- vapply(mean(mframe[, response, drop = T]), FUN = depvarTrans,
      #             FUN.VALUE = c(k = 0))
      k <- attr(pres, "constant")
      # k <- grandMean(object)

      partialResiduals <-
        data.frame(X = mframe[, variable, drop = T],
                   Y = pres[, res[[variable]]-1] + k
                   #Y = pres[, variable, drop = T] + k
                   )
      MODELFRAME <- cbind(mframe, partialResiduals)
    } else {
      k <- attr(pres, "constant")
      partialResiduals <-
        data.frame(X = mframe[, variable, drop = T],
                   #Y = pres[, variable, drop = T] + k
                   Y = pres[, res[[variable]]-1] + k + yAt
                   )
      MODELFRAME <- cbind(mframe, partialResiduals)
    }

    if (missing(colour)){
      p <- p +
        geom_point(data = MODELFRAME,
                   aes(x = .data$X, y = .data$Y),
                   pch = 20, size = 2, color = col[8], alpha = .5) +
        stat_smooth(data = MODELFRAME, method = "loess", span = 1,
                    aes(x = .data$X, y = .data$Y),
                    colour= col[7], se = FALSE)
    } else {
      p <- p +
        geom_point(data = MODELFRAME,
                   aes(x = .data$X, y = .data$Y, colour = {{colour}}),
                   pch = 20, size = 2, alpha = .5) +
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
