#' Mass grid predictions for marginal effects plotting
#'
#' \code{predictResponse} makes predictions for plotting marginal effects for
#' model terms with or without confidence/prediction intervals
#'
#' @export
#' @examples
#' library(sf)
#' data(centro_2015)
#' centro_2015 <- within(centro_2015, VU <- valor/area_total)
#' fit<- lm(log(VU) ~ log(area_total) + quartos + suites +
#'           garagens + log(dist_b_mar) + padrao,
#'           data = centro_2015, subset = -c(31, 39))
#' p <- predictResponse("area_total", fit)
#' p
predictResponse <-
  function(x, object,
           interval =  c("none", "confidence", "prediction", "both"),
           level = 0.80,
           at,
           FUN,
           residuals = FALSE,
           ...){

  interval <- match.arg(interval)
  variable <- x
  params <- parameters(object)
  response <- params$response
  depvarTrans <- params$depvarTrans
  predictors <- params$predictors

  if(!missing(at))  grid <- createGrid(variable, object, at = at) else
    grid <- createGrid(variable, object)

  new <- grid$new
  p_local <- grid$p_local

  if(interval != "both") {
    y <- stats::predict.lm(object = object, newdata = new, interval = interval,
                           level = level, ...)
  } else {
    yc <- stats::predict.lm(object = object,
                            newdata = remove_missing_levels(object, new),
                            interval = "confidence", level = level, ...)
    yp <- stats::predict.lm(object = object,
                            newdata = remove_missing_levels(object, new),
                            interval = "prediction", level = level, ...)
    yc <- as.data.frame(yc)
    yp <- as.data.frame(yp)
    y <- dplyr::inner_join(yc, yp, by = "fit", suffix = c(".conf", ".pred"))
  }

  if (!missing(FUN)) {
    Y <- inverse(y, FUN)
    Y <- cbind(Y, campo_arbitrio(Y))
  } else if (is.na(depvarTrans) | depvarTrans == "identity"){
    Y <- cbind(y, campo_arbitrio(y))
  } else {
    Y <- inverse(y, FUN = depvarTrans)
    CA <- campo_arbitrio(Y)
    Y <- cbind(Y, CA)
    Y <- inverse(Y, FUN = invFunc(depvarTrans))
  }

  pred <- data.frame(new[, variable], Y)
  colnames(pred)[1] <- variable
  colnames(pred)[2] <- response

  mframe <-
    expand.model.frame(model = object,
                       extras = as.formula(
                         paste("~", paste(predictors, collapse = "+"))),
                       na.expand = TRUE)

  if (residuals == TRUE) mframe <- broom::augment(object, newdata = mframe)

  # beta <- coef(fit)[which(stringr::str_detect(names(coef(fit)),
  #                                             pattern = variable))]

  z <- list()
  z$grid <- new
  z$p_local <- p_local
  z$pred <- pred
  z$mframe <- mframe

  if (residuals == TRUE) {
    z$pres <- as.data.frame(residuals(object, "partial"))
  }

  return(z)

  }
