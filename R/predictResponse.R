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
#' mod <- lm(rsqrt(VU) ~ rsqrt(area_total) + sqrt(quartos) + suites +
#'           sqrt(garagens) + rsqrt(dist_b_mar) + padrao,
#'           data = centro_2015, subset = -c(4, 31, 39))
#' p <- predictResponse("area_total", mod)
#' p
predictResponse <-
  function(x, object,
           interval =  c("none", "confidence", "prediction", "both"),
           level = 0.80,
           at,
           func,
           residuals = FALSE,
           ...){

  interval <- match.arg(interval)
  variable <- x
  params <- parameters(object)
  response <- params$response
  predictors <- params$predictors

  if(!missing(at))  grid <- createGrid(variable, object, at = at) else
    grid <- createGrid(variable, object)

  new <- grid$new
  p_local <- grid$p_local

  if(interval != "both") {
    Y <- stats::predict.lm(object = object, newdata = new, interval = interval,
                           level = level, ...)
  } else {
    Y1 <- stats::predict.lm(object = object,
                            newdata = remove_missing_levels(object, new),
                            interval = "confidence", level = level, ...)
    Y2 <- stats::predict.lm(object = object,
                            newdata = remove_missing_levels(object, new),
                            interval = "prediction", level = level, ...)
    Y1 <- as.data.frame(Y1)
    Y2 <- as.data.frame(Y2)
    Y <- dplyr::inner_join(Y1, Y2, by = "fit")
  }

  if (!missing(func)) {
    Y <- inverse(Y, func)
    Y <- cbind(Y, campo_arbitrio(Y))
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
