#' Mass grid predictions for marginal effects plotting
#'
#' \code{predictResponse} makes predictions for plotting marginal effects for
#' model terms with or without confidence/prediction intervals
#'
#' @export
#' @examples
#' library(sf)
#' data(centro_2015)
#' dados <- st_drop_geometry(centro_2015)
#' mod <- lm(sqrt(valor) ~ sqrt(area_total) + quartos + suites, dados)
#' p <- predictResponse("area_total", mod)
#' p
predictResponse <-
  function(x, object,
           interval =  c("none", "confidence", "prediction", "both"),
           level = 0.80,
           at,
           func,
           ...){

  interval <- match.arg(interval)
  variable <- x
  #z <- object
  params <- parameters(object)
  response <- params$response

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

  z <- list()
  z$grid <- new
  z$p_local <- p_local
  z$pred <- pred

  return(z)

}