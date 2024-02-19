#' Create grid for predictions
#'
#' \code{createGrid} create grids to predictions for ploting model variables
#' (terms) with or without confidence/prediction intervals
#' @param object object of class lm
#' @param variable variable to be plotted against response variable
#' @param func function used to transform the response (optional)
#' @param interval the type of interval calculation (provided to predict.lm) to
#'   be ploted.
#' @param level Tolerance/confidence level (provided to predict.lm) to
#'   be ploted.
#' @param ca (T/F) should the limits of the invertal of arbitration be plotted?
#' @param av (T/F) should the arbitrated value be plotted?
#' @param at List to be used to calculate the predictions
#' (defaults for center of each variable).
#' @param \dots further arguments passed to predict.lm.
#' @export
#' @examples
#' library(sf)
#' data(centro_2015)
#' dados <- st_drop_geometry(centro_2015)
#' mod <- lm(sqrt(valor) ~ sqrt(area_total) + quartos + suites, dados)
#' p <- createGrid("area_total", mod)
#' p <- createGrid("area_total", mod,
#'                 at = list(area_total = 205, quartos = 3, suites = 1))

createGrid <- function(x, object, at, ...){
  variable <- x
  params <- parameters(object)
  response <- params$response
  preds <- params$predictors

  df <- as.data.frame(eval(stats::getCall(object)$data))
  variavel <- df[, variable, drop = FALSE]

  grid <- seq(min(df[, variable], na.rm = TRUE),
              max(df[, variable], na.rm = TRUE),
              length = 101)

  if (missing(at)) {
    new <- data.frame(grid, lapply(df[setdiff(preds, variable)], centre))
    p_local <- NULL
  } else {
    new <- data.frame(grid, at)
    p_local <- predict(object, newdata = at)
  }

  names(new)[1] <- variable

  z <- list()
  z$new <- new
  z$p_local <- p_local

  return(z)
}
