#' Update method for bestfit
#'
#' This functions implements the \code{update} method for \link{bestfit} models.
#'
#' @param object a bestfit object.
#' @param formula. a generic formula.
#' @param \dots further arguments to be added to the original call.
#' @return a new bestfit object, update according to the arguments passed to \code{update.bestfit}.
#' @export
#' @examples
#' dados <- centro_2015@data
#' best_fit <- bestfit(valor ~ ., dados)
#' s <- summary(best_fit)
#' out <- car::outlierTest(s$fit)
#'
#' #update best_fit in order to exclude the outlier found (AP_31)
#' outliers <- match(names(out$p), rownames(centro_2015@data))
#' update(best_fit, subset = -outliers)
#'

update.bestfit <- function(object, formula., ...) {
  call <- object$call

  if (!missing(formula.)) {
    call$formula <- stats::update.formula(stats::formula(object), formula.)
  }
  pryr::modify_call(call, pryr::dots(...))
  eval(call, environment(stats::formula(object)))
}