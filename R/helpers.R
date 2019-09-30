#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' Parent Function: Power of a number.
#'
#' This is an internal function to generate another ones which will be
#' effectivelly exported. This function is not exported in NAMESPACE, therefore
#' it is not available for the end-user.
#'
#' @param exponent The exponent.
#' @return A parent function that allows the user to create a closure that
#'   returns the n-th power of its argument.
#' @examples
#'
#' \dontrun{
#' power <- function(exponent) {
#' function(x) {
#'   x ^ exponent
#' }
#' }
#'
#' square <- power(2)
#' square_root <- power(.5)
#'
#' square(2) #4
#' square_root(4) #2
#' }

power <- function(exponent) {
  function(x) {
    x^exponent
  }
}

#' Reciprocal of the square of a number
#'
#' @param x a numeric vector or array
#' @export
#' @examples
#'
#' rsqr(2)
#' rsqr(1:10)

rsqr <- power(-2)

#' Reciprocal (1/x) of a number
#'
#' @param x a numeric vector or array
#' @export
#' @examples
#'
#' rec(2)
#' rec(1:10)

rec <- power(-1)

#' Reciprocal of the square root of a number
#'
#' @param x a numeric vector or array
#' @export
#' @examples
#'
#' rsqrt(4)
#' rsqrt(1:10)

rsqrt <- power(-0.5)

#' Square of a number
#'
#' @param x a numeric vector or array
#' @export
#' @examples
#'
#' sqr(2)
#' sqr(1:10)

sqr <- power(2)

#' Extract object parameters
#'
#' Returns the parameters used to build a model.
#'
#' @param object A model object.
#' @param \dots not used.
#' @return the parameters, predictors and response names besides the
#' original data used to build the model.
#' @name parameters
#' @export
parameters <- function(object, ...) {
  UseMethod("parameters")
}

#' Returns a vector generated with the inverse of the function f
#'
#' @param x A vector or object of type
#' @param func a function of the box-cox family (rsqr(), rec(), rsqrt(), log(),
#'   sqrt(), I() and sqr())
#' @return a
#' @export
#' @examples
#' dados <- centro_2015@data
#' fit <- lm(log(valor) ~ ., data = dados)
#' aval <- new_data(fit)
#' Y <- predict(fit, newdata = aval, interval = "confidence")
#' inverse(Y, "log")

inverse <- function(x, func) {
  switch(func,
         rsqr = appraiseR::rsqrt(x),
         rec = appraiseR::rec(x),
         rsqrt = appraiseR::rsqr(x),
         log = exp(x),
         sqrt = appraiseR::sqr(x),
         identity = identity(x),
         sqr = sqrt(x)
  )
}

#' @rdname parameters
#' @examples
#' dados <- centro_2015@data
#' fit <- lm(log(valor) ~ ., dados)
#' p <- parameters(fit)
#' p$parameters
#' p$predictors
#' p$response
#' p$data
#' @export
#'
parameters.lm <- function(object, ...) {
  z <- object
  cl <- stats::getCall(z)
  data <- eval(cl$data)
  vars <- all.vars(stats::formula(z))
  termsLabels <- attr(stats::terms.formula(stats::formula(z),
                                           data = data
  ),
  "term.labels"
  )
  x <- sapply(vars, grepl, termsLabels)
  preds <- vars[apply(x, 2, any)]
  resp <- vars[!apply(x, 2, any)]

  param <-
    list(parameters = c(resp, preds),
         predictors = preds,
         response = resp,
         data = data,
         call = cl)

  return(param)
}

#' @rdname parameters
#' @examples
#' best_fit <- bestfit(valor ~ ., centro_2015@data)
#' parameters(best_fit)
#' @export
#'
parameters.bestfit <- function(object, ...) {
  z <- object
  cl <- z$call
  data <- eval(cl$data, environment(stats::formula(z)))

  resp <- z$response
  preds <- z$predictors

  param <-
    list(parameters = c(resp, preds),
         predictors = preds,
         response = resp,
         data = data,
         call = cl)

  return(param)
}

#' Builds \code{newdata} argument to be used in \link{predict.lm}
#'
#' Builds a new \code{data.frame} containing only elements
#' to be appraised from the current \code{data.frame}
#'
#' @param object object of class \code{lm}
#'
#' @examples
#' dados <- centro_2015@data
#' fit <- lm(log(valor) ~ ., data = dados)
#' new_data(fit)
#' @export

new_data <- function(object) {
  z <- object
  params <- parameters(z)
  response <- params$response
  response <- as.name(response)
  parameters <- params$parameters
  data <- params$data
  aval <-
    data %>%
    dplyr::filter(is.na(!!response)) %>%
    dplyr::select(parameters)
  aval
}