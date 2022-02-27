#' @import stats
#' @import ggplot2

precision = getFromNamespace("precision", "scales")

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr %<>%
#' @export
magrittr::`%<>%`

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

#' Cube of a number
#'
#' @param x a numeric vector or array
#' @export
#' @examples
#'
#' cube(2)

cube <- power(3)

#' Returns the inverse of the function f (character)
#'
#' @param func a function of the box-cox family (rsqr(), rec(), rsqrt(), log(),
#' cubroot(), sqrt(), I() and sqr())
#' @return the inverse function, in character format.
#' @export
#' @examples
#' invFunc("log")

invFunc <- function(func){
  switch(func,
         rsqr = "rsqrt",
         rec = "rec",
         rsqrt = "rsqr",
         log = "exp",
         cubroot = "cube",
         sqrt = "sqr",
         identity = "identity",
         sqr = "sqrt"
  )
}

#' Returns a vector generated with the inverse of the function f
#'
#' @param x A vector or object of type
#' @param func a function of the box-cox family (rsqr(), rec(), rsqrt(), log(),
#' cubroot(), sqrt(), I() and sqr())
#' @return a
#' @export
#' @examples
#' inverse(rsqr(10), "rsqr")
#' inverse(rec(10), "rec")
#' inverse(rsqrt(10), "rsqrt")
#' inverse(log(1), "log")
#' inverse(sqrt(4), "sqrt")
#' inverse(sqr(4), "sqr")
#' dados <- st_drop_geometry(centro_2015)
#' fit <- lm(log(valor) ~ ., data = dados)
#' aval <- new_data(fit)
#' Y <- predict(fit, newdata = aval, interval = "confidence")
#' inverse(Y, "log")
#'

inverse <- function(x, func) {
  switch(func,
         rsqr = appraiseR::rsqrt(x),
         rec = appraiseR::rec(x),
         rsqrt = appraiseR::rsqr(x),
         log = exp(x),
         cubroot = appraiseR::cube(x),
         sqrt = appraiseR::sqr(x),
         identity = identity(x),
         sqr = sqrt(x)
  )
}

#' Return the median value or the modal value of a vector, depending on whether
#' the vector is numeric or a factor.
#'
#' @inheritParams stats::median
#' @return the median value for objects of the class integer or double. The
#'   modal value for objects of class factor.
#' @name centre
#' @export
centre <- function(x, ...) UseMethod("centre")

#' @rdname centre
#' @examples
#' vec <- c(-3, -2, 0, 1, 1, 3)
#' centre(vec)
#' @export
centre.numeric <- function(x, na.rm = TRUE, ...) {
  x <- stats::median(x, na.rm = na.rm, ...)
  x
}

#' @rdname centre
#' @examples
#' vec <- c(-3, -2, 0, 1, 1, 3)
#' vec <- as.factor(vec)
#' centre(vec)
#' dados <- st_drop_geometry(centro_2015)
#' centre(dados$padrao)
#' @export
centre.factor <- function(x, na.rm = TRUE, ...){
  x <- raster::modal(x, na.rm = na.rm)
  x
}

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
  myformula <- stats::formula(z)
  data <- eval(cl$data)
  vars <- all.vars(myformula)
  tt <- terms(myformula)

  resp <- all.vars(update(myformula, . ~ 1))
  preds <- setdiff(vars, resp)

  lhs <- myformula[[2]]
  rhs <- myformula[[3]]

  param <-
    list(parameters = c(resp, preds),
         predictors = preds,
         response = resp,
         lhs = lhs,
         rhs = rhs,
         data = data,
         call = cl)

  return(param)
}

#' @rdname parameters
#' @examples
#' dados <- st_drop_geometry(centro_2015)
#' best_fit <- bestfit(valor ~ ., dados)
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
#' dados <- st_drop_geometry(centro_2015)
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

#' Wrapper around format
#'
#' This function is only a wrapper around \link{format} function that uses standard
#' brazillian formats by default
#'
#' @param x a number to be formatted by \code{format}
#' @inheritParams base::format
#' @export
brformat <- function(x, decimal.mark = ",", big.mark = ".", digits = 2,
                     nsmall = 2, scientific = FALSE, ...) {
  format(x, decimal.mark = decimal.mark, big.mark = big.mark, digits = digits,
         nsmall = nsmall, scientific = scientific, ...)
}

#' @rdname brformat
#' @export
brf <- brformat

#' Wrapper around brformat
#'
#' This is a wrapper around \link{brformat}.
#'
#' @param x a real number
#' @param prefix currency units. Defaults for brazilian reais.
#' @param \ldots further arguments to be passed to \link{brformat}.
#' @return text in Reais.
#' @examples
#' Reais(100)
#' @export
#'
Reais <- function(x, prefix = "R$ ", ...) {
  paste(prefix, brformat(x, ...), sep = "")
}

#' @rdname Reais
#' @examples
#' library(ggplot2)
#' p <- ggplot(centro_2015@data, aes(x = area_total, y = valor)) +
#'       geom_point(na.rm = TRUE)
#' p + scale_y_continuous(labels = reais(nsmall = 0))
#' @export
reais <- function(...) {
  function(x) Reais(x, ...)
}

#' Write in percentage form
#'
#' This function transforms any number to percentage format for reporting purposes.
#' @param x a real number
#' @examples
#' porcento(0.25)
#' pct(0.25)
#' @export
#'
porcento <- function (x, ...) {
  if (length(x) == 0)
    return(character())
  x <- brf(100*x, ...)
  paste0(x, "%")
}

#' @rdname porcento
#' @export
pct <- porcento

#' Regression equation
#'
#' Givens a \link{lm} object, returns its regression equation
#' @param object object of class \code{lm}
#' @param type the equation type required: regression (reg) or estimation (est).
#' @param inline the equation mode. TRUE for inline equations or FALSE for
#' displayed mode.
#' @param func transformation applied to dependent variable.
#' @param accuracy number to round to; for POSIXct objects, a number of seconds
#' @param f rounding function: floor, ceiling or round
#' @examples
#' dados <- st_drop_geometry(centro_2015)
#' fit <- lm(log(valor) ~ ., dados)
#' equacoes(fit)
#' equacoes(fit, precision = 1)
#' @export
equacoes <- function(object, type = c("reg", "est"), inline = TRUE, func,
                     accuracy = 100, f = round, errorTerm = TRUE){
  z <- object
  myformula <- stats::formula(z)
  parametros <- parameters(z)
  type <- match.arg(type)

  if(type == "reg") {
    lhs <- format(parametros$lhs)
  } else {
    lhs <- parametros$response
  }

  coefs <- coef(z)
  coefs <- plyr::round_any(coefs, accuracy, f)

  rhs <-  paste(coefs[1], "+", paste(coefs[-1], "\\cdot",
                                     names(coefs[-1]),
                                     collapse = " + ")
                )
  rhs <- gsub("\\_", "\\\\_", rhs)
  rhs <- gsub("\\+ -", "- ", rhs)
  rhs <- gsub("[.]", ",", rhs)

  if (type == "reg"){
    Formula <- paste(lhs, "=", rhs)
  } else if (!missing(func)) {
    Formula <- paste(lhs, " = ", appraiseR::invFunc(func), "(", rhs, ")",
                     sep = "")
  } else {
    message("Estimation regression asked but no transformation passed.")
  }

  if (type == "reg" & errorTerm == TRUE){
    Formula <- paste(Formula, "+ \\varepsilon")
  }

  if (inline == TRUE) {
    cat('$', Formula, '$', sep = "")
  } else {
    cat('$$', Formula, '$$', sep = "")
  }
}
