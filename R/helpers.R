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
#' all.equal(rsqr(2), 1/2^2)
#' rsqr(1:10)

rsqr <- power(-2)

#' Reciprocal (1/x) of a number
#'
#' @param x a numeric vector or array
#' @export
#' @examples
#'
#' rec(2)
#' all.equal(rec(2), 1/2)
#' rec(1:10)

rec <- power(-1)

#' Reciprocal of the square root of a number
#'
#' @param x a numeric vector or array
#' @export
#' @examples
#'
#' rsqrt(4)
#' all.equal(rsqrt(2), 1/sqrt(2))
#' rsqrt(rsqr(1:10))

rsqrt <- power(-.5)

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

#' Cubic root of a number
#'
#' @param x a numeric vector or array
#' @export
#' @examples
#'
#' cubroot(8)

cubroot <- power(1/3)

#' Returns the inverse of the function f (character)
#'
#' @param FUN a function of the box-cox family (rsqr(), rec(), rsqrt(), log(),
#' cubroot(), sqrt(), I() and sqr())
#' @return the inverse function, in character format.
#' @export
#' @examples
#' invFunc("log")

invFunc <- function(FUN){
  switch(FUN,
         rsqr = "rsqrt",
         rec = "rec",
         rsqrt = "rsqr",
         log = "exp",
         exp = "log",
         sqrt = "sqr",
         sqr = "sqrt",
         cube = "cubroot",
         cubroot = "cube"
  )
}

#' Returns a vector generated with the inverse of the function f
#'
#' @param x A vector or object of type
#' @param FUN a function of the box-cox family (rsqr(), rec(), rsqrt(), log(),
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

inverse <- function(x, FUN) {
  switch(FUN,
         rsqr = appraiseR::rsqrt(x),
         rec = appraiseR::rec(x),
         rsqrt = appraiseR::rsqr(x),
         log = exp(x),
         exp = log(x),
         cubroot = appraiseR::cube(x),
         sqrt = appraiseR::sqr(x),
         identity = identity(x),
         sqr = sqrt(x),
         cube = appraiseR::cubroot(x)
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
#' x <- c(-3, -2, 0, NA, 1, 1, 3)
#' centre(x) # .5 (default is median(x))
#' centre(x, FUN = mean) # 0
#' centre(x, FUN = raster::modal) # 1
#' @export
centre.numeric <- function(x, FUN = median, na.rm = TRUE, ...) {
  FUN(x, na.rm = na.rm, ...)
}

#' @rdname centre
#' @examples
#' y <- c(-3, -2, 0, 1, 1, 3)
#' y <- as.factor(y)
#' centre(y) # the reference level
#' centre(y, FUN = raster::modal) # the most frequent (modal) value
#' library(sf)
#' data(centro_2015)
#' centre(centro_2015$padrao) # the reference level
#' centre(centro_2015$padrao, FUN = raster::modal) # the most frequent value
#' @export
centre.factor <- function(x, FUN, na.rm = TRUE, ...){
  #x <- raster::modal(x, na.rm = na.rm)

  if (missing(FUN)) {
    x <- levels(x)[1]
  } else {
    x <- FUN(x, na.rm = na.rm, ...)
    }
  return(x)
}
#' centre(centro_2015$geometry)
#' @export
centre.sfc_POINT <- function(x, FUN = mean, ...){
  z <- sf::st_coordinates(x)
  apply(z, 2, FUN, ...)
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
#' # 1. Real dataset
#'
#' library(sf)
#' data(centro_2015)
#' centro_2015 <- st_drop_geometry(centro_2015)
#' fit <- lm(log(valor) ~ ., centro_2015, subset = -c(31, 39))
#' p <- parameters(fit)
#' p$parameters
#' p$rhs
#' p$predictors
#' p$response
#' p$lhs
#' p$depvarTrans
#' p$data
#'
#' # 2. Null model
#'
#' fit1 <- lm(log(valor) ~ 1, data = centro_2015)
#' p1 <- parameters(fit1)
#' p1$parameters
#' p1$rhs
#' p1$predictors
#' p1$response
#' p1$lhs
#' p1$depvarTrans
#' p1$data
#'
#' # 3. Random generated data
#'
#' n <- 20
#' set.seed(1)
#' Frente <- rnorm(n = n, mean = 12, sd = 2.5)
#' Prof <- rnorm(n = n, mean = 25, sd = 5)
#' Area <- Frente*Prof
#' # quadratic relationship
#' PU <- 5000 - 10*Area + .005*Area^2 + 10*Frente + rnorm(n, mean = 0, sd = 150)
#' d <- data.frame(PU, Area, Frente, Prof)
#' fit2 <- lm(PU ~ poly(Area, 2) + Frente, data = d)
#' p2 <- parameters(fit2)
#' p2$parameters
#' p2$rhs
#' p2$predictors
#' p2$response
#' p2$lhs
#' p2$depvarTrans
#' p2$data
#'
#' fit3 <- lm(PU ~ poly(Area, 2, raw=TRUE) + Frente, data = d)
#' p3 <- parameters(fit3)
#' p3$parameters
#' p3$rhs
#' p3$predictors
#' p3$response
#' p3$lhs
#' p3$depvarTrans
#' p3$data
#' @export
#'
parameters.lm <- function(object, ...) {
  z <- object

  cl <- stats::getCall(z)
  myformula <- stats::formula(z)
  data <- as.data.frame(eval(z$call$data))
  vars <- all.vars(myformula)
  tt <- terms(myformula)

  resp <- all.vars(update(myformula, . ~ 1))
  preds <- setdiff(vars, resp)

  #lhs <- rownames(attr(z$terms, "factors"))[1]
  lhs <- as.character(myformula)[2]
  rhs <- as.character(myformula)[3]

  if (stringr::str_detect(lhs, paste("\\(", resp, "\\)", sep = ""))) {
    depvarTrans <- stringr::str_replace(lhs,
                                        pattern = paste("\\(", resp, "\\)",
                                                        sep = ""),
                                        replacement = "")
  } else {
    depvarTrans <- NA
  }


  param <-
    list(parameters = c(resp, preds),
         predictors = preds,
         response = resp,
         depvarTrans = depvarTrans,
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
#' @param FUN transformation applied to dependent variable.
#' @param accuracy number to round to; for POSIXct objects, a number of seconds
#' @param f rounding function: floor, ceiling or round
#' @examples
#' data(centro_2015)
#' centro_2015 <- within(centro_2015, PU <- valor/area_total)
#' fit <- lm(log(PU) ~ log(area_total) + quartos + suites + garagens +
#'            log(dist_b_mar) + padrao, centro_2015)
#' equacoes(fit)
#' equacoes(fit, accuracy = .01)
#' @export
equacoes <- function(object, type = c("reg", "est"), inline = TRUE, FUN,
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
  } else if (!missing(FUN)) {
    Formula <- paste(lhs, " = ", appraiseR::invFunc(FUN), "(", rhs, ")",
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

# copied from `insight`
# jacobian / derivate for log models and other transformations ----------------


# this function adjusts any IC for models with transformed response variables
.adjust_ic_jacobian <- function(model, ic) {
  response_transform <- insight::find_transformation(model)
  if (!is.null(ic) && !is.null(response_transform) && !identical(response_transform, "identity")) {
    adjustment <- .safe(.ll_analytic_adjustment(model, insight::get_weights(model, na_rm = TRUE)))
    if (!is.null(adjustment)) {
      ic <- ic - 2 * adjustment
    }
  }
  ic
}


# copied from `insight`
.ll_analytic_adjustment <- function(x, model_weights = NULL) {
  tryCatch(
    {
      trans <- parameters(x)$depvarTrans
      resp <- insight::get_response(x)
      switch(trans,
             identity = .weighted_sum(log(resp), w = model_weights),
             log = .weighted_sum(log(1/resp), w = model_weights),
             log1p = .weighted_sum(log(1/(resp + 1)), w = model_weights),
             log2 = .weighted_sum(log(1/(resp * log(2))), w = model_weights),
             log10 = .weighted_sum(log(1/(resp * log(10))), w = model_weights),
             exp = .weighted_sum(resp, w = model_weights),
             expm1 = .weighted_sum((resp-1), w = model_weights),
             sqrt = .weighted_sum(log(0.5/ sqrt(resp)), w = model_weights),
             .ll_jacobian_adjustment(x, model_weights)
      )
    },
    error = function(e) {
      NULL
    }
  )
}


# this function calculates the adjustment for the log-likelihood of a model
# with transformed response
.ll_jacobian_adjustment <- function(model, weights = NULL) {
  tryCatch(
    {
      trans <- parameters(model)$depvarTrans
      .weighted_sum(log(
        diag(attr(with(
          insight::get_data(model, verbose = FALSE),
          stats::numericDeriv(
            expr = quote(trans(
              get(insight::find_response(model))
            )),
            theta = insight::find_response(model)
          )
        ), "gradient"))
      ), weights)
    },
    error = function(e) {
      NULL
    }
  )
}


.weighted_sum <- function(x, w = NULL, ...) {
  if (is.null(w)) {
    mean(x) * length(x)
  } else {
    stats::weighted.mean(x, w) * length(x)
  }
}