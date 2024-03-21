#' Mass grid predictions for marginal effects plotting
#'
#' \code{predictResponse} makes predictions for plotting marginal effects for
#' model terms with or without confidence/prediction intervals
#'
#' @export
#' @examples
#'
#' 1. Random generated data
#'
#' 1.1 Just one factor variable
#'
#' n <- 30
#' Bairro_A <- rnorm(n, mean = 500, sd = 100)
#' Bairro_B <- rnorm(n, mean = 750, sd = 100)
#' Bairro_C <- rnorm(n, mean = 1000, sd = 100)
#'
#' dados <- data.frame(VU = c(Bairro_A, Bairro_B, Bairro_C),
#'                     Bairro = c(rep("A", n), rep("B", n), rep("C", n))
#' )
#'
#' fit <- lm(VU ~ Bairro, data = dados)
#'
#' predictResponse("Bairro", fit)
#' predictResponse("Bairro", fit, residuals = T)
#' predictResponse("Bairro", fit, at = list(Bairro = "A"))
#'
#' 1.2 Just one continuous variable
#'
#' sample_cov <- matrix(c(1000^2, -40000,
#'                        -40000,  50^2),
#'                      ncol = 2, byrow = T)
#' n <- 50
#' set.seed(4)
#' dados <- mvrnorm(n = n,
#'                 mu = c(5000, 360),
#'                 Sigma = sample_cov,
#'                 empirical = T)
#' colnames(dados) <- c("PU", "Area")
#' dados <- as.data.frame(dados)
#'
#' fit <- lm(PU ~ Area, data = dados)
#'
#' predictResponse("Area", fit, residuals = T)
#' predictResponse("Area", fit, residuals = T, at = list(Area = 600))
#'
#' 1.3 One continuous variable and one factor
#'
#' n = 25
#' set.seed(1)
#' Area <- rnorm(n = 3*n, mean = 360, sd = 50)
#'
#' Bairro <- factor(sample(LETTERS[1:3], size = 3*n, replace = T))
#'
#' dados <- data.frame(Area, Bairro)
#'
#' dados <- within(dados, {
#'   PU <- ifelse(Bairro == "A", 7500,
#'                ifelse(Bairro == "B", 10000, 12500))
#'   PU <- PU - 15*Area + rnorm(n = 3*n, mean = 0, sd = 1000)
#' })
#'
#' fit <- lm(PU ~ Area + Bairro, data = dados)
#'
#' predictResponse("Area", fit)
#'
#' 1.4 Polynomial regression
#'
#' n <- 50
#' set.seed(4)
#' Area <- rnorm(n = n, mean = 360, sd = 75)
#' VU <- 8000 + 50*Area -.1*Area^2 + rnorm(n = n, mean = 0, sd = 1000)
#'
#' dados <- data.frame(cbind(VU, Area))
#'
#' fit <- lm(VU ~ poly(Area, 2), data = dados)
#'
#' p <- predictResponse("Area", fit)
#'
#' fit2 <- lm(VU ~ poly(Area, 2, raw = TRUE), data = dados)
#'
#' p <- predictResponse("Area", fit2)
#'
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
  parametros <- params$parameters
  response <- params$response
  lhs <- params$lhs
  depvarTrans <- params$depvarTrans
  predictors <- params$predictors

  if(!missing(at))  grid <- createGrid(variable, object, at = at) else
    grid <- createGrid(variable, object)

  #new <- cbind(1, grid$new)
  #names(new)[1] <- "(Intercept)"
  new <- grid$new
  p_local <- grid$p_local
  mframe <- grid$mframe

  tt <- attr(terms(object), "variables")
  sel <- lapply(tt, all.vars)[-1]
  res <- lapply(all.vars(terms(object)), \(x) which(sapply(sel, \(y) x %in% y)))
  res <- setNames(res, all.vars(terms(object)))

  # y <- as.matrix(new) %*% coef(object)

  if(interval == "both") {
    yc <- stats::predict.lm(object = object, type = "terms",
                            newdata = remove_missing_levels(object, new),
                            interval = "confidence", level = level, ...)
    yp <- stats::predict.lm(object = object, type = "terms",
                            newdata = remove_missing_levels(object, new),
                            interval = "prediction", level = level, ...)

    k <- attr(yc[["fit"]], "constant")
    if (!missing(at)) {
      yAt <- unique(rowSums(yc[["fit"]][, unlist(res[setdiff(predictors, variable)])-1,
                                 drop = F]))
    } else {
      yAt <- 0
    }

      ycFit <- rowSums(yc[["fit"]][, res[[variable]]-1, drop = F]) + k + yAt
      ycLwr <- rowSums(yc[["lwr"]][, res[[variable]]-1, drop = F]) + k + yAt
      ycUpr <- rowSums(yc[["upr"]][, res[[variable]]-1, drop = F]) + k + yAt

      ypFit <- rowSums(yp[["fit"]][, res[[variable]]-1, drop = F]) + k + yAt
      ypLwr <- rowSums(yp[["lwr"]][, res[[variable]]-1, drop = F]) + k + yAt
      ypUpr <- rowSums(yp[["upr"]][, res[[variable]]-1, drop = F]) + k + yAt

      yc <- data.frame(fit = ycFit, lwr = ycLwr, upr = ycUpr)
      yp <- data.frame(fit = ypFit, lwr = ypLwr, upr = ypUpr)

      y <- dplyr::inner_join(yc, yp, by = "fit",
                             suffix = c(".conf", ".pred"))

  } else if (interval %in% c("confidence", "prediction")) {
    y <- stats::predict.lm(object = object, newdata = new, type = "terms",
                           interval = interval, level = level, ...)

    k <- attr(y[["fit"]], "constant")
    if (!missing(at)) {
      yAt <- unique(rowSums(y[["fit"]][, unlist(res[setdiff(predictors, variable)])-1,
                                 drop = F]))
    } else {
      yAt <- 0
    }
    yFit <- rowSums(y[["fit"]][, res[[variable]]-1, drop = F]) + k + yAt
    yLwr <- rowSums(y[["lwr"]][, res[[variable]]-1, drop = F]) + k + yAt
    yUpr <- rowSums(y[["upr"]][, res[[variable]]-1, drop = F]) + k + yAt
    y <- data.frame(fit = yFit, lwr = yLwr, upr = yUpr)
  } else { # i.e., if interval = "none"
    y <- stats::predict.lm(object = object, newdata = new, type = "terms")
    k <- attr(y, "constant")
    if (!missing(at)) {
      yAt <- unique(rowSums(y[, unlist(res[setdiff(predictors, variable)])-1,
                              drop = F]))
    } else {
      yAt <- 0
    }
    y <- rowSums(y[, res[[variable]]-1, drop = F]) + k + yAt
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
  colnames(pred)[2] <- ifelse(!missing(FUN), response, lhs)

  z <- list()
  z$grid <- new
  z$p_local <- p_local
  z$pred <- pred

  if (!missing(at)) z$yAt <- yAt

  if (residuals == TRUE) {
    mframe <- broom::augment(object, newdata = mframe)
    z$mframe <- mframe
    if (length(predictors) > 1){
      z$pres <-residuals(object, "partial")
    } else {
      if(is.factor(mframe[, variable]) | is.character(mframe[, variable])){
        z$pres <- residuals(object)
      } else {
        z$pres <- residuals(object, "partial")
        colnames(z$pres) = variable
      }
    }
  }

  return(z)

  }
