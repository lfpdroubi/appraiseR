#' Summary method for class bestfit
#'
#' \code{summary} method for class \code{\link{bestfit}} produce result summary
#' containing the formula for the best (chosen) fit and the \code{summary.lm}
#' for that fit.
#'
#' @param object an object of class \code{\link{bestfit}}.
#' @param fit the number of the chosen fit from the combination matrix (defaults
#'   for the best fit found with \code{\link{bestfit}}).
#' @param subset a specification of the rows to be used: defaults to all rows.
#'   This can be any valid indexing vector (see \link{[.data.frame}) for the
#'   rows of data or if that is not supplied, a data frame made up of the
#'   variables used in \code{formula}.
#' @param \dots not used.
#' @return Returns the call for the \code{\link{bestfit}} function, the best
#'   (chosen) fit number, the \code{lm }formula and the \code{lm} fit summary
#'   for the best (chosen) fit transformations found by \code{\link{bestfit}}.
#'
#' @export
#' @examples
#' library(sf)
#' dados <- st_drop_geometry(centro_2015)
#' dados$padrao <- as.numeric(dados$padrao)
#' best_fit <- bestfit(valor ~ ., data = dados,
#'                     transf = c("rec", "rsqrt", "log", "sqrt"),
#'                     subset = -c(31, 39))
#' summary(best_fit)
#' summary(best_fit, fit = 514)
summary.bestfit <- function(object, fit = 1, subset, ...){
  id <- NULL #just for R CMD check

  z <- object
  a <- z$tab

  preds <- z$predictors

  formula_y <- paste0(base::subset(a, id == fit, select = z$response, drop = TRUE),
                      "(", z$response, ")")
  terms_x <- sapply(preds, function (x) paste0(base::subset(a,
                                                            id == fit,
                                                            select = x,
                                                            drop = TRUE),
                                               "(", x, ")"))
  formula_x <- paste0(terms_x, collapse = " + ")
  formula <- paste0(formula_y, " ~ ", formula_x)

  args <- list(formula, data = stats::getCall(z)$data)

  if (!missing(subset)) {
    args$subset <- subset
  } else {
    if (!is.null(z$subset)) args$subset <- z$subset
  }

  model <- do.call("lm", args)
  #g <- call("lm", formula, eval(stats::getCall(z)$data, environment(formula(z))))
  #model <- eval(g)

  g <- grau(model)

  est <- list(call = z$call,
              bestfit = base::subset(a, id == fit),
              formula = formula,
              fit = model,
              table = a,
              nmin = g$nmin,
              tmax = g$tmax,
              fstat = g$fstat)
  class(est) <- "summary.bestfit"
  est
}

#' @export
#'
mystarformat <- function(x) symnum(x, corr = FALSE, na = FALSE,
                                   cutpoints = c(0, 0.10, 0.20, 0.30, 1),
                                   symbols = c("***", "**", "*", " "))

#' @export
#'
show_coef <- function(fit) {
  mycoef<-data.frame(coef(summary(fit)), check.names=F)
  mycoef$signif = mystarformat(mycoef$`Pr(>|t|)`)
  mycoef$`Pr(>|t|)` = format.pval(mycoef$`Pr(>|t|)`)
  mycoef
}

#' @export

print.summary.bestfit <- function(x, ...){
  cat("Call:\n")
  print(x$call)
  cat("\nBest (Chosen) Transformations:\n")
  print(x$bestfit)
  cat("\nBest (Chosen) fit LM summary:\n")
  print(show_coef(x$fit))
  cat("---\n Signif. codes:  0 ‘***’ 0.10 ‘**’ 0.20 ‘*’ 0.30 ‘ ’ 1\n")
  cat("NBR-14.653-2 check:\n")
  cat("Number of market data used:\n")
  print(x$nmin)
  cat("Max significance level allowed for each predictor:\n")
  print(x$tmax)
  cat("Max significance level allowed for F-test:\n")
  print(x$fstat)
}