#' Fitter function for bestfit
#'
#' This is the basic computing engine called by \code{\link{bestfit}}. This
#' function should not be used directly by the user.
#'
#' @param X a design matrix for a regression model
#' @param y the vector of the response variable
#' @param t The transformed data
#' @param p The combinations of transformed variables to be tested
#' @param response The name of the response variable
#' @return a vector with adjusted R2 to each fit

bestfitEst <- function(X, y, t, p, response){

  ## Montagem da matriz
  M <- cbind(y, X)
  colnames(M)[1] <- response

  ## Calculo do R2 de cada combinacao
  R2 <- vector(mode = "numeric", length = nrow(p))

  ## Loop: a cada iteracao, verifica apenas as transformacoes que mudaram da
  ## iteracao anterior
  for (i in seq(nrow(p))){
    a <- p[i,][p[i-1,] != p[i,]]
    n <- paste(a, names(a), sep = ".")
    for (j in seq_along(a))
      M[,names(a)[j]] <- t[,n[j]]
    fit <- RcppEigen::fastLmPure(M[,-1], M[,1])
    R2[i] <- miscTools::rSquared(M[,response], fit$residuals)
  }

  ## Calculo de R2 ajustado e formatacao dos dados para impressao em tela
  n <- nrow(M)
  gl <- fit$df.residual
  R2 <- 1-((n-1)/gl)*(1-R2)
  list(adj.R2 = R2)
}

#' Best fit models
#'
#' Find best transformations of the parameters for Linear Regression.
#'
#' @param formula A standard linear regression formula, with no transformation
#'   in the parameters.
#' @param data A data frame containing the variables in the model.
#' @param subset a specification of the rows to be used: defaults to all rows.
#'   This can be any valid indexing vector (see \link{[.data.frame}) for the
#'   rows of data or if that is not supplied, a data frame made up of the
#'   variables used in \code{formula}.
#' @param transf A family of functions to be used to transform the variables in
#'   the data frame, in order to find the best combination of transformation to
#'   be applied to the data - usually functions of the box-cox family.
#' @examples
#' library(sf)
#' data(centro_2015)
#' dados <- st_drop_geometry(centro_2015)
#' best_fit <- bestfit(valor ~ ., data = dados)
#' print(best_fit, n = 20)
#' s <- summary(best_fit)
#'
#' #There still may be outliers:
#' out <- car::outlierTest(s$fit) #31
#' outliers <- 31
#'
#' # There are two ways to handle with them:
#'
#' # Recalling bestfit with a subset argument ...
#' best_fit2 <- bestfit(valor ~ ., data = dados, subset = -outliers)
#'
#' # Or assigning a subset argument directly into summary.bestfit
#'  s <- summary(best_fit, fit = 1, subset = -outliers)
#'
#' # The latter takes less computational effort, since it only updates the
#' # lm call of the chosen fit. The former is more precise, since it runs
#' # bestfit again without the outliers.
#'
#' @rdname bestfit
#' @export
#'
bestfit <- function(formula, data, subset,
                            transf = c('rsqrt', 'log', 'sqrt')){
  df <- as.data.frame(data)
  mf <- stats::model.frame(formula = formula, data = df)
  preds <- attr(stats::terms.formula(formula, data = df), "term.labels")
  response <- colnames(mf)[attr(stats::terms.formula(formula, data = df),
                                "response")]
  parameters <- c(response, preds)

  for (i in parameters) if (is.character(df[,i])) df[,i] <- as.factor(df[,i])

  ## Montagem da matriz X e do vetor y para o ajuste do modelo
  cl <- match.call()
  cl1 <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset"), names(cl1), 0L)
  mf <- cl1[c(1L, m)]
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  X <- stats::model.matrix(attr(mf, "terms"), data = mf)
  y <- stats::model.response(mf)

  t <- alltransf(data = df, subset = subset,
                 select = parameters, transf = transf)
  p <- allperm(data = df, subset = subset,
               select = parameters, transf = transf)

  newdata <-  df[which(is.na(df[, response])), ]

  #z <- bestfit.default(X, y, t, p, response)

  X <- as.matrix(X)
  y <- as.numeric(y)
  t <- as.data.frame(t)
  p <- as.matrix(p)
  response <- as.character(response)

  z <- bestfitEst(X, y, t, p, response)

  class(z) <- "bestfit"

  ## Join combinations with adj.R2 vector in a single data frame
  z$tab <- data.frame(id = seq(nrow(p)), p)
  z$tab$adj_R2 <- z$adj.R2
  z$tab <- z$tab[order(-z$tab[,"adj_R2"]),]
  z$tab[,"id"] <- seq(nrow(z$tab))

  z$call <- cl
  z$subset <- if (missing(subset)) NULL else subset
  z$combinations <- p
  z$response <- response
  z$predictors <- preds
  if (nrow(newdata) == 0) z$newdata <- NULL else z$newdata <- newdata
  z
}