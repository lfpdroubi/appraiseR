#'Based on a dataframe containing the estimated values, returns amplitude
#'according to NBR14.653-2
#'
#'Amplitude according to NBR14.653-2.
#'
#'@param Y dataframe with \emph{Y_hat} values with confidence/prediction
#'  \code{\link{bestfit}}
#'
#'@return a vector containing the amplitudes of the \emph{Y_hat} in \%
#'
#'@examples
#'dados <- st_drop_geometry(centro_2015)
#'fit <- lm(log(valor) ~ ., data = dados)
#'require(dplyr)
#'aval <- dados %>% filter(is.na(valor))
#'Y_hat <- predict(object = fit, interval = "confidence", newdata = aval)
#'Y_hat <- inverse(Y_hat, "log")
#'amp <- amplitude(Y_hat)
#'amp
#'@rdname parametros_NBR
#'@export
amplitude <- function(Y){
  if (is.null(dim(Y))) amp <- NULL else {
    amp <- apply(Y, 1, function (x) 100 * (max(x)-min(x))/x[1])
    amp <- round(amp,2)
  }
  return(amp)
}

#'Based on a dataframe containing the estimated values, gives precision degree
#'parameters based in NBR14.653-2
#'
#'Precision degree based on NBR14.653-2.
#'
#'@param amplitude a vector containing the amplitudes of the estimations
#'  \code{\link{bestfit}}
#'
#'@return a vector containing the precision degrees of the estimations.
#'
#'@examples
#'g_precisao(amp)
#'@rdname parametros_NBR
#'@export
g_precisao <- function(amplitude) {
  # Calcula a amplitude do intervalo
  amp <- amplitude

  # Calcula o grau de precisao baseado na amplitude do intervalo
  if (is.null(amp)) gp <- NULL else {
    gp <- sapply(amp/100,  function (x) ifelse(x > .5, "Fora de Especifica\u00E7\u00E3o",
                                               ifelse(x <= .3, "III",
                                                      ifelse(x <= .4, "II", "I"))))
  }
  return(gp)
}

#'Based on a dataframe containing the estimated values, returns interval of
#'arbitration parameters according to NBR14.653-2
#'
#'Interval of arbitration according to NBR14.653-2.
#'
#'
#'@return a vector containing the interval of arbitration according to
#'  NBR14.653-2.
#'
#'@examples
#'campo_arbitrio(Y_hat)
#'@rdname parametros_NBR
#'@export
campo_arbitrio <- function(Y){
  if(is.vector(Y)) {
    Y <- as.matrix(Y)
    dimnames(Y) <- list(NULL, "fit")
  }
  ca <- matrix(data = c(0.85*Y[,"fit"], 1.15*Y[,"fit"]), nrow = nrow(Y),
               dimnames = list(rownames(Y), c("C.A.I.", "C.A.S.")))
  return(ca)
}

#'Based on a dataframe containing the estimated values, returns interval evaluation
#'according to NBR14.653-2
#'
#'Interval evaluation based on NBR14.653-2.
#'
#'@return a vector containing interval evaluation according to NBR14.653-2.
#'
#'@examples
#'aval_intervalar(Y_hat)
#'@rdname parametros_NBR
#'@export
aval_intervalar <- function(Y){
  if (is.null(dim(Y))) avi <- NULL else {
    ca <- campo_arbitrio(Y)
    avi <- matrix(c(pmax(ca[,"C.A.I."], apply(Y, 1, min)),
                    pmin(ca[,"C.A.S."], apply(Y, 1, max))),
                  nrow = nrow(Y),
                  dimnames = list(rownames(Y), c("L.I.", "L.S.")))
  }
  return(avi)
}