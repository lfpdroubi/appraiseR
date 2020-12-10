#' Standard on Ratio Studies function
#'
#' Statiscal anlysis for matching of appraisal and market data.
#' Based on International Association of Assessing Officers' Standard on Ratio
#' Studies Document
#'
#' @param AssessedValue vector of assessed values (valores venais ou valores
#' preditos do modelo)
#' @param SalePrice vector of sale prices (preços de mercado ou preços reais do
#' modelo)
#' @return COD, PRD (ratios) and PRB according to IAOO
#' @export
iaao_Ratio <- function(x, ...) UseMethod("iaao_Ratio")

#' @param OutlierTrimming boolean for outliers trimming or not
#' @examples
#' library(sf)
#' zilli_2020 <- st_drop_geometry(zilli_2020)
#' zilli_2020$PC <- as.numeric(zilli_2020$PC)
#' fit <- lm(log(VU) ~ log(AP) + log(DABM) + ND + NB + NG + PSN + PC + BRO,
#' data = zilli_2020[1:190, ], subset = -c(86, 115))
#'
#' new = zilli_2020[c(191:213, 215:225), ]
#' fitted <- predict(fit, newdata = new)
#' iaao_Ratio(exp(fitted), new[, "VU", drop = TRUE])
#' @rdname iaao_Ratio
#' @export
iaao_Ratio.default <- function(AssessedValue, SalePrice,
                               OutlierTrimming = FALSE) {
  df_sale <-  data.frame(AssessedValue = AssessedValue, SalePrice = SalePrice)
  df_sale["ASR"] <- df_sale$AssessedValue / df_sale$SalePrice
  if (OutlierTrimming)
  {
    df_sale <- df_sale[order(df_sale$ASR),]
    q1 <- quantile(df_sale$ASR,probs = c(0.25),na.rm = TRUE)
    q3 <- quantile(df_sale$ASR,probs = c(0.75),na.rm = TRUE)
    iqr <- q3 - q1

    df_sale.ouliers <- df_sale[df_sale$ASR < q1 - 3*iqr | df_sale$ASR > q3 + 3*iqr , ]

    cat("Total de dados antes do saneamento = ", length(df_sale$ASR),  "\n" )

    cat("Total de outliers = ",length(df_sale.ouliers$ASR),  "\n" )

    df_sale <- df_sale[df_sale$ASR >= q1 - 3*iqr & df_sale$ASR <= q3 + 3*iqr,]

    cat("Total de dados APÓS o saneamento = ", length(df_sale$ASR),  "\n\n" )
  }

  MeanRatio <- mean(df_sale$ASR)

  MedianRatio <- median(df_sale$ASR)

  df_sale["Difference_Ratio_MedianRatio"] <- df_sale$ASR - MedianRatio

  df_sale["Pct_Diff"] <- (df_sale$ASR - MedianRatio)/MedianRatio

  AverageOfDifferences <- mean(abs(df_sale$Difference_Ratio_MedianRatio))

  COD <- AverageOfDifferences / MedianRatio

  TotalOfAssessedValues <- sum(df_sale$AssessedValue)
  TotalOfSalesPrices <- sum(df_sale$SalePrice)
  WeightedMean <- TotalOfAssessedValues / TotalOfSalesPrices
  PRD <- MeanRatio / WeightedMean

  # Calculo PRB

  Value <- .5*df_sale$SalePrice + .5*df_sale$AssessedValue/MedianRatio
  IndepVar <- log(Value)/.693
  DepVar <- df_sale$Pct_Diff

  fit <- lm(DepVar ~ IndepVar)

  PRB <- coef(fit)["IndepVar"]

  PRB_CI <- confint(fit)["IndepVar", ]

  z <- list(AssessedValue = AssessedValue,
            SalePrice = SalePrice,
            MedianRatio = MedianRatio,
            COD = COD,
            PRD = PRD,
            PRB = PRB,
            PRB_CI = PRB_CI,
            ASR = df_sale$ASR,
            PctDiff = df_sale$Pct_Diff,
            Value = Value)
  class(z) <- "iaao"
  return(z)

}
#' @param object An object of class lm.
#' @param func function used to transform the response (defaults to identity)
#' @param \dots further arguments passed to \code{iaao_Ratio}.
#' @examples
#' # Applied to centro_2015 dataset
#' dados <- st_drop_geometry(centro_2015)
#' dados$padrao <- as.numeric(dados$padrao)
#' fit <- lm(log(valor)~area_total + quartos + suites + garagens +
#' log(dist_b_mar) + I(1/padrao), data = dados, subset = -c(31, 39))
#' iaao_Ratio(object = fit, func = "log")
#' @rdname iaao_Ratio
#' @export
iaao_Ratio.lm <- function(object, func = "identity", ...){
  z <- object
  fitted <- fitted(z)
  mf <- stats::model.frame(z)
  y <- stats::model.response(mf)
  Y <- data.frame(AssessedValue = fitted, SalePrice = y)
  Y <- inverse(Y, func = func)
  s <- iaao_Ratio(AssessedValue = Y$AssessedValue,
                  SalePrice = Y$SalePrice, ...)
  return(s)
}
#'
#'@export
plot.iaao <- function(object, ...){
  z <- object
  plot(z$AssessedValue, z$ASR,
       xlab = "Valores Ajustados", ylab = "ASR", ...)
  plot(z$Value, z$ASR,
       xlab = "VALUE", ylab = "ASR", ...)
  plot(z$SalePrice, z$ASR,
       xlab = "Valores Observados", ylab = "ASR", ...)
  plot(log(z$Value)/.693, z$PctDiff,
       xlab = "LN(VALUE)/.693", ylab = "Pct Diff.", ...)
}
#' @export
#'
print.iaao <- function(x, ...){

  MedianRatio <- x$MedianRatio
  COD <- x$COD
  PRD <- x$PRD
  PRB <- x$PRB
  PRB_CI <- x$PRB_CI

  if(MedianRatio < .70)
  {
    nivelMedianRatio <- "Valor Venal baixo em relação ao valor de mercado: \n necessidade de atualização dos valores venais (mínimo deve ser 70%)."
  }
  else if(MedianRatio >= .70 & MedianRatio <= 1)
  {
    nivelMedianRatio <- "Valor Venal compatível com valor de mercado"
  }
  else
  {
    nivelMedianRatio <- "Valor Venal SUPERIOR ao valor de mercado: \n necessidade de atualização dos valores venais."
  }

  cat("Razão das medianas (Median Ratio) = ", brf(MedianRatio, nsmall = 3),
      "\nNível: ", nivelMedianRatio , "\n\n" )

  if(COD <= .10)
  {
    nivelCOD <- "Equidade de Valor Venal EXCELENTE (COD menor igual a 10%)"
  }
  else if(COD > .10 & COD <= .1499)
  {
    nivelCOD <- "Equidade de Valor Venal BOA (COD entre 11% e 14%)"
  }
  else if(COD >= .15 & COD <= .20)
  {
    nivelCOD <- "Equidade de Valor Venal JUSTA (COD entre 15% e 20%)"
  }
  else if(COD > .20 & COD < .30)
  {
    nivelCOD <- "Equidade de Valor Venal RUIM (COD entre 20% e 30%)"
  }
  else
  {
    nivelCOD <- "Equidade de Valor Venal RUIM - falta de homogeneidade nos \n valores e a necessidade de atualiza\u00E7\u00E3o (COD > 30%). "
  }

  cat("COD (Coefficient of Dispersion) = ", pct(COD), "\nNível: ", nivelCOD , "\n\n" )

  if(PRD < .98)
  {
    nivelPRD <- "Tendência PROGRESSIVA de Valor Venal \nFORA do intervalo recomendado (98% a 103%)"
  }
  else if(PRD >= .98 & PRD < 1)
  {
    nivelPRD <- "Tendência PROGRESSIVA de Valor Venal \nDENTRO do intervalo recomendado (98% a 103%)"
  }
  else if(PRD == 1)
  {
    nivelPRD <- "Sem Tendência de progressividade ou regressividade"
  }
  else if(PRD > 1 & PRD <= 1.03 )
  {
    nivelPRD <- "Tendência REGRESSIVA de Valor Venal \nDENTRO do intervalo recomendado (98% a 103%)"
  }
  else
  {
    nivelPRD <- "Tendência REGRESSIVA de Valor Venal \nFORA do intervalo recomendado (98% a 103%)"
  }

  cat("PRD (Price-Related Differential) = ", pct(PRD), "\nNível: ", nivelPRD , "\n\n" )

  cat("PRB (Price-Related Bias) = ", brf(PRB, nsmall = 3), "\nIntervalo de Confiança: ", brf(PRB_CI) , "\n\n")
}