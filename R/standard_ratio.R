#' Standard on Ratio Studies function
#'
#' Statiscal anlysis for matching of appraisal and market data.
#' Based on International Association of Assessing Officers' Standard on Ratio Studies Document
#'
#' @param AssessedValue vector of assessed values (valores venais ou valores preditos do modelo)
#' @param SalePrice vector of sale prices (preços de mercado ou preços reais do modelo)
#' @param OutlierTrimming boolean for outliers trimming or not
#' @return COD and PRD Ratios according to IAOO
#' @export
#' @examples
#' library(appraiseR)
#'
#' zilli_2020 <- st_drop_geometry(zilli_2020)
#' zilli_2020$PC <- as.numeric(zilli_2020$PC)
#' fit <- lm(log(VU) ~ log(AP) + log(DABM) + ND + NB + NG + PSN + PC + BRO,
#' data = zilli_2020[1:190, ], subset = -c(86, 115))
#'
#' new = zilli_2020[c(191:213, 215:225), ]
#' fitted <- predict(fit, newdata = new)
#' iaao_Ratio(exp(fitted), new[, "VU", drop = TRUE])

iaao_Ratio <- function(AssessedValue, SalePrice, OutlierTrimming = FALSE) {
  df_sale <-  data.frame(AssessedValue = AssessedValue, SalePrice = SalePrice)
  df_sale["Ratio"] <- df_sale$AssessedValue / df_sale$SalePrice
  if (OutlierTrimming)
  {
    df_sale <- df_sale[order(df_sale$Ratio),]
    q1 <- quantile(df_sale$Ratio,probs = c(0.25),na.rm = TRUE)
    q3 <- quantile(df_sale$Ratio,probs = c(0.75),na.rm = TRUE)
    iqr <- q3 - q1

    df_sale.ouliers <- df_sale[df_sale$Ratio < q1 - 3*iqr | df_sale$Ratio > q3 + 3*iqr , ]

    cat("Total de dados antes do saneamento = ", length(df_sale$Ratio),  "\n" )

    cat("Total de outliers = ",length(df_sale.ouliers$Ratio),  "\n" )

    df_sale <- df_sale[df_sale$Ratio >= q1 - 3*iqr & df_sale$Ratio <= q3 + 3*iqr,]

    cat("Total de dados APÓS o saneamento = ", length(df_sale$Ratio),  "\n\n" )
  }

  MeanRatio <- mean(df_sale$Ratio)

  MedianRatio <- median(df_sale$Ratio)

  df_sale["Difference_Ratio_MedianRatio"] <- abs(df_sale$Ratio - MedianRatio)

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

  AverageOfDifferences <- mean(df_sale$Difference_Ratio_MedianRatio)

  COD <- AverageOfDifferences / MedianRatio

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

  TotalOfAssessedValues <- sum(df_sale$AssessedValue)
  TotalOfSalesPrices <- sum(df_sale$SalePrice)
  WeightedMean <- TotalOfAssessedValues / TotalOfSalesPrices
  PRD <- MeanRatio / WeightedMean
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

}

